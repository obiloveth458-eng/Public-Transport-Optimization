;; Eco Commute Rewarder Smart Contract
;; This contract rewards passengers for using public transport with token incentives

;; Constants for error handling and reward parameters
(define-constant ERR-UNAUTHORIZED (err u200))
(define-constant ERR-USER-NOT-FOUND (err u201))
(define-constant ERR-INSUFFICIENT-BALANCE (err u202))
(define-constant ERR-INVALID-REWARD-AMOUNT (err u203))
(define-constant ERR-REWARD-ALREADY-CLAIMED (err u204))
(define-constant ERR-EXPIRED-REWARD (err u205))
(define-constant ERR-INVALID-ECO-SCORE (err u206))
(define-constant ERR-MINIMUM-TRIPS-NOT-MET (err u207))
(define-constant ERR-TRANSFER-FAILED (err u208))
(define-constant ERR-INVALID-MULTIPLIER (err u209))

;; Reward system constants
(define-constant BASE-REWARD-PER-TRIP u100) ;; Base tokens per trip
(define-constant PEAK-HOUR-BONUS u50) ;; Extra tokens for off-peak usage
(define-constant WEEKEND-BONUS u25) ;; Extra tokens for weekend usage
(define-constant CARBON-OFFSET-RATE u10) ;; Tokens per kg CO2 saved
(define-constant LOYALTY-MULTIPLIER u150) ;; 150% for loyal users
(define-constant ECO-CHAMPION-MULTIPLIER u200) ;; 200% for eco champions
(define-constant MIN-TRIPS-FOR-BONUS u10) ;; Minimum trips for bonus rewards
(define-constant TOKEN-DECIMALS u6) ;; Token precision
(define-constant REWARD-EXPIRY-BLOCKS u144000) ;; ~100 days

;; System management variables
(define-data-var contract-owner principal tx-sender)
(define-data-var total-tokens-distributed uint u0)
(define-data-var total-participants uint u0)
(define-data-var reward-pool-balance uint u1000000000) ;; Initial pool of 1B tokens
(define-data-var base-reward-rate uint BASE-REWARD-PER-TRIP)
(define-data-var carbon-price uint u50) ;; Price per kg CO2 in tokens
(define-data-var system-active bool true)

;; User profiles and eco-score tracking
(define-map user-profiles
  { user-id: principal }
  {
    registration-block: uint,
    total-trips: uint,
    total-tokens-earned: uint,
    total-carbon-saved: uint, ;; in grams
    eco-score: uint, ;; 0-1000 scale
    streak-days: uint,
    last-trip-block: uint,
    tier-level: (string-ascii 16), ;; "bronze", "silver", "gold", "platinum"
    referral-count: uint,
    achievements: (list 20 (string-ascii 32))
  }
)

;; Trip rewards and carbon impact tracking
(define-map trip-rewards
  { trip-id: uint }
  {
    user-id: principal,
    trip-block: uint,
    base-reward: uint,
    bonus-rewards: uint,
    carbon-saved: uint, ;; in grams
    reward-multiplier: uint, ;; percentage
    total-reward: uint,
    claimed: bool,
    expiry-block: uint,
    eco-impact-score: uint
  }
)

;; Token balance and transaction history
(define-map token-balances
  { user-id: principal }
  {
    available-balance: uint,
    pending-rewards: uint,
    total-earned: uint,
    total-redeemed: uint,
    last-updated: uint
  }
)

;; Eco achievements and milestones
(define-map achievements
  { achievement-id: (string-ascii 32) }
  {
    achievement-name: (string-ascii 64),
    description: (string-utf8 256),
    requirement-type: (string-ascii 32), ;; "trips", "carbon", "streak", "referrals"
    requirement-value: uint,
    reward-amount: uint,
    badge-level: (string-ascii 16), ;; "bronze", "silver", "gold", "legendary"
    active: bool
  }
)

;; Leaderboard and competition tracking
(define-map leaderboard-entries
  { period: uint, rank: uint } ;; period is week/month number
  {
    user-id: principal,
    total-score: uint,
    trips-count: uint,
    carbon-saved: uint,
    bonus-earned: uint
  }
)

;; Referral system tracking
(define-map referral-system
  { referrer: principal, referee: principal }
  {
    referral-block: uint,
    referrer-bonus: uint,
    referee-bonus: uint,
    both-claimed: bool,
    referee-qualified: bool ;; requires minimum trips
  }
)

;; Carbon offset marketplace
(define-map carbon-offsets
  { offset-id: uint }
  {
    user-id: principal,
    carbon-amount: uint, ;; in grams
    offset-price: uint, ;; in tokens
    creation-block: uint,
    project-type: (string-ascii 32), ;; "reforestation", "renewable", "efficiency"
    verified: bool,
    retired: bool
  }
)

;; User registration and onboarding
(define-public (register-user (user-id principal))
  (begin
    ;; Validate user is registering themselves
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    
    ;; Create user profile with initial values
    (map-set user-profiles
      { user-id: user-id }
      {
        registration-block: stacks-block-height,
        total-trips: u0,
        total-tokens-earned: u0,
        total-carbon-saved: u0,
        eco-score: u100, ;; Starting eco-score
        streak-days: u0,
        last-trip-block: stacks-block-height,
        tier-level: "bronze",
        referral-count: u0,
        achievements: (list)
      }
    )
    
    ;; Initialize token balance
    (map-set token-balances
      { user-id: user-id }
      {
        available-balance: u0,
        pending-rewards: u0,
        total-earned: u0,
        total-redeemed: u0,
        last-updated: stacks-block-height
      }
    )
    
    ;; Update total participants
    (var-set total-participants (+ (var-get total-participants) u1))
    
    (ok true)
  )
)

;; Record eco-friendly trip and calculate rewards
(define-public (record-eco-trip
    (trip-id uint)
    (user-id principal)
    (distance uint) ;; in meters
    (trip-type (string-ascii 16)) ;; "bus", "metro", "tram", "bike-share"
    (is-peak-avoidance bool)
    (is-weekend bool)
  )
  (let (
    (user-profile (unwrap! (map-get? user-profiles { user-id: user-id }) ERR-USER-NOT-FOUND))
    (carbon-saved (calculate-carbon-savings distance trip-type))
    (base-reward (var-get base-reward-rate))
  )
    ;; Only system or user can record their trips
    (asserts! (or (is-eq tx-sender (var-get contract-owner)) (is-eq tx-sender user-id)) ERR-UNAUTHORIZED)
    (asserts! (var-get system-active) ERR-UNAUTHORIZED)
    
    ;; Calculate reward components
    (let (
      (bonus-rewards (calculate-bonus-rewards base-reward is-peak-avoidance is-weekend))
      (eco-multiplier (calculate-eco-multiplier (get eco-score user-profile)))
      (total-base (+ base-reward bonus-rewards))
      (total-reward (/ (* total-base eco-multiplier) u100))
    )
      ;; Record trip reward
      (map-set trip-rewards
        { trip-id: trip-id }
        {
          user-id: user-id,
          trip-block: stacks-block-height,
          base-reward: base-reward,
          bonus-rewards: bonus-rewards,
          carbon-saved: carbon-saved,
          reward-multiplier: eco-multiplier,
          total-reward: total-reward,
          claimed: false,
          expiry-block: (+ stacks-block-height REWARD-EXPIRY-BLOCKS),
          eco-impact-score: (calculate-eco-impact-score carbon-saved distance)
        }
      )
      
      ;; Update user profile
      (update-user-eco-profile user-id carbon-saved total-reward)
      
      ;; Add to pending rewards
      (update-pending-rewards user-id total-reward)
      
      ;; Check and award achievements
      (check-and-award-achievements user-id)
      
      (ok total-reward)
    )
  )
)

;; Claim accumulated rewards
(define-public (claim-rewards (user-id principal) (trip-ids (list 50 uint)))
  (let (
    (user-balance (unwrap! (map-get? token-balances { user-id: user-id }) ERR-USER-NOT-FOUND))
  )
    ;; Only user can claim their own rewards
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    
    ;; Calculate total claimable rewards
    (let (
      (claimable-amount (calculate-claimable-rewards user-id trip-ids))
    )
      ;; Validate sufficient pool balance
      (asserts! (<= claimable-amount (var-get reward-pool-balance)) ERR-INSUFFICIENT-BALANCE)
      
      ;; Transfer rewards to user balance
      (map-set token-balances
        { user-id: user-id }
        {
          available-balance: (+ (get available-balance user-balance) claimable-amount),
          pending-rewards: (if (>= (get pending-rewards user-balance) claimable-amount)
                            (- (get pending-rewards user-balance) claimable-amount)
                            u0),
          total-earned: (+ (get total-earned user-balance) claimable-amount),
          total-redeemed: (get total-redeemed user-balance),
          last-updated: stacks-block-height
        }
      )
      
      ;; Mark trip rewards as claimed
      (mark-rewards-as-claimed trip-ids)
      
      ;; Update system totals
      (var-set reward-pool-balance (- (var-get reward-pool-balance) claimable-amount))
      (var-set total-tokens-distributed (+ (var-get total-tokens-distributed) claimable-amount))
      
      (ok claimable-amount)
    )
  )
)

;; Create referral link and track referrals
(define-public (create-referral (referrer principal) (referee principal))
  (let (
    (referrer-profile (unwrap! (map-get? user-profiles { user-id: referrer }) ERR-USER-NOT-FOUND))
  )
    ;; Only referrer can create referral
    (asserts! (is-eq tx-sender referrer) ERR-UNAUTHORIZED)
    
    ;; Create referral record
    (map-set referral-system
      { referrer: referrer, referee: referee }
      {
        referral-block: stacks-block-height,
        referrer-bonus: u500, ;; 500 tokens for referrer
        referee-bonus: u200, ;; 200 tokens for referee
        both-claimed: false,
        referee-qualified: false
      }
    )
    
    ;; Update referrer's referral count
    (map-set user-profiles
      { user-id: referrer }
      (merge referrer-profile {
        referral-count: (+ (get referral-count referrer-profile) u1)
      })
    )
    
    (ok true)
  )
)

;; Purchase carbon offsets with tokens
(define-public (purchase-carbon-offset
    (user-id principal)
    (carbon-amount uint)
    (project-type (string-ascii 32))
  )
  (let (
    (user-balance (unwrap! (map-get? token-balances { user-id: user-id }) ERR-USER-NOT-FOUND))
    (offset-cost (* carbon-amount (var-get carbon-price)))
    (offset-id (+ (var-get total-participants) stacks-block-height)) ;; Simple ID generation
  )
    ;; Only user can purchase for themselves
    (asserts! (is-eq tx-sender user-id) ERR-UNAUTHORIZED)
    
    ;; Check sufficient balance
    (asserts! (>= (get available-balance user-balance) offset-cost) ERR-INSUFFICIENT-BALANCE)
    
    ;; Create carbon offset record
    (map-set carbon-offsets
      { offset-id: offset-id }
      {
        user-id: user-id,
        carbon-amount: carbon-amount,
        offset-price: offset-cost,
        creation-block: stacks-block-height,
        project-type: project-type,
        verified: false,
        retired: false
      }
    )
    
    ;; Deduct tokens from user balance
    (map-set token-balances
      { user-id: user-id }
      {
        available-balance: (- (get available-balance user-balance) offset-cost),
        pending-rewards: (get pending-rewards user-balance),
        total-earned: (get total-earned user-balance),
        total-redeemed: (+ (get total-redeemed user-balance) offset-cost),
        last-updated: stacks-block-height
      }
    )
    
    (ok offset-id)
  )
)

;; Read-only functions for data access
(define-read-only (get-user-profile (user-id principal))
  (map-get? user-profiles { user-id: user-id })
)

(define-read-only (get-trip-reward (trip-id uint))
  (map-get? trip-rewards { trip-id: trip-id })
)

(define-read-only (get-token-balance (user-id principal))
  (map-get? token-balances { user-id: user-id })
)

(define-read-only (get-achievement (achievement-id (string-ascii 32)))
  (map-get? achievements { achievement-id: achievement-id })
)

(define-read-only (get-leaderboard-entry (period uint) (rank uint))
  (map-get? leaderboard-entries { period: period, rank: rank })
)

(define-read-only (get-referral-info (referrer principal) (referee principal))
  (map-get? referral-system { referrer: referrer, referee: referee })
)

(define-read-only (get-carbon-offset (offset-id uint))
  (map-get? carbon-offsets { offset-id: offset-id })
)

(define-read-only (get-system-stats)
  {
    total-tokens-distributed: (var-get total-tokens-distributed),
    total-participants: (var-get total-participants),
    reward-pool-balance: (var-get reward-pool-balance),
    system-active: (var-get system-active),
    base-reward-rate: (var-get base-reward-rate)
  }
)

;; Private helper functions
(define-private (calculate-carbon-savings (distance uint) (trip-type (string-ascii 16)))
  ;; Simplified calculation: average car emits ~120g CO2 per km
  (let (
    (distance-km (/ distance u1000))
    (car-emissions (* distance-km u120))
    (public-transport-emissions (/ car-emissions u4)) ;; 25% of car emissions
  )
    (if (> car-emissions public-transport-emissions)
      (- car-emissions public-transport-emissions)
      u0
    )
  )
)

(define-private (calculate-bonus-rewards (base-reward uint) (peak-avoidance bool) (weekend bool))
  (let (
    (peak-bonus (if peak-avoidance PEAK-HOUR-BONUS u0))
    (weekend-bonus (if weekend WEEKEND-BONUS u0))
  )
    (+ peak-bonus weekend-bonus)
  )
)

(define-private (calculate-eco-multiplier (eco-score uint))
  (if (>= eco-score u800)
    ECO-CHAMPION-MULTIPLIER ;; Eco champion
    (if (>= eco-score u600)
      LOYALTY-MULTIPLIER ;; Loyal user
      (if (>= eco-score u400)
        u125 ;; Good user
        u100 ;; Default multiplier
      )
    )
  )
)

(define-private (calculate-eco-impact-score (carbon-saved uint) (distance uint))
  ;; Score based on carbon efficiency per km
  (if (> distance u0)
    (/ (* carbon-saved u100) (/ distance u1000))
    u0
  )
)

(define-private (update-user-eco-profile (user-id principal) (carbon-saved uint) (reward-amount uint))
  (let (
    (profile (unwrap-panic (map-get? user-profiles { user-id: user-id })))
    (new-total-trips (+ (get total-trips profile) u1))
    (new-carbon-saved (+ (get total-carbon-saved profile) carbon-saved))
    (new-tokens-earned (+ (get total-tokens-earned profile) reward-amount))
  )
    (map-set user-profiles
      { user-id: user-id }
      (merge profile {
        total-trips: new-total-trips,
        total-tokens-earned: new-tokens-earned,
        total-carbon-saved: new-carbon-saved,
        eco-score: (calculate-new-eco-score new-total-trips new-carbon-saved),
        last-trip-block: stacks-block-height,
        tier-level: (calculate-tier-level new-total-trips new-carbon-saved)
      })
    )
  )
)

(define-private (update-pending-rewards (user-id principal) (reward-amount uint))
  (let (
    (balance (unwrap-panic (map-get? token-balances { user-id: user-id })))
  )
    (map-set token-balances
      { user-id: user-id }
      (merge balance {
        pending-rewards: (+ (get pending-rewards balance) reward-amount)
      })
    )
  )
)

(define-private (calculate-claimable-rewards (user-id principal) (trip-ids (list 50 uint)))
  ;; Simplified - would iterate through trip-ids and sum claimable amounts
  u1000 ;; Placeholder return value
)

(define-private (mark-rewards-as-claimed (trip-ids (list 50 uint)))
  ;; Would iterate through and mark each trip reward as claimed
  true
)

(define-private (check-and-award-achievements (user-id principal))
  ;; Would check user progress against achievements and award appropriately
  true
)

(define-private (calculate-new-eco-score (trips uint) (carbon-saved uint))
  ;; Score based on trips frequency and carbon impact
  (let (
    (trip-score (if (> trips u100) u500 (* trips u5)))
    (carbon-score (if (> carbon-saved u50000) u500 (/ carbon-saved u100)))
  )
    (if (> (+ trip-score carbon-score) u1000) u1000 (+ trip-score carbon-score))
  )
)

(define-private (calculate-tier-level (trips uint) (carbon-saved uint))
  (if (and (>= trips u100) (>= carbon-saved u100000))
    "platinum"
    (if (and (>= trips u50) (>= carbon-saved u50000))
      "gold"
      (if (and (>= trips u25) (>= carbon-saved u25000))
        "silver"
        "bronze"
      )
    )
  )
)
