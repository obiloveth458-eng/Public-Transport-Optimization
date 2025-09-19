;; Passenger Behavior Analyzer Smart Contract
;; This contract analyzes passenger patterns to optimize routes and schedules for public transportation

;; Constants for error handling and system parameters
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-PASSENGER-NOT-FOUND (err u101))
(define-constant ERR-ROUTE-NOT-FOUND (err u102))
(define-constant ERR-INVALID-TIME-SLOT (err u103))
(define-constant ERR-INVALID-CAPACITY (err u104))
(define-constant ERR-SCHEDULE-CONFLICT (err u105))
(define-constant ERR-INVALID-COORDINATES (err u106))
(define-constant ERR-INSUFFICIENT-DATA (err u107))
(define-constant ERR-ANALYSIS-FAILED (err u108))
(define-constant ERR-OPTIMIZATION-ERROR (err u109))

;; System configuration constants
(define-constant MAX-CAPACITY u200) ;; Maximum passenger capacity per vehicle
(define-constant TIME-SLOTS-PER-DAY u24) ;; 24 hour time slots
(define-constant PEAK-HOUR-START u7) ;; 7 AM
(define-constant PEAK-HOUR-END u9) ;; 9 AM
(define-constant EVENING-PEAK-START u17) ;; 5 PM
(define-constant EVENING-PEAK-END u19) ;; 7 PM
(define-constant MIN-ANALYSIS-SAMPLES u10) ;; Minimum data points for analysis
(define-constant OPTIMIZATION-THRESHOLD u75) ;; Percentage threshold for optimization

;; Data variables for system management
(define-data-var contract-owner principal tx-sender)
(define-data-var total-passengers uint u0)
(define-data-var total-routes uint u0)
(define-data-var analysis-period uint u7) ;; Days for analysis window
(define-data-var optimization-frequency uint u24) ;; Hours between optimizations
(define-data-var last-optimization-block uint u0)

;; Helper function for capacity calculation
(define-private (calculate-recommended-capacity (demand uint))
  (if (> demand u60) (+ demand u20) demand) ;; add buffer for high demand
)

;; Data mappings for passenger analytics
(define-map passenger-profiles
  { passenger-id: principal }
  {
    registration-block: uint,
    total-trips: uint,
    preferred-routes: (list 10 uint),
    average-trip-duration: uint, ;; in minutes
    peak-hour-usage: uint, ;; percentage
    off-peak-usage: uint, ;; percentage
    weekend-usage: uint, ;; percentage
    loyalty-score: uint, ;; 0-100
    behavior-pattern: (string-ascii 32), ;; "regular", "occasional", "peak-only", "flexible"
    last-trip-block: uint
  }
)

;; Route information and performance metrics
(define-map route-data
  { route-id: uint }
  {
    route-name: (string-ascii 64),
    start-location: { lat: int, lng: int },
    end-location: { lat: int, lng: int },
    intermediate-stops: (list 20 { lat: int, lng: int }),
    average-capacity: uint,
    peak-capacity: uint,
    off-peak-capacity: uint,
    average-duration: uint, ;; in minutes
    efficiency-score: uint, ;; 0-100
    passenger-satisfaction: uint, ;; 0-100
    total-trips-served: uint,
    last-updated: uint
  }
)

;; Time-based passenger flow analytics
(define-map passenger-flow
  { route-id: uint, time-slot: uint, day-type: (string-ascii 16) } ;; "weekday", "weekend", "holiday"
  {
    passenger-count: uint,
    boarding-count: uint,
    alighting-count: uint,
    peak-load: uint,
    average-wait-time: uint, ;; in minutes
    occupancy-rate: uint, ;; percentage
    delay-incidents: uint,
    service-reliability: uint, ;; percentage
    demand-forecast: uint
  }
)

;; Trip history and patterns
(define-map trip-records
  { trip-id: uint }
  {
    passenger-id: principal,
    route-id: uint,
    boarding-stop: { lat: int, lng: int },
    alighting-stop: { lat: int, lng: int },
    trip-start-time: uint, ;; block height
    trip-duration: uint, ;; in minutes
    day-of-week: uint, ;; 1-7
    time-slot: uint, ;; 0-23
    passenger-load: uint,
    delay-time: uint, ;; in minutes
    satisfaction-rating: uint ;; 1-5
  }
)

;; Route optimization recommendations
(define-map optimization-recommendations
  { route-id: uint, analysis-date: uint }
  {
    current-efficiency: uint,
    recommended-frequency: uint, ;; trips per hour
    suggested-capacity: uint,
    peak-hour-adjustments: (string-utf8 256),
    off-peak-adjustments: (string-utf8 256),
    new-stop-recommendations: (list 5 { lat: int, lng: int }),
    schedule-modifications: (string-utf8 512),
    expected-improvement: uint, ;; percentage
    implementation-priority: (string-ascii 16) ;; "high", "medium", "low"
  }
)

;; Demand forecasting data
(define-map demand-forecasts
  { route-id: uint, forecast-date: uint, time-slot: uint }
  {
    predicted-demand: uint,
    confidence-level: uint, ;; percentage
    historical-average: uint,
    trend-direction: (string-ascii 16), ;; "increasing", "decreasing", "stable"
    seasonal-factor: uint, ;; percentage adjustment
    weather-impact: uint, ;; percentage adjustment
    event-impact: uint, ;; percentage adjustment for special events
    recommended-capacity: uint
  }
)

;; Passenger registration and profile management
(define-public (register-passenger
    (passenger-id principal)
    (initial-preferences (list 10 uint))
  )
  (begin
    ;; Validate input
    (asserts! (is-eq tx-sender passenger-id) ERR-UNAUTHORIZED)
    
    ;; Create passenger profile
    (map-set passenger-profiles
      { passenger-id: passenger-id }
      {
        registration-block: stacks-block-height,
        total-trips: u0,
        preferred-routes: initial-preferences,
        average-trip-duration: u30, ;; default 30 minutes
        peak-hour-usage: u0,
        off-peak-usage: u0,
        weekend-usage: u0,
        loyalty-score: u50, ;; start with neutral score
        behavior-pattern: "new",
        last-trip-block: stacks-block-height
      }
    )
    
    ;; Update total passenger count
    (var-set total-passengers (+ (var-get total-passengers) u1))
    
    (ok true)
  )
)

;; Register a new route with initial configuration
(define-public (register-route
    (route-id uint)
    (route-name (string-ascii 64))
    (start-lat int)
    (start-lng int)
    (end-lat int)
    (end-lng int)
    (stops (list 20 { lat: int, lng: int }))
  )
  (begin
    ;; Only contract owner can register routes
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    
    ;; Validate coordinates
    (asserts! (and (> start-lat -9000000) (< start-lat 9000000)) ERR-INVALID-COORDINATES)
    (asserts! (and (> start-lng -18000000) (< start-lng 18000000)) ERR-INVALID-COORDINATES)
    
    ;; Create route record
    (map-set route-data
      { route-id: route-id }
      {
        route-name: route-name,
        start-location: { lat: start-lat, lng: start-lng },
        end-location: { lat: end-lat, lng: end-lng },
        intermediate-stops: stops,
        average-capacity: u50, ;; initial estimate
        peak-capacity: u80,
        off-peak-capacity: u30,
        average-duration: u45, ;; initial estimate 45 minutes
        efficiency-score: u70, ;; initial score
        passenger-satisfaction: u75, ;; initial satisfaction
        total-trips-served: u0,
        last-updated: stacks-block-height
      }
    )
    
    ;; Update total routes count
    (var-set total-routes (+ (var-get total-routes) u1))
    
    (ok route-id)
  )
)

;; Record a passenger trip for analysis
(define-public (record-trip
    (trip-id uint)
    (passenger-id principal)
    (route-id uint)
    (boarding-lat int)
    (boarding-lng int)
    (alighting-lat int)
    (alighting-lng int)
    (trip-duration uint)
    (passenger-load uint)
    (delay-time uint)
    (satisfaction-rating uint)
  )
  (let (
    (current-time (mod (/ stacks-block-height u60) u24)) ;; current hour
    (day-of-week (mod (/ stacks-block-height u1440) u7)) ;; approximate day of week
    (passenger-profile (unwrap! (map-get? passenger-profiles { passenger-id: passenger-id }) ERR-PASSENGER-NOT-FOUND))
    (route-info (unwrap! (map-get? route-data { route-id: route-id }) ERR-ROUTE-NOT-FOUND))
  )
    ;; Validate inputs
    (asserts! (<= satisfaction-rating u5) ERR-INVALID-CAPACITY)
    (asserts! (< passenger-load MAX-CAPACITY) ERR-INVALID-CAPACITY)
    
    ;; Record trip data
    (map-set trip-records
      { trip-id: trip-id }
      {
        passenger-id: passenger-id,
        route-id: route-id,
        boarding-stop: { lat: boarding-lat, lng: boarding-lng },
        alighting-stop: { lat: alighting-lat, lng: alighting-lng },
        trip-start-time: stacks-block-height,
        trip-duration: trip-duration,
        day-of-week: day-of-week,
        time-slot: current-time,
        passenger-load: passenger-load,
        delay-time: delay-time,
        satisfaction-rating: satisfaction-rating
      }
    )
    
    ;; Update passenger profile
    (update-passenger-behavior passenger-id current-time trip-duration satisfaction-rating)
    
    ;; Update route analytics
    (update-route-analytics route-id passenger-load delay-time satisfaction-rating)
    
    (ok trip-id)
  )
)

;; Analyze passenger patterns for route optimization
(define-public (analyze-passenger-patterns (route-id uint))
  (let (
    (route-info (unwrap! (map-get? route-data { route-id: route-id }) ERR-ROUTE-NOT-FOUND))
    (analysis-date (/ stacks-block-height u1440)) ;; current day
  )
    ;; Only contract owner can trigger analysis
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR-UNAUTHORIZED)
    
    ;; Check if sufficient data exists
    (asserts! (>= (get total-trips-served route-info) MIN-ANALYSIS-SAMPLES) ERR-INSUFFICIENT-DATA)
    
    ;; Perform analysis calculations
    (let (
      (current-efficiency (get efficiency-score route-info))
      (peak-demand (calculate-peak-demand route-id))
      (off-peak-demand (calculate-off-peak-demand route-id))
      (optimization-potential (calculate-optimization-potential route-id))
    )
      ;; Generate optimization recommendations
      (map-set optimization-recommendations
        { route-id: route-id, analysis-date: analysis-date }
        {
          current-efficiency: current-efficiency,
          recommended-frequency: (calculate-optimal-frequency peak-demand),
          suggested-capacity: (calculate-optimal-capacity peak-demand off-peak-demand),
          peak-hour-adjustments: u"Increase frequency by 20% during peak hours",
          off-peak-adjustments: u"Reduce frequency by 15% during off-peak hours",
          new-stop-recommendations: (list),
          schedule-modifications: u"Adjust departure times based on demand patterns",
          expected-improvement: optimization-potential,
          implementation-priority: (if (> optimization-potential u20) "high" "medium")
        }
      )
      
      ;; Update last optimization timestamp
      (var-set last-optimization-block stacks-block-height)
      
      (ok optimization-potential)
    )
  )
)

;; Generate demand forecast for route planning
(define-public (generate-demand-forecast
    (route-id uint)
    (forecast-days uint)
    (time-slot uint)
  )
  (let (
    (route-info (unwrap! (map-get? route-data { route-id: route-id }) ERR-ROUTE-NOT-FOUND))
    (forecast-date (+ (/ stacks-block-height u1440) forecast-days))
  )
    ;; Validate time slot
    (asserts! (< time-slot TIME-SLOTS-PER-DAY) ERR-INVALID-TIME-SLOT)
    
    ;; Calculate forecast based on historical patterns
    (let (
      (historical-average (calculate-historical-average route-id time-slot))
      (trend-factor (calculate-trend-factor route-id))
      (seasonal-adjustment (calculate-seasonal-adjustment forecast-date))
      (predicted-demand (apply-forecast-adjustments historical-average trend-factor seasonal-adjustment))
    )
      ;; Store forecast data
      (map-set demand-forecasts
        { route-id: route-id, forecast-date: forecast-date, time-slot: time-slot }
        {
          predicted-demand: predicted-demand,
          confidence-level: u85, ;; default confidence
          historical-average: historical-average,
          trend-direction: "stable",
          seasonal-factor: seasonal-adjustment,
          weather-impact: u100, ;; neutral impact
          event-impact: u100, ;; neutral impact
          recommended-capacity: (calculate-recommended-capacity predicted-demand)
        }
      )
      
      (ok predicted-demand)
    )
  )
)

;; Read-only functions for data retrieval
(define-read-only (get-passenger-profile (passenger-id principal))
  (map-get? passenger-profiles { passenger-id: passenger-id })
)

(define-read-only (get-route-data (route-id uint))
  (map-get? route-data { route-id: route-id })
)

(define-read-only (get-passenger-flow (route-id uint) (time-slot uint) (day-type (string-ascii 16)))
  (map-get? passenger-flow { route-id: route-id, time-slot: time-slot, day-type: day-type })
)

(define-read-only (get-trip-record (trip-id uint))
  (map-get? trip-records { trip-id: trip-id })
)

(define-read-only (get-optimization-recommendations (route-id uint) (analysis-date uint))
  (map-get? optimization-recommendations { route-id: route-id, analysis-date: analysis-date })
)

(define-read-only (get-demand-forecast (route-id uint) (forecast-date uint) (time-slot uint))
  (map-get? demand-forecasts { route-id: route-id, forecast-date: forecast-date, time-slot: time-slot })
)

(define-read-only (get-system-stats)
  {
    total-passengers: (var-get total-passengers),
    total-routes: (var-get total-routes),
    analysis-period: (var-get analysis-period),
    last-optimization: (var-get last-optimization-block)
  }
)

;; Private helper functions
(define-private (update-passenger-behavior
    (passenger-id principal)
    (time-slot uint)
    (trip-duration uint)
    (satisfaction uint)
  )
  (let (
    (profile (unwrap-panic (map-get? passenger-profiles { passenger-id: passenger-id })))
    (is-peak-hour (or (and (>= time-slot PEAK-HOUR-START) (<= time-slot PEAK-HOUR-END))
                      (and (>= time-slot EVENING-PEAK-START) (<= time-slot EVENING-PEAK-END))))
  )
    (map-set passenger-profiles
      { passenger-id: passenger-id }
      (merge profile {
        total-trips: (+ (get total-trips profile) u1),
        average-trip-duration: (/ (+ (* (get average-trip-duration profile) (get total-trips profile)) trip-duration)
                                 (+ (get total-trips profile) u1)),
        peak-hour-usage: (if is-peak-hour
                          (+ (get peak-hour-usage profile) u1)
                          (get peak-hour-usage profile)),
        off-peak-usage: (if (not is-peak-hour)
                         (+ (get off-peak-usage profile) u1)
                         (get off-peak-usage profile)),
        loyalty-score: (calculate-loyalty-score (get total-trips profile) satisfaction),
        last-trip-block: stacks-block-height
      })
    )
  )
)

(define-private (update-route-analytics
    (route-id uint)
    (passenger-load uint)
    (delay-time uint)
    (satisfaction uint)
  )
  (let (
    (route (unwrap-panic (map-get? route-data { route-id: route-id })))
  )
    (map-set route-data
      { route-id: route-id }
      (merge route {
        total-trips-served: (+ (get total-trips-served route) u1),
        average-capacity: (/ (+ (* (get average-capacity route) (get total-trips-served route)) passenger-load)
                           (+ (get total-trips-served route) u1)),
        passenger-satisfaction: (/ (+ (* (get passenger-satisfaction route) (get total-trips-served route)) (* satisfaction u20))
                                 (+ (get total-trips-served route) u1)),
        efficiency-score: (calculate-efficiency-score passenger-load delay-time),
        last-updated: stacks-block-height
      })
    )
  )
)

(define-private (calculate-peak-demand (route-id uint))
  ;; Simplified calculation - in reality would analyze historical data
  u80
)

(define-private (calculate-off-peak-demand (route-id uint))
  ;; Simplified calculation - in reality would analyze historical data
  u40
)

(define-private (calculate-optimization-potential (route-id uint))
  ;; Simplified calculation - in reality would use complex analytics
  u25
)

(define-private (calculate-optimal-frequency (demand uint))
  (if (> demand u60) u6 u4) ;; trips per hour based on demand
)

(define-private (calculate-optimal-capacity (peak-demand uint) (off-peak-demand uint))
  (if (> peak-demand u70) u120 u80) ;; passenger capacity
)

(define-private (calculate-historical-average (route-id uint) (time-slot uint))
  ;; Simplified - would analyze actual historical data
  u45
)

(define-private (calculate-trend-factor (route-id uint))
  ;; Simplified trend calculation
  u105 ;; 5% increase trend
)

(define-private (calculate-seasonal-adjustment (date uint))
  ;; Simplified seasonal calculation
  u100 ;; neutral adjustment
)

(define-private (apply-forecast-adjustments (base uint) (trend uint) (seasonal uint))
  (/ (* (* base trend) seasonal) u10000)
)

(define-private (calculate-loyalty-score (total-trips uint) (satisfaction uint))
  (let ((trip-factor (if (> total-trips u50) u100 (* total-trips u2)))
        (satisfaction-factor (* satisfaction u20)))
    (/ (+ trip-factor satisfaction-factor) u2))
)

(define-private (calculate-efficiency-score (load uint) (delay uint))
  (let ((load-score (if (> load u60) u100 (/ (* load u100) u60)))
        (delay-penalty (if (> delay u10) u20 u0)))
    (if (> load-score delay-penalty) (- load-score delay-penalty) u0))
)
