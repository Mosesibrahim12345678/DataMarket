;; DataInsights Contract
;; Provides comprehensive analytics and insights for the DataMarket ecosystem

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u301))
(define-constant err-unauthorized (err u302))
(define-constant err-invalid-period (err u303))

;; Data variables
(define-data-var analytics-enabled bool true)
(define-data-var current-analytics-period uint u1)

;; Track dataset access patterns over time
(define-map dataset-usage-metrics
  { dataset-id: uint, period: uint }
  {
    access-count: uint,
    unique-subscribers: uint,
    total-revenue: uint,
    avg-session-length: uint,
    peak-usage-hour: uint
  }
)

;; Track subscriber behavior patterns
(define-map subscriber-analytics
  { subscriber: principal, period: uint }
  {
    datasets-accessed: uint,
    total-spent: uint,
    avg-rating-given: uint,
    subscription-renewals: uint,
    favorite-category: (string-ascii 50)
  }
)

;; Market trend analytics
(define-map market-trends
  { period: uint }
  {
    total-datasets: uint,
    total-subscribers: uint,
    total-transactions: uint,
    avg-dataset-price: uint,
    most-popular-category: (string-ascii 50),
    total-market-volume: uint
  }
)

;; Record dataset access for analytics
(define-public (record-dataset-access (dataset-id uint) (session-length uint))
  (let
    (
      (period (var-get current-analytics-period))
      (current-metrics (default-to 
        { access-count: u0, unique-subscribers: u0, total-revenue: u0, 
          avg-session-length: u0, peak-usage-hour: u0 }
        (map-get? dataset-usage-metrics { dataset-id: dataset-id, period: period })))
      (current-hour (mod stacks-block-height u24))
      (new-access-count (+ (get access-count current-metrics) u1))
      (new-avg-session (/ (+ (* (get avg-session-length current-metrics) (get access-count current-metrics)) session-length) new-access-count))
    )
    
    (asserts! (var-get analytics-enabled) (ok false))
    
    (map-set dataset-usage-metrics
      { dataset-id: dataset-id, period: period }
      {
        access-count: new-access-count,
        unique-subscribers: (get unique-subscribers current-metrics),
        total-revenue: (get total-revenue current-metrics),
        avg-session-length: new-avg-session,
        peak-usage-hour: (if (> new-access-count u1) 
          (if (> (get access-count current-metrics) u0) (get peak-usage-hour current-metrics) current-hour)
          current-hour)
      }
    )
    
    (ok true)
  )
)

;; Record subscription for analytics
(define-public (record-subscription (dataset-id uint) (subscriber principal) (amount uint) (category (string-ascii 50)))
  (let
    (
      (period (var-get current-analytics-period))
      (dataset-metrics (default-to 
        { access-count: u0, unique-subscribers: u0, total-revenue: u0, 
          avg-session-length: u0, peak-usage-hour: u0 }
        (map-get? dataset-usage-metrics { dataset-id: dataset-id, period: period })))
      (subscriber-data (default-to
        { datasets-accessed: u0, total-spent: u0, avg-rating-given: u0,
          subscription-renewals: u0, favorite-category: "" }
        (map-get? subscriber-analytics { subscriber: subscriber, period: period })))
      (market-data (default-to
        { total-datasets: u0, total-subscribers: u0, total-transactions: u0,
          avg-dataset-price: u0, most-popular-category: "", total-market-volume: u0 }
        (map-get? market-trends { period: period })))
    )
    
    ;; Update dataset metrics
    (map-set dataset-usage-metrics
      { dataset-id: dataset-id, period: period }
      (merge dataset-metrics {
        unique-subscribers: (+ (get unique-subscribers dataset-metrics) u1),
        total-revenue: (+ (get total-revenue dataset-metrics) amount)
      })
    )
    
    ;; Update subscriber analytics
    (map-set subscriber-analytics
      { subscriber: subscriber, period: period }
      {
        datasets-accessed: (+ (get datasets-accessed subscriber-data) u1),
        total-spent: (+ (get total-spent subscriber-data) amount),
        avg-rating-given: (get avg-rating-given subscriber-data),
        subscription-renewals: (get subscription-renewals subscriber-data),
        favorite-category: category
      }
    )
    
    ;; Update market trends
    (map-set market-trends
      { period: period }
      (merge market-data {
        total-transactions: (+ (get total-transactions market-data) u1),
        total-market-volume: (+ (get total-market-volume market-data) amount)
      })
    )
    
    (ok true)
  )
)

;; Get dataset popularity ranking
(define-read-only (get-dataset-popularity-score (dataset-id uint))
  (let
    (
      (period (var-get current-analytics-period))
      (metrics (map-get? dataset-usage-metrics { dataset-id: dataset-id, period: period }))
    )
    
    (match metrics
      data (ok (+ (* (get access-count data) u3) 
                  (* (get unique-subscribers data) u5)
                  (/ (get total-revenue data) u1000)))
      (ok u0)
    )
  )
)

;; Get market overview
(define-read-only (get-market-overview (period-param uint))
  (let
    (
      (period (if (is-eq period-param u0) (var-get current-analytics-period) period-param))
    )
    
    (map-get? market-trends { period: period })
  )
)

;; Get subscriber insights
(define-read-only (get-subscriber-insights (subscriber principal))
  (let
    (
      (period (var-get current-analytics-period))
    )
    
    (map-get? subscriber-analytics { subscriber: subscriber, period: period })
  )
)

;; Administrative functions
(define-public (advance-analytics-period)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set current-analytics-period (+ (var-get current-analytics-period) u1))
    (ok (var-get current-analytics-period))
  )
)

(define-public (toggle-analytics (enabled bool))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (var-set analytics-enabled enabled)
    (ok enabled)
  )
)

;; Get current analytics period
(define-read-only (get-current-period)
  (var-get current-analytics-period)
)

;; Check if analytics are enabled
(define-read-only (get-analytics-status)
  (var-get analytics-enabled)
)
