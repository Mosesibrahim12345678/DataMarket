;; Data Provider Reputation System
;; Tracks multi-dimensional reputation metrics for data providers

;; Constants for reputation calculations
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-rating (err u105))
(define-constant err-insufficient-reputation (err u108))

;; Reputation score boundaries
(define-constant max-reputation-score u1000)
(define-constant min-reputation-score u0)
(define-constant initial-reputation-score u500)

;; Weight factors for reputation calculation
(define-constant quality-weight u40)
(define-constant reliability-weight u25)
(define-constant responsiveness-weight u20)
(define-constant dispute-weight u15)

;; Provider reputation profile
(define-map provider-reputation
  { provider: principal }
  {
    total-score: uint,
    quality-score: uint,
    reliability-score: uint,
    responsiveness-score: uint,
    dispute-score: uint,
    total-datasets: uint,
    active-datasets: uint,
    total-subscribers: uint,
    last-updated: uint
  }
)

;; Provider performance metrics
(define-map provider-metrics
  { provider: principal }
  {
    successful-transactions: uint,
    failed-transactions: uint,
    total-disputes: uint,
    resolved-disputes: uint,
    average-response-time: uint,
    subscription-renewals: uint,
    quality-ratings-sum: uint,
    quality-ratings-count: uint
  }
)

;; Reputation events for audit trail
(define-map reputation-events
  { provider: principal, event-id: uint }
  {
    event-type: (string-ascii 50),
    score-change: int,
    timestamp: uint,
    description: (string-ascii 200)
  }
)

;; Track next event ID per provider
(define-map provider-event-counters
  { provider: principal }
  { next-event-id: uint }
)

;; Reputation tier benefits
(define-map reputation-tiers
  { tier-level: uint }
  {
    min-score: uint,
    max-score: uint,
    fee-discount: uint,
    featured-priority: uint,
    max-datasets: uint
  }
)

;; Initialize reputation tiers in constructor-like function
(define-private (initialize-reputation-tiers)
  (begin
    ;; Bronze tier (0-299)
    (map-set reputation-tiers
      { tier-level: u1 }
      { min-score: u0, max-score: u299, fee-discount: u0, featured-priority: u1, max-datasets: u5 }
    )
    ;; Silver tier (300-599)
    (map-set reputation-tiers
      { tier-level: u2 }
      { min-score: u300, max-score: u599, fee-discount: u5, featured-priority: u2, max-datasets: u15 }
    )
    ;; Gold tier (600-799)
    (map-set reputation-tiers
      { tier-level: u3 }
      { min-score: u600, max-score: u799, fee-discount: u10, featured-priority: u3, max-datasets: u50 }
    )
    ;; Platinum tier (800-1000)
    (map-set reputation-tiers
      { tier-level: u4 }
      { min-score: u800, max-score: u1000, fee-discount: u15, featured-priority: u4, max-datasets: u999 }
    )
  )
)

;; Initialize provider reputation profile
(define-public (initialize-provider-reputation (provider principal))
  (begin
    ;; Initialize tiers on first call
    (initialize-reputation-tiers)
    
    ;; Set initial reputation if not exists
    (match (map-get? provider-reputation { provider: provider })
      existing-rep (ok false) ;; Already initialized
      (begin
        (map-set provider-reputation
          { provider: provider }
          {
            total-score: initial-reputation-score,
            quality-score: initial-reputation-score,
            reliability-score: initial-reputation-score,
            responsiveness-score: initial-reputation-score,
            dispute-score: initial-reputation-score,
            total-datasets: u0,
            active-datasets: u0,
            total-subscribers: u0,
            last-updated: stacks-block-height
          }
        )
        (map-set provider-metrics
          { provider: provider }
          {
            successful-transactions: u0,
            failed-transactions: u0,
            total-disputes: u0,
            resolved-disputes: u0,
            average-response-time: u0,
            subscription-renewals: u0,
            quality-ratings-sum: u0,
            quality-ratings-count: u0
          }
        )
        (map-set provider-event-counters
          { provider: provider }
          { next-event-id: u1 }
        )
        (ok true)
      )
    )
  )
)

;; Update provider reputation after dataset registration
(define-public (update-reputation-on-dataset-registration (provider principal))
  (let
    (
      (reputation (default-to 
        { total-score: initial-reputation-score, quality-score: initial-reputation-score, 
          reliability-score: initial-reputation-score, responsiveness-score: initial-reputation-score,
          dispute-score: initial-reputation-score, total-datasets: u0, active-datasets: u0, 
          total-subscribers: u0, last-updated: stacks-block-height }
        (map-get? provider-reputation { provider: provider })))
    )
    
    ;; Update dataset counts
    (map-set provider-reputation
      { provider: provider }
      (merge reputation {
        total-datasets: (+ (get total-datasets reputation) u1),
        active-datasets: (+ (get active-datasets reputation) u1),
        last-updated: stacks-block-height
      })
    )
    
    ;; Log reputation event
    (unwrap-panic (log-reputation-event provider "dataset_registered" 5 "New dataset registered"))
    
    (ok true)
  )
)

;; Update reputation after successful subscription
(define-public (update-reputation-on-subscription (provider principal))
  (let
    (
      (reputation (unwrap! (map-get? provider-reputation { provider: provider }) err-not-found))
      (metrics (default-to
        { successful-transactions: u0, failed-transactions: u0, total-disputes: u0,
          resolved-disputes: u0, average-response-time: u0, subscription-renewals: u0,
          quality-ratings-sum: u0, quality-ratings-count: u0 }
        (map-get? provider-metrics { provider: provider })))
    )
    
    ;; Update metrics
    (map-set provider-metrics
      { provider: provider }
      (merge metrics {
        successful-transactions: (+ (get successful-transactions metrics) u1)
      })
    )
    
    ;; Update subscriber count
    (map-set provider-reputation
      { provider: provider }
      (merge reputation {
        total-subscribers: (+ (get total-subscribers reputation) u1),
        last-updated: stacks-block-height
      })
    )
    
    ;; Small positive reputation boost
    (unwrap-panic (log-reputation-event provider "subscription_gained" 2 "New subscription acquired"))
    
    (ok true)
  )
)

;; Update reputation based on quality rating
(define-public (update-reputation-on-rating (provider principal) (rating uint))
  (let
    (
      (reputation (unwrap! (map-get? provider-reputation { provider: provider }) err-not-found))
      (metrics (unwrap! (map-get? provider-metrics { provider: provider }) err-not-found))
      (new-ratings-count (+ (get quality-ratings-count metrics) u1))
      (new-ratings-sum (+ (get quality-ratings-sum metrics) rating))
      (new-average (/ new-ratings-sum new-ratings-count))
      (quality-score-change (if (> new-average u50) 
        (/ (- new-average u50) u2) 
        (- u0 (/ (- u50 new-average) u2))))
    )
    
    ;; Validate rating range
    (asserts! (and (>= rating u1) (<= rating u100)) err-invalid-rating)
    
    ;; Update metrics
    (map-set provider-metrics
      { provider: provider }
      (merge metrics {
        quality-ratings-sum: new-ratings-sum,
        quality-ratings-count: new-ratings-count
      })
    )
    
    ;; Calculate new quality score
    (let ((new-quality-score (+ (get quality-score reputation) quality-score-change)))
      (map-set provider-reputation
        { provider: provider }
        (merge reputation {
          quality-score: (if (> new-quality-score max-reputation-score) 
            max-reputation-score 
            (if (< new-quality-score min-reputation-score) 
              min-reputation-score 
              new-quality-score)),
          last-updated: stacks-block-height
        })
      )
    )
    
    ;; Recalculate total reputation score
    (try! (recalculate-total-reputation provider))
    
    ;; Log event
    (unwrap-panic (log-reputation-event provider "quality_rating" (if (> quality-score-change u0) 
      (to-int quality-score-change) 
      (- (to-int (- u0 quality-score-change)))) "Quality rating received"))
    
    (ok true)
  )
)

;; Update reputation on dispute
(define-public (update-reputation-on-dispute (provider principal) (resolved bool))
  (let
    (
      (reputation (unwrap! (map-get? provider-reputation { provider: provider }) err-not-found))
      (metrics (unwrap! (map-get? provider-metrics { provider: provider }) err-not-found))
      (dispute-penalty (if resolved u5 u15))
    )
    
    ;; Update metrics
    (map-set provider-metrics
      { provider: provider }
      (merge metrics {
        total-disputes: (+ (get total-disputes metrics) u1),
        resolved-disputes: (if resolved (+ (get resolved-disputes metrics) u1) (get resolved-disputes metrics))
      })
    )
    
    ;; Apply dispute penalty to dispute score
    (let ((current-dispute-score (get dispute-score reputation))
          (new-dispute-score (if (>= current-dispute-score dispute-penalty)
            (- current-dispute-score dispute-penalty)
            min-reputation-score)))
      (map-set provider-reputation
        { provider: provider }
        (merge reputation {
          dispute-score: (if (< new-dispute-score min-reputation-score) 
            min-reputation-score 
            new-dispute-score),
          last-updated: stacks-block-height
        })
      )
    )
    
    ;; Recalculate total reputation
    (try! (recalculate-total-reputation provider))
    
    ;; Log event
    (unwrap-panic (log-reputation-event provider "dispute_occurred" (- 0 (to-int dispute-penalty)) 
      (if resolved "Dispute resolved favorably" "Dispute resolved unfavorably")))
    
    (ok true)
  )
)

;; Recalculate total reputation score using weighted average
(define-private (recalculate-total-reputation (provider principal))
  (let
    (
      (reputation (unwrap! (map-get? provider-reputation { provider: provider }) err-not-found))
      (quality (get quality-score reputation))
      (reliability (get reliability-score reputation))
      (responsiveness (get responsiveness-score reputation))
      (dispute (get dispute-score reputation))
      (total-weight (+ quality-weight reliability-weight responsiveness-weight dispute-weight))
      (weighted-sum (+ 
        (* quality quality-weight)
        (* reliability reliability-weight)
        (* responsiveness responsiveness-weight)
        (* dispute dispute-weight)))
      (new-total-score (/ weighted-sum total-weight))
    )
    
    (map-set provider-reputation
      { provider: provider }
      (merge reputation { total-score: new-total-score })
    )
    
    (ok true)
  )
)

;; Log reputation event for audit trail
(define-private (log-reputation-event (provider principal) (event-type (string-ascii 50)) (score-change int) (description (string-ascii 200)))
  (let
    (
      (counter (default-to { next-event-id: u1 } (map-get? provider-event-counters { provider: provider })))
      (event-id (get next-event-id counter))
    )
    
    (map-set reputation-events
      { provider: provider, event-id: event-id }
      {
        event-type: event-type,
        score-change: score-change,
        timestamp: stacks-block-height,
        description: description
      }
    )
    
    (map-set provider-event-counters
      { provider: provider }
      { next-event-id: (+ event-id u1) }
    )
    
    (ok true)
  )
)

;; Get provider's reputation tier
(define-read-only (get-provider-tier (provider principal))
  (match (map-get? provider-reputation { provider: provider })
    reputation 
    (let ((score (get total-score reputation)))
      (if (>= score u800) u4
        (if (>= score u600) u3
          (if (>= score u300) u2 u1))))
    u1
  )
)

;; Calculate fee discount for provider based on reputation
(define-read-only (get-reputation-fee-discount (provider principal))
  (let ((tier (get-provider-tier provider)))
    (match (map-get? reputation-tiers { tier-level: tier })
      tier-data (get fee-discount tier-data)
      u0
    )
  )
)

;; Check if provider meets reputation threshold for action
(define-read-only (meets-reputation-threshold (provider principal) (threshold uint))
  (match (map-get? provider-reputation { provider: provider })
    reputation (>= (get total-score reputation) threshold)
    false
  )
)

;; Read-only functions for data access
(define-read-only (get-provider-reputation (provider principal))
  (map-get? provider-reputation { provider: provider })
)

(define-read-only (get-provider-metrics (provider principal))
  (map-get? provider-metrics { provider: provider })
)

(define-read-only (get-reputation-event (provider principal) (event-id uint))
  (map-get? reputation-events { provider: provider, event-id: event-id })
)

(define-read-only (get-tier-benefits (tier-level uint))
  (map-get? reputation-tiers { tier-level: tier-level })
)
