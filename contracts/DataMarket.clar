;; title: DataMarket
;; version: 1.0
;; summary: A decentralized marketplace for data exchange
;; description: Allows data providers to register datasets, stake tokens, and subscribers to access verified datasets with automated payments

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-stake (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-rating (err u105))

;; minimum stake required to register a dataset (in microSTX)
(define-constant min-stake u1000000) ;; 1 STX

;; data vars
(define-data-var next-dataset-id uint u1)

;; data maps
;; dataset info
(define-map datasets
  { dataset-id: uint }
  {
    provider: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    price: uint,
    stake: uint,
    quality-score: uint,
    active: bool
  }
)

;; track dataset subscriptions
(define-map subscriptions
  { dataset-id: uint, subscriber: principal }
  { expiry: uint }
)

;; track dataset ratings by users
(define-map dataset-ratings
  { dataset-id: uint, rater: principal }
  { rating: uint }
)

;; track total usage of datasets
(define-map dataset-usage
  { dataset-id: uint }
  { access-count: uint }
)

;; public functions

;; register a new dataset with a stake
(define-public (register-dataset (name (string-ascii 100)) (description (string-ascii 500)) (price uint) (stake uint))
  (let
    (
      (dataset-id (var-get next-dataset-id))
    )
    ;; check if stake meets minimum requirement
    (asserts! (>= stake min-stake) err-insufficient-stake)
    
    ;; transfer stake from user to contract
    (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
    
    ;; register the dataset
    (map-set datasets
      { dataset-id: dataset-id }
      {
        provider: tx-sender,
        name: name,
        description: description,
        price: price,
        stake: stake,
        quality-score: u50, ;; default score of 50 out of 100
        active: true
      }
    )
    
    ;; increment dataset ID for next registration
    (var-set next-dataset-id (+ dataset-id u1))
    
    ;; return the new dataset ID
    (ok dataset-id)
  )
)

;; subscribe to a dataset
(define-public (subscribe-to-dataset (dataset-id uint))
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
      (price (get price dataset))
      (provider (get provider dataset))
      (current-block-height stacks-block-height)
      ;; subscription lasts for 1440 blocks (approximately 10 days on Stacks)
      (expiry-height (+ current-block-height u1440))
    )
    
    ;; check if dataset is active
    (asserts! (get active dataset) err-not-found)
    
    ;; transfer payment from subscriber to provider
    (try! (stx-transfer? price tx-sender provider))
    
    ;; record subscription
    (map-set subscriptions
      { dataset-id: dataset-id, subscriber: tx-sender }
      { expiry: expiry-height }
    )
    
    ;; update usage statistics
    (match (map-get? dataset-usage { dataset-id: dataset-id })
      existing-usage (map-set dataset-usage
                      { dataset-id: dataset-id }
                      { access-count: (+ (get access-count existing-usage) u1) })
      (map-set dataset-usage
        { dataset-id: dataset-id }
        { access-count: u1 })
    )
    
    (ok true)
  )
)

;; rate a dataset (1-100)
(define-public (rate-dataset (dataset-id uint) (rating uint))
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
      (subscription (unwrap! (map-get? subscriptions { dataset-id: dataset-id, subscriber: tx-sender }) err-unauthorized))
    )
    
    ;; check if rating is valid (1-100)
    (asserts! (and (>= rating u1) (<= rating u100)) err-invalid-rating)
    
    ;; check if subscription is still valid
    (asserts! (<= stacks-block-height (get expiry subscription)) err-unauthorized)
    
    ;; record the rating
    (map-set dataset-ratings
      { dataset-id: dataset-id, rater: tx-sender }
      { rating: rating }
    )
    
    ;; update dataset quality score (simplified approach - just set it to this rating)
    ;; in a real implementation, you'd calculate an average of all ratings
    (map-set datasets
      { dataset-id: dataset-id }
      (merge dataset { quality-score: rating })
    )
    
    (ok true)
  )
)

;; deactivate a dataset (only provider can do this)
(define-public (deactivate-dataset (dataset-id uint))
  (let
    (
      (dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
    )
    
    ;; check if caller is the provider
    (asserts! (is-eq tx-sender (get provider dataset)) err-unauthorized)
    
    ;; return stake to provider first
    (try! (as-contract (stx-transfer? (get stake dataset) tx-sender (get provider dataset))))
    
    ;; deactivate the dataset only if transfer succeeds
    (map-set datasets
      { dataset-id: dataset-id }
      (merge dataset { active: false })
    )
    
    (ok true)
  )
)

;; read only functions

;; get dataset details
(define-read-only (get-dataset (dataset-id uint))
  (map-get? datasets { dataset-id: dataset-id })
)

;; check if user has active subscription
(define-read-only (has-subscription (dataset-id uint) (user principal))
  (match (map-get? subscriptions { dataset-id: dataset-id, subscriber: user })
    subscription (ok (< stacks-block-height (get expiry subscription)))
    (ok false)
  )
)

;; get dataset usage statistics
(define-read-only (get-dataset-usage (dataset-id uint))
  (default-to { access-count: u0 } (map-get? dataset-usage { dataset-id: dataset-id }))
)



(define-map dataset-categories
    { dataset-id: uint }
    { category: (string-ascii 50) }
)

(define-public (set-dataset-category (dataset-id uint) (category (string-ascii 50)))
    (let
        ((dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found)))
        (asserts! (is-eq tx-sender (get provider dataset)) err-unauthorized)
        (map-set dataset-categories
            { dataset-id: dataset-id }
            { category: category }
        )
        (ok true)
    )
)


(define-map dataset-reviews
    { dataset-id: uint, reviewer: principal }
    { review: (string-ascii 500), timestamp: uint }
)

(define-public (add-review (dataset-id uint) (review-text (string-ascii 500)))
    (let
        ((subscription (unwrap! (map-get? subscriptions { dataset-id: dataset-id, subscriber: tx-sender }) err-unauthorized)))
        (asserts! (<= stacks-block-height (get expiry subscription)) err-unauthorized)
        (map-set dataset-reviews
            { dataset-id: dataset-id, reviewer: tx-sender }
            { review: review-text, timestamp: stacks-block-height }
        )
        (ok true)
    )
)


(define-constant discount-threshold u3)
(define-constant discount-percentage u10)

(define-public (bulk-subscribe (dataset-ids (list 10 uint)))
    (let
        ((prices (map get-dataset-price dataset-ids))
         (total-price (fold + prices u0))
         (discounted-price (if (>= (len dataset-ids) discount-threshold)
            (- total-price (/ (* total-price discount-percentage) u100))
            total-price)))
        (try! (subscribe-multiple dataset-ids discounted-price))
        (ok true)
    )
)

(define-private (get-dataset-price (dataset-id uint))
    (get price (unwrap! (map-get? datasets { dataset-id: dataset-id }) u0))
)

(define-private (subscribe-single (dataset-id uint))
    (let
        ((current-block-height stacks-block-height)
         (expiry-height (+ current-block-height u1440)))
        (map-set subscriptions
            { dataset-id: dataset-id, subscriber: tx-sender }
            { expiry: expiry-height }
        )
    )
)

(define-private (subscribe-multiple (dataset-ids (list 10 uint)) (total-price uint))
    (begin
        (try! (stx-transfer? total-price tx-sender (as-contract tx-sender)))
        (map subscribe-single dataset-ids)
        (ok true)
    )
)


(define-map dataset-versions
    { dataset-id: uint, version: uint }
    { 
        update-notes: (string-ascii 500),
        timestamp: uint
    }
)

(define-data-var version-counter uint u1)

(define-public (update-dataset (dataset-id uint) (update-notes (string-ascii 500)))
    (let
        ((dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
         (current-version (var-get version-counter)))
        (asserts! (is-eq tx-sender (get provider dataset)) err-unauthorized)
        (map-set dataset-versions
            { dataset-id: dataset-id, version: current-version }
            { update-notes: update-notes, timestamp: stacks-block-height }
        )
        (var-set version-counter (+ current-version u1))
        (ok true)
    )
)


(define-map referral-rewards
    { referrer: principal }
    { total-rewards: uint }
)

(define-constant referral-percentage u5)

(define-public (subscribe-with-referral (dataset-id uint) (referrer principal))
    (let
        ((dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
         (price (get price dataset))
         (reward-amount (/ (* price referral-percentage) u100)))
        (try! (subscribe-to-dataset dataset-id))
        (match (map-get? referral-rewards { referrer: referrer })
            existing-rewards (map-set referral-rewards
                { referrer: referrer }
                { total-rewards: (+ (get total-rewards existing-rewards) reward-amount) })
            (map-set referral-rewards
                { referrer: referrer }
                { total-rewards: reward-amount })
        )
        (try! (stx-transfer? reward-amount (as-contract tx-sender) referrer))
        (ok true)
    )
)


(define-map featured-datasets
    { dataset-id: uint }
    { featured: bool }
)

(define-public (set-featured-dataset (dataset-id uint) (featured bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found)
        (map-set featured-datasets
            { dataset-id: dataset-id }
            { featured: featured }
        )
        (ok true)
    )
)


(define-map dataset-analytics
    { dataset-id: uint }
    {
        daily-views: uint,
        weekly-views: uint,
        monthly-revenue: uint,
        last-accessed: uint
    }
)

(define-public (record-dataset-access (dataset-id uint))
    (let
        ((dataset (unwrap! (map-get? datasets { dataset-id: dataset-id }) err-not-found))
         (current-analytics (default-to { daily-views: u0, weekly-views: u0, monthly-revenue: u0, last-accessed: u0 }
            (map-get? dataset-analytics { dataset-id: dataset-id }))))
        (map-set dataset-analytics
            { dataset-id: dataset-id }
            {
                daily-views: (+ (get daily-views current-analytics) u1),
                weekly-views: (+ (get weekly-views current-analytics) u1),
                monthly-revenue: (+ (get monthly-revenue current-analytics) (get price dataset)),
                last-accessed: stacks-block-height
            }
        )
        (ok true)
    )
)