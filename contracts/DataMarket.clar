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
