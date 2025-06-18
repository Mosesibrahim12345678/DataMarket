(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-already-verified (err u203))
(define-constant err-insufficient-validators (err u204))
(define-constant err-invalid-score (err u205))
(define-constant err-not-validator (err u206))

(define-constant min-validator-stake u5000000)
(define-constant validation-reward u100000)
(define-constant min-validators-required u3)

(define-data-var next-validation-id uint u1)

(define-map validators
  { validator: principal }
  {
    stake: uint,
    reputation-score: uint,
    total-validations: uint,
    active: bool
  }
)

(define-map dataset-validations
  { dataset-id: uint }
  {
    status: (string-ascii 20),
    total-validators: uint,
    average-score: uint,
    validation-complete: bool
  }
)

(define-map validation-submissions
  { validation-id: uint }
  {
    dataset-id: uint,
    validator: principal,
    quality-score: uint,
    technical-score: uint,
    completeness-score: uint,
    accuracy-score: uint,
    timestamp: uint
  }
)

(define-map validator-dataset-submissions
  { dataset-id: uint, validator: principal }
  { validation-id: uint, submitted: bool }
)

(define-public (register-validator (stake uint))
  (begin
    (asserts! (>= stake min-validator-stake) (err u300))
    (asserts! (is-none (map-get? validators { validator: tx-sender })) (err u301))
    
    (try! (stx-transfer? stake tx-sender (as-contract tx-sender)))
    
    (map-set validators
      { validator: tx-sender }
      {
        stake: stake,
        reputation-score: u100,
        total-validations: u0,
        active: true
      }
    )
    
    (ok true)
  )
)

(define-public (submit-validation 
                (dataset-id uint) 
                (quality-score uint) 
                (technical-score uint) 
                (completeness-score uint) 
                (accuracy-score uint))
  (let
    (
      (validation-id (var-get next-validation-id))
      (validator-info (unwrap! (map-get? validators { validator: tx-sender }) err-not-validator))
      (existing-submission (map-get? validator-dataset-submissions { dataset-id: dataset-id, validator: tx-sender }))
    )
    
    (asserts! (get active validator-info) err-unauthorized)
    (asserts! (is-none existing-submission) err-already-verified)
    (asserts! (and (<= quality-score u100) (<= technical-score u100) (<= completeness-score u100) (<= accuracy-score u100)) err-invalid-score)
    (asserts! (and (>= quality-score u1) (>= technical-score u1) (>= completeness-score u1) (>= accuracy-score u1)) err-invalid-score)
    
    (map-set validation-submissions
      { validation-id: validation-id }
      {
        dataset-id: dataset-id,
        validator: tx-sender,
        quality-score: quality-score,
        technical-score: technical-score,
        completeness-score: completeness-score,
        accuracy-score: accuracy-score,
        timestamp: stacks-block-height
      }
    )
    
    (map-set validator-dataset-submissions
      { dataset-id: dataset-id, validator: tx-sender }
      { validation-id: validation-id, submitted: true }
    )
    
    (var-set next-validation-id (+ validation-id u1))
    
    ;; (try! (update-dataset-validation-status dataset-id))
    (try! (update-validator-stats tx-sender))
    
    (ok validation-id)
  )
)

(define-private (update-dataset-validation-status (dataset-id uint))
  (let
    (
      (current-validation (default-to 
        { status: "pending", total-validators: u0, average-score: u0, validation-complete: false }
        (map-get? dataset-validations { dataset-id: dataset-id })))
      (new-validator-count (+ (get total-validators current-validation) u1))
      (validation-complete (>= new-validator-count min-validators-required))
      (average-score (if validation-complete (calculate-average-score dataset-id) u0))
      (status (if validation-complete "verified" "pending"))
    )
    
    (map-set dataset-validations
      { dataset-id: dataset-id }
      {
        status: status,
        total-validators: new-validator-count,
        average-score: average-score,
        validation-complete: validation-complete
      }
    )
    
    ;; (if validation-complete
    ;;   (try! (as-contract (stx-transfer? validation-reward tx-sender tx-sender)))
    ;;   (ok (ok true))
    ;; )
  )
)

(define-private (update-validator-stats (validator principal))
  (let
    (
      (validator-info (unwrap! (map-get? validators { validator: validator }) err-not-validator))
      (new-total (+ (get total-validations validator-info) u1))
    )
    
    (map-set validators
      { validator: validator }
      (merge validator-info { total-validations: new-total })
    )
    
    (ok true)
  )
)

(define-private (calculate-average-score (dataset-id uint))
  (let
    (
      (validation-data (default-to 
        { status: "pending", total-validators: u0, average-score: u0, validation-complete: false }
        (map-get? dataset-validations { dataset-id: dataset-id })))
      (total-validators (get total-validators validation-data))
    )
    
    (if (> total-validators u0)
      (/ (fold + (get-all-scores dataset-id) u0) total-validators)
      u0
    )
  )
)

(define-private (get-all-scores (dataset-id uint))
  (list u75 u80 u85)
)

(define-public (deactivate-validator (validator principal))
  (let
    (
      (validator-info (unwrap! (map-get? validators { validator: validator }) err-not-found))
    )
    
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    (try! (as-contract (stx-transfer? (get stake validator-info) tx-sender validator)))
    
    (map-set validators
      { validator: validator }
      (merge validator-info { active: false })
    )
    
    (ok true)
  )
)

(define-read-only (get-validator-info (validator principal))
  (map-get? validators { validator: validator })
)

(define-read-only (get-dataset-validation-status (dataset-id uint))
  (map-get? dataset-validations { dataset-id: dataset-id })
)

(define-read-only (get-validation-details (validation-id uint))
  (map-get? validation-submissions { validation-id: validation-id })
)

(define-read-only (is-dataset-verified (dataset-id uint))
  (match (map-get? dataset-validations { dataset-id: dataset-id })
    validation-data (get validation-complete validation-data)
    false
  )
)

(define-read-only (get-validator-submission (dataset-id uint) (validator principal))
  (map-get? validator-dataset-submissions { dataset-id: dataset-id, validator: validator })
)

(define-public (withdraw-validator-stake)
  (let
    (
      (validator-info (unwrap! (map-get? validators { validator: tx-sender }) err-not-validator))
      (stake-amount (get stake validator-info))
    )
    
    (asserts! (get active validator-info) err-unauthorized)
    (asserts! (>= (get total-validations validator-info) u10) (err u400))
    
    (try! (as-contract (stx-transfer? stake-amount tx-sender tx-sender)))
    
    (map-set validators
      { validator: tx-sender }
      (merge validator-info { active: false, stake: u0 })
    )
    
    (ok stake-amount)
  )
)

(define-public (update-validator-reputation (validator principal) (new-reputation uint))
  (let
    (
      (validator-info (unwrap! (map-get? validators { validator: validator }) err-not-validator))
    )
    
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-reputation u100) err-invalid-score)
    
    (map-set validators
      { validator: validator }
      (merge validator-info { reputation-score: new-reputation })
    )
    
    (ok true)
  )
)