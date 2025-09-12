(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u104))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-status (err u106))
(define-constant err-deadline-passed (err u107))

(define-constant escrow-period u432)

(define-map escrow-payments
  { escrow-id: uint }
  {
    subscriber: principal,
    provider: principal,
    dataset-id: uint,
    amount: uint,
    created-at: uint,
    deadline: uint,
    status: (string-ascii 20)
  }
)

(define-map escrow-disputes
  { escrow-id: uint }
  {
    dispute-reason: (string-ascii 500),
    created-at: uint,
    resolved: bool
  }
)

(define-data-var next-escrow-id uint u1)

(define-public (create-escrow-payment (dataset-id uint) (amount uint) (provider principal))
  (let
    (
      (escrow-id (var-get next-escrow-id))
      (current-block-height stacks-block-height)
      (deadline (+ current-block-height escrow-period))
    )
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set escrow-payments
      { escrow-id: escrow-id }
      {
        subscriber: tx-sender,
        provider: provider,
        dataset-id: dataset-id,
        amount: amount,
        created-at: current-block-height,
        deadline: deadline,
        status: "pending"
      }
    )
    
    (var-set next-escrow-id (+ escrow-id u1))
    
    (ok escrow-id)
  )
)

(define-public (confirm-data-quality (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-payments { escrow-id: escrow-id }) err-not-found))
    )
    
    (asserts! (is-eq tx-sender (get subscriber escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "pending") err-invalid-status)
    (asserts! (< stacks-block-height (get deadline escrow)) err-deadline-passed)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get provider escrow))))
    
    (map-set escrow-payments
      { escrow-id: escrow-id }
      (merge escrow { status: "completed" })
    )
    
    (ok true)
  )
)

(define-public (raise-dispute (escrow-id uint) (reason (string-ascii 500)))
  (let
    (
      (escrow (unwrap! (map-get? escrow-payments { escrow-id: escrow-id }) err-not-found))
    )
    
    (asserts! (is-eq tx-sender (get subscriber escrow)) err-unauthorized)
    (asserts! (is-eq (get status escrow) "pending") err-invalid-status)
    (asserts! (< stacks-block-height (get deadline escrow)) err-deadline-passed)
    
    (map-set escrow-disputes
      { escrow-id: escrow-id }
      {
        dispute-reason: reason,
        created-at: stacks-block-height,
        resolved: false
      }
    )
    
    (map-set escrow-payments
      { escrow-id: escrow-id }
      (merge escrow { status: "disputed" })
    )
    
    (ok true)
  )
)

(define-public (resolve-dispute (escrow-id uint) (favor-provider bool))
  (let
    (
      (escrow (unwrap! (map-get? escrow-payments { escrow-id: escrow-id }) err-not-found))
      (dispute (unwrap! (map-get? escrow-disputes { escrow-id: escrow-id }) err-not-found))
    )
    
    (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
    (asserts! (is-eq (get status escrow) "disputed") err-invalid-status)
    (asserts! (not (get resolved dispute)) err-already-exists)
    
    (if favor-provider
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get provider escrow))))
      (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get subscriber escrow))))
    )
    
    (map-set escrow-disputes
      { escrow-id: escrow-id }
      (merge dispute { resolved: true })
    )
    
    (map-set escrow-payments
      { escrow-id: escrow-id }
      (merge escrow { status: (if favor-provider "completed" "refunded") })
    )
    
    (ok true)
  )
)

(define-public (auto-release-payment (escrow-id uint))
  (let
    (
      (escrow (unwrap! (map-get? escrow-payments { escrow-id: escrow-id }) err-not-found))
    )
    
    (asserts! (is-eq (get status escrow) "pending") err-invalid-status)
    (asserts! (>= stacks-block-height (get deadline escrow)) err-deadline-passed)
    
    (try! (as-contract (stx-transfer? (get amount escrow) tx-sender (get provider escrow))))
    
    (map-set escrow-payments
      { escrow-id: escrow-id }
      (merge escrow { status: "auto-released" })
    )
    
    (ok true)
  )
)

(define-read-only (get-escrow (escrow-id uint))
  (map-get? escrow-payments { escrow-id: escrow-id })
)

(define-read-only (get-dispute (escrow-id uint))
  (map-get? escrow-disputes { escrow-id: escrow-id })
)

(define-read-only (get-escrow-status (escrow-id uint))
  (match (map-get? escrow-payments { escrow-id: escrow-id })
    escrow (ok (get status escrow))
    err-not-found
  )
)

(define-read-only (is-escrow-expired (escrow-id uint))
  (match (map-get? escrow-payments { escrow-id: escrow-id })
    escrow (ok (>= stacks-block-height (get deadline escrow)))
    err-not-found
  )
)
