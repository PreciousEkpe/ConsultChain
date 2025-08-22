;; ConsultChain: Decentralized Business Consulting Marketplace
;; Version: 1.0.0
;; Connects business consultants with companies for strategic advisory and consulting services

(define-data-var marketplace-director principal tx-sender)

(define-map consultant-profiles
  { consultant-id: uint }
  {
    advisor: principal,
    consultation-rate: uint,
    specialization-field: (string-ascii 50),
    consultant-credentials: (string-ascii 500),
    consulting-years: uint,
    accredited: bool
  })

(define-map consulting-engagements
  { consultant-id: uint, engagement-id: uint }
  {
    company: principal,
    engagement-time: uint,
    engagement-type: (string-ascii 20)
  })

(define-data-var next-consultant-id uint u1)

(define-map engagement-tracker
  { consultant-id: uint }
  { engagements: uint })

;; Register as a consultant
(define-public (register-consultant (field-input (string-ascii 50)) (credentials-input (string-ascii 500)) (years-input uint) (rate-input uint))
  (let
    (
      (consultant-id (var-get next-consultant-id))
      (engagement-id u0)
      (field field-input)
      (credentials credentials-input)
      (years years-input)
      (rate rate-input)
    )
    ;; Input validation
    (asserts! (> rate u0) (err u1))
    (asserts! (> (len field) u0) (err u5))
    (asserts! (> (len credentials) u0) (err u6))
    (asserts! (> years u0) (err u7))
    
    (map-set consultant-profiles
      { consultant-id: consultant-id }
      {
        advisor: tx-sender,
        consultation-rate: rate,
        specialization-field: field,
        consultant-credentials: credentials,
        consulting-years: years,
        accredited: false
      }
    )
    (map-set consulting-engagements
      { consultant-id: consultant-id, engagement-id: engagement-id }
      {
        company: tx-sender,
        engagement-time: consultant-id,
        engagement-type: "registered"
      }
    )
    (map-set engagement-tracker
      { consultant-id: consultant-id }
      { engagements: u1 }
    )
    (var-set next-consultant-id (+ consultant-id u1))
    (ok consultant-id)
  ))

;; Engage a consultant
(define-public (engage-consultant (consultant-id-input uint))
  (let
    (
      (consultant-id consultant-id-input)
      (consultant-info (unwrap! (map-get? consultant-profiles { consultant-id: consultant-id }) (err u2)))
      (rate (get consultation-rate consultant-info))
      (advisor (get advisor consultant-info))
      (engagement-data (default-to { engagements: u0 } (map-get? engagement-tracker { consultant-id: consultant-id })))
      (engagement-id (get engagements engagement-data))
      (new-engagement-id (+ engagement-id u1))
    )
    ;; Input validation
    (asserts! (> consultant-id u0) (err u8))
    (asserts! (not (is-eq tx-sender advisor)) (err u3))
    
    (try! (stx-transfer? rate tx-sender advisor))
    (map-set consulting-engagements
      { consultant-id: consultant-id, engagement-id: engagement-id }
      {
        company: tx-sender,
        engagement-time: (var-get next-consultant-id),
        engagement-type: "engaged"
      }
    )
    (map-set engagement-tracker
      { consultant-id: consultant-id }
      { engagements: new-engagement-id }
    )
    (ok true)
  ))

;; Accredit a consultant (director only)
(define-public (accredit-consultant (consultant-id-input uint))
  (let
    (
      (consultant-id consultant-id-input)
      (consultant-info (unwrap! (map-get? consultant-profiles { consultant-id: consultant-id }) (err u2)))
      (engagement-data (default-to { engagements: u0 } (map-get? engagement-tracker { consultant-id: consultant-id })))
      (engagement-id (get engagements engagement-data))
      (new-engagement-id (+ engagement-id u1))
    )
    ;; Input validation
    (asserts! (> consultant-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get marketplace-director)) (err u4))
    
    (map-set consultant-profiles
      { consultant-id: consultant-id }
      (merge consultant-info { accredited: true })
    )
    (map-set consulting-engagements
      { consultant-id: consultant-id, engagement-id: engagement-id }
      {
        company: (get advisor consultant-info),
        engagement-time: (var-get next-consultant-id),
        engagement-type: "accredited"
      }
    )
    (map-set engagement-tracker
      { consultant-id: consultant-id }
      { engagements: new-engagement-id }
    )
    (ok true)
  ))

;; Get consultant profile
(define-read-only (get-consultant (consultant-id uint))
  (map-get? consultant-profiles { consultant-id: consultant-id }))

;; Get consulting engagement record
(define-read-only (get-engagement-record (consultant-id uint) (engagement-id uint))
  (map-get? consulting-engagements { consultant-id: consultant-id, engagement-id: engagement-id }))

;; Get total engagements for a consultant
(define-read-only (get-engagement-count (consultant-id uint))
  (let
    (
      (engagement-data (default-to { engagements: u0 } (map-get? engagement-tracker { consultant-id: consultant-id })))
    )
    (get engagements engagement-data)
  ))

;; Get marketplace stats
(define-read-only (get-marketplace-stats)
  {
    director: (var-get marketplace-director),
    total-consultants: (- (var-get next-consultant-id) u1)
  })