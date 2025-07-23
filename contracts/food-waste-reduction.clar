;; Food Waste Reduction Coordination Contract
;; Minimizes food waste throughout the supply chain

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-EXISTS (err u202))
(define-constant ERR-INVALID-INPUT (err u203))
(define-constant ERR-INSUFFICIENT-INVENTORY (err u204))

;; Participant types
(define-constant PRODUCER u1)
(define-constant DISTRIBUTOR u2)
(define-constant RETAILER u3)
(define-constant CONSUMER-ORG u4)

;; Data Variables
(define-data-var total-participants uint u0)
(define-data-var total-waste-prevented uint u0)
(define-data-var coordination-fee uint u100)

;; Data Maps
(define-map participants principal
  {
    participant-id: uint,
    name: (string-ascii 100),
    participant-type: uint,
    location: (string-ascii 100),
    waste-reduction-target: uint,
    actual-waste-reduction: uint,
    reputation-score: uint,
    active: bool,
    joined-at: uint
  })

(define-map inventory-reports principal
  {
    total-inventory: uint,
    waste-generated: uint,
    waste-prevented: uint,
    surplus-available: uint,
    last-report-period: uint,
    reports-count: uint
  })

(define-map coordination-requests uint
  {
    requester: principal,
    request-type: uint, ;; 1=surplus-offer, 2=waste-pickup, 3=redistribution
    quantity: uint,
    product-type: (string-ascii 50),
    location: (string-ascii 100),
    expiry-timeline: uint,
    fulfilled: bool,
    created-at: uint
  })

(define-map waste-tracking principal
  {
    baseline-waste: uint,
    current-period-waste: uint,
    waste-reduction-percentage: uint,
    penalties-incurred: uint,
    rewards-earned: uint,
    tracking-periods: uint
  })

(define-data-var next-request-id uint u1)

;; Read-only functions
(define-read-only (get-participant (participant principal))
  (map-get? participants participant))

(define-read-only (get-inventory-report (participant principal))
  (map-get? inventory-reports participant))

(define-read-only (get-coordination-request (request-id uint))
  (map-get? coordination-requests request-id))

(define-read-only (get-waste-tracking (participant principal))
  (map-get? waste-tracking participant))

(define-read-only (calculate-waste-reduction (participant principal))
  (match (get-waste-tracking participant)
    waste-data
    (if (> (get baseline-waste waste-data) u0)
      (/ (* (- (get baseline-waste waste-data)
               (get current-period-waste waste-data)) u100)
         (get baseline-waste waste-data))
      u0)
    u0))

(define-read-only (get-reputation-score (participant principal))
  (match (get-participant participant)
    participant-data (get reputation-score participant-data)
    u0))

;; Public functions
(define-public (register-participant (name (string-ascii 100)) (participant-type uint)
                                   (location (string-ascii 100)) (waste-target uint))
  (let ((participant-id (+ (var-get total-participants) u1)))
    (asserts! (and (>= participant-type u1) (<= participant-type u4)) ERR-INVALID-INPUT)
    (asserts! (is-none (get-participant tx-sender)) ERR-ALREADY-EXISTS)
    (asserts! (> waste-target u0) ERR-INVALID-INPUT)
    (map-set participants tx-sender
      {
        participant-id: participant-id,
        name: name,
        participant-type: participant-type,
        location: location,
        waste-reduction-target: waste-target,
        actual-waste-reduction: u0,
        reputation-score: u50,
        active: true,
        joined-at: block-height
      })
    (map-set inventory-reports tx-sender
      {
        total-inventory: u0,
        waste-generated: u0,
        waste-prevented: u0,
        surplus-available: u0,
        last-report-period: block-height,
        reports-count: u0
      })
    (map-set waste-tracking tx-sender
      {
        baseline-waste: u0,
        current-period-waste: u0,
        waste-reduction-percentage: u0,
        penalties-incurred: u0,
        rewards-earned: u0,
        tracking-periods: u0
      })
    (var-set total-participants participant-id)
    (ok participant-id)))

(define-public (report-inventory (total-inventory uint) (waste-generated uint)
                                (surplus-available uint))
  (let ((existing-report (unwrap! (get-inventory-report tx-sender) ERR-NOT-FOUND))
        (waste-prevented (if (> total-inventory waste-generated)
                          (- total-inventory waste-generated) u0)))
    (asserts! (>= total-inventory waste-generated) ERR-INVALID-INPUT)
    (map-set inventory-reports tx-sender
      (merge existing-report
        {
          total-inventory: total-inventory,
          waste-generated: waste-generated,
          waste-prevented: waste-prevented,
          surplus-available: surplus-available,
          last-report-period: block-height,
          reports-count: (+ (get reports-count existing-report) u1)
        }))
    (var-set total-waste-prevented (+ (var-get total-waste-prevented) waste-prevented))
    (ok true)))

(define-public (create-coordination-request (request-type uint) (quantity uint)
                                          (product-type (string-ascii 50))
                                          (location (string-ascii 100))
                                          (expiry-timeline uint))
  (let ((request-id (var-get next-request-id)))
    (asserts! (is-some (get-participant tx-sender)) ERR-NOT-FOUND)
    (asserts! (and (>= request-type u1) (<= request-type u3)) ERR-INVALID-INPUT)
    (asserts! (> quantity u0) ERR-INVALID-INPUT)
    (asserts! (> expiry-timeline block-height) ERR-INVALID-INPUT)
    (map-set coordination-requests request-id
      {
        requester: tx-sender,
        request-type: request-type,
        quantity: quantity,
        product-type: product-type,
        location: location,
        expiry-timeline: expiry-timeline,
        fulfilled: false,
        created-at: block-height
      })
    (var-set next-request-id (+ request-id u1))
    (ok request-id)))

(define-public (fulfill-coordination-request (request-id uint))
  (let ((request-data (unwrap! (get-coordination-request request-id) ERR-NOT-FOUND))
        (fulfiller-data (unwrap! (get-participant tx-sender) ERR-NOT-FOUND)))
    (asserts! (not (get fulfilled request-data)) ERR-INVALID-INPUT)
    (asserts! (< block-height (get expiry-timeline request-data)) ERR-INVALID-INPUT)
    (asserts! (not (is-eq tx-sender (get requester request-data))) ERR-INVALID-INPUT)
    (map-set coordination-requests request-id
      (merge request-data { fulfilled: true }))
    ;; Update reputation scores for both parties
    (map-set participants tx-sender
      (merge fulfiller-data
        { reputation-score: (+ (get reputation-score fulfiller-data) u5) }))
    (ok true)))

(define-public (update-waste-tracking (current-waste uint) (baseline-waste uint))
  (let ((existing-tracking (unwrap! (get-waste-tracking tx-sender) ERR-NOT-FOUND))
        (participant-data (unwrap! (get-participant tx-sender) ERR-NOT-FOUND)))
    (let ((reduction-percentage (if (> baseline-waste u0)
                                  (/ (* (- baseline-waste current-waste) u100) baseline-waste)
                                  u0)))
      (map-set waste-tracking tx-sender
        (merge existing-tracking
          {
            baseline-waste: (if (is-eq (get baseline-waste existing-tracking) u0)
                              baseline-waste
                              (get baseline-waste existing-tracking)),
            current-period-waste: current-waste,
            waste-reduction-percentage: reduction-percentage,
            tracking-periods: (+ (get tracking-periods existing-tracking) u1)
          }))
      (map-set participants tx-sender
        (merge participant-data
          { actual-waste-reduction: reduction-percentage }))
      (ok reduction-percentage))))

(define-public (assess-penalties-rewards (participant principal))
  (let ((participant-data (unwrap! (get-participant participant) ERR-NOT-FOUND))
        (tracking-data (unwrap! (get-waste-tracking participant) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (let ((target (get waste-reduction-target participant-data))
          (actual (get actual-waste-reduction participant-data)))
      (if (>= actual target)
        ;; Reward for meeting/exceeding target
        (map-set waste-tracking participant
          (merge tracking-data
            { rewards-earned: (+ (get rewards-earned tracking-data) u100) }))
        ;; Penalty for missing target
        (map-set waste-tracking participant
          (merge tracking-data
            { penalties-incurred: (+ (get penalties-incurred tracking-data) u50) })))
      (ok true))))
