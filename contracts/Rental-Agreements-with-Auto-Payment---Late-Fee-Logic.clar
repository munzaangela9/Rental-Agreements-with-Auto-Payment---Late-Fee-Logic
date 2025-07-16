(define-constant contract-owner tx-sender)
(define-constant late-fee-percentage u5)
(define-constant grace-period-blocks u144)
(define-constant seconds-in-day u86400)
(define-constant dispute-resolution-period u604800)

(define-constant dispute-status-open u0)
(define-constant dispute-status-resolved u1)
(define-constant dispute-status-closed u2)

(define-data-var last-payment-height uint u0)
(define-data-var rental-amount uint u0)
(define-data-var payment-due-date uint u0)

(define-map rental-agreements
    principal
    {
        tenant: principal,
        landlord: principal,
        rent-amount: uint,
        security-deposit: uint,
        start-date: uint,
        end-date: uint,
        last-payment: uint,
        active: bool,
    }
)

(define-map tenant-payments
    principal
    {
        total-paid: uint,
        late-fees-paid: uint,
        payments-made: uint,
    }
)

(define-map rental-disputes
    uint
    {
        tenant: principal,
        landlord: principal,
        dispute-type: (string-ascii 50),
        description: (string-ascii 200),
        amount: uint,
        status: uint,
        created-at: uint,
        resolved-at: (optional uint),
        winner: (optional principal),
    }
)

(define-data-var dispute-counter uint u0)

(define-public (create-rental-agreement
        (tenant principal)
        (rent-amount uint)
        (security-deposit uint)
        (duration uint)
    )
    (let (
            (start-block (get-stacks-block-info? time u0))
            (end-block (+ (unwrap-panic start-block) (* duration seconds-in-day)))
        )
        (asserts! (is-eq tx-sender contract-owner) (err u100))
        (asserts! (> rent-amount u0) (err u101))
        (asserts! (> security-deposit u0) (err u102))
        (ok (map-set rental-agreements tenant {
            tenant: tenant,
            landlord: tx-sender,
            rent-amount: rent-amount,
            security-deposit: security-deposit,
            start-date: (unwrap-panic start-block),
            end-date: end-block,
            last-payment: (unwrap-panic start-block),
            active: true,
        }))
    )
)

(define-public (pay-rent (amount uint))
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tx-sender) (err u200)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (late-fee (calculate-late-fee tx-sender))
            (total-due (+ amount late-fee))
        )
        (asserts! (get active rental-info) (err u201))
        (asserts! (>= amount (get rent-amount rental-info)) (err u202))
        (try! (stx-transfer? total-due tx-sender (get landlord rental-info)))
        (update-payment-records tx-sender amount late-fee)
        (ok true)
    )
)

(define-private (calculate-late-fee (tenant principal))
    (let (
            (rental-info (unwrap-panic (map-get? rental-agreements tenant)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (last-payment (get last-payment rental-info))
            (rent-amount (get rent-amount rental-info))
        )
        (if (> (- current-time last-payment) (* seconds-in-day u30))
            (/ (* rent-amount late-fee-percentage) u100)
            u0
        )
    )
)

(define-private (update-payment-records
        (tenant principal)
        (amount uint)
        (late-fee uint)
    )
    (let ((current-records (default-to {
            total-paid: u0,
            late-fees-paid: u0,
            payments-made: u0,
        }
            (map-get? tenant-payments tenant)
        )))
        (map-set tenant-payments tenant {
            total-paid: (+ (get total-paid current-records) amount),
            late-fees-paid: (+ (get late-fees-paid current-records) late-fee),
            payments-made: (+ (get payments-made current-records) u1),
        })
        (map-set rental-agreements tenant
            (merge (unwrap-panic (map-get? rental-agreements tenant)) { last-payment: (unwrap-panic (get-stacks-block-info? time u0)) })
        )
    )
)

(define-public (terminate-agreement (tenant principal))
    (let ((rental-info (unwrap! (map-get? rental-agreements tenant) (err u300))))
        (asserts! (is-eq tx-sender (get landlord rental-info)) (err u301))
        (asserts! (get active rental-info) (err u302))
        (map-set rental-agreements tenant (merge rental-info { active: false }))
        (try! (stx-transfer? (get security-deposit rental-info) tx-sender tenant))
        (ok true)
    )
)

(define-read-only (get-rental-info (tenant principal))
    (ok (map-get? rental-agreements tenant))
)

(define-read-only (get-payment-history (tenant principal))
    (ok (map-get? tenant-payments tenant))
)

(define-read-only (get-late-fee (tenant principal))
    (ok (calculate-late-fee tenant))
)

(define-public (update-rent-amount
        (tenant principal)
        (new-amount uint)
    )
    (let ((rental-info (unwrap! (map-get? rental-agreements tenant) (err u400))))
        (asserts! (is-eq tx-sender (get landlord rental-info)) (err u401))
        (asserts! (> new-amount u0) (err u402))
        (map-set rental-agreements tenant
            (merge rental-info { rent-amount: new-amount })
        )
        (ok true)
    )
)

(define-public (create-dispute
        (tenant principal)
        (dispute-type (string-ascii 50))
        (description (string-ascii 200))
        (amount uint)
    )
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tenant) (err u500)))
            (dispute-id (var-get dispute-counter))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
        )
        (asserts!
            (or (is-eq tx-sender tenant) (is-eq tx-sender (get landlord rental-info)))
            (err u501)
        )
        (asserts! (get active rental-info) (err u502))
        (asserts! (> amount u0) (err u503))
        (var-set dispute-counter (+ dispute-id u1))
        (map-set rental-disputes dispute-id {
            tenant: tenant,
            landlord: (get landlord rental-info),
            dispute-type: dispute-type,
            description: description,
            amount: amount,
            status: dispute-status-open,
            created-at: current-time,
            resolved-at: none,
            winner: none,
        })
        (ok dispute-id)
    )
)

(define-public (resolve-dispute
        (dispute-id uint)
        (winner principal)
    )
    (let (
            (dispute-info (unwrap! (map-get? rental-disputes dispute-id) (err u600)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (dispute-amount (get amount dispute-info))
            (tenant (get tenant dispute-info))
            (landlord (get landlord dispute-info))
        )
        (asserts! (is-eq tx-sender contract-owner) (err u601))
        (asserts! (is-eq (get status dispute-info) dispute-status-open)
            (err u602)
        )
        (asserts! (or (is-eq winner tenant) (is-eq winner landlord)) (err u603))
        (asserts!
            (< (- current-time (get created-at dispute-info))
                dispute-resolution-period
            )
            (err u604)
        )
        (map-set rental-disputes dispute-id
            (merge dispute-info {
                status: dispute-status-resolved,
                resolved-at: (some current-time),
                winner: (some winner),
            })
        )
        (if (is-eq winner tenant)
            (try! (stx-transfer? dispute-amount landlord tenant))
            (try! (stx-transfer? dispute-amount tenant landlord))
        )
        (ok true)
    )
)

(define-public (close-expired-dispute (dispute-id uint))
    (let (
            (dispute-info (unwrap! (map-get? rental-disputes dispute-id) (err u700)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
        )
        (asserts! (is-eq (get status dispute-info) dispute-status-open)
            (err u701)
        )
        (asserts!
            (>= (- current-time (get created-at dispute-info))
                dispute-resolution-period
            )
            (err u702)
        )
        (map-set rental-disputes dispute-id
            (merge dispute-info {
                status: dispute-status-closed,
                resolved-at: (some current-time),
            })
        )
        (ok true)
    )
)

(define-read-only (get-dispute-info (dispute-id uint))
    (ok (map-get? rental-disputes dispute-id))
)

(define-read-only (get-active-disputes-count)
    (ok (var-get dispute-counter))
)
