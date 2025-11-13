(define-constant contract-owner tx-sender)
(define-constant late-fee-percentage u5)
(define-constant grace-period-blocks u144)
(define-constant seconds-in-day u86400)
(define-constant dispute-resolution-period u604800)

(define-constant dispute-status-open u0)
(define-constant dispute-status-resolved u1)
(define-constant dispute-status-closed u2)

(define-constant renewal-status-pending u0)
(define-constant renewal-status-approved u1)
(define-constant renewal-status-rejected u2)
(define-constant renewal-eligibility-days u30)

(define-constant maintenance-status-open u0)
(define-constant maintenance-status-completed u1)
(define-constant maintenance-status-cancelled u2)
(define-constant maintenance-priority-low u1)
(define-constant maintenance-priority-medium u2)
(define-constant maintenance-priority-high u3)
(define-constant maintenance-response-time u604800)

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

(define-map renewal-requests
    uint
    {
        tenant: principal,
        landlord: principal,
        current-rent: uint,
        proposed-rent: uint,
        new-duration: uint,
        status: uint,
        requested-at: uint,
        reviewed-at: (optional uint),
    }
)

(define-data-var renewal-counter uint u0)

(define-map maintenance-requests
    uint
    {
        tenant: principal,
        landlord: principal,
        issue-type: (string-ascii 50),
        description: (string-ascii 300),
        priority: uint,
        status: uint,
        escrow-amount: uint,
        created-at: uint,
        completed-at: (optional uint),
    }
)

(define-data-var maintenance-counter uint u0)

(define-map escrow-holdings
    uint
    {
        tenant: principal,
        landlord: principal,
        amount: uint,
        released: bool,
    }
)

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

(define-public (request-renewal
        (proposed-rent uint)
        (new-duration uint)
    )
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tx-sender) (err u800)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (end-date (get end-date rental-info))
            (renewal-id (var-get renewal-counter))
            (eligibility-timestamp (- end-date (* renewal-eligibility-days seconds-in-day)))
        )
        (asserts! (get active rental-info) (err u801))
        (asserts! (>= current-time eligibility-timestamp) (err u802))
        (asserts! (> proposed-rent u0) (err u803))
        (asserts! (> new-duration u0) (err u804))
        (var-set renewal-counter (+ renewal-id u1))
        (map-set renewal-requests renewal-id {
            tenant: tx-sender,
            landlord: (get landlord rental-info),
            current-rent: (get rent-amount rental-info),
            proposed-rent: proposed-rent,
            new-duration: new-duration,
            status: renewal-status-pending,
            requested-at: current-time,
            reviewed-at: none,
        })
        (ok renewal-id)
    )
)

(define-public (review-renewal-request
        (renewal-id uint)
        (approved bool)
        (final-rent uint)
    )
    (let (
            (renewal-info (unwrap! (map-get? renewal-requests renewal-id) (err u900)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (tenant (get tenant renewal-info))
            (rental-info (unwrap! (map-get? rental-agreements tenant) (err u901)))
        )
        (asserts! (is-eq tx-sender (get landlord renewal-info)) (err u902))
        (asserts! (is-eq (get status renewal-info) renewal-status-pending)
            (err u903)
        )
        (asserts! (> final-rent u0) (err u904))
        (if approved
            (begin
                (map-set renewal-requests renewal-id
                    (merge renewal-info {
                        status: renewal-status-approved,
                        reviewed-at: (some current-time),
                        proposed-rent: final-rent,
                    })
                )
                (map-set rental-agreements tenant
                    (merge rental-info {
                        rent-amount: final-rent,
                        end-date: (+ (get end-date rental-info)
                            (* (get new-duration renewal-info) seconds-in-day)
                        ),
                    })
                )
            )
            (map-set renewal-requests renewal-id
                (merge renewal-info {
                    status: renewal-status-rejected,
                    reviewed-at: (some current-time),
                })
            )
        )
        (ok approved)
    )
)

(define-read-only (check-renewal-eligibility (tenant principal))
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tenant) (err u1000)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (end-date (get end-date rental-info))
            (eligibility-timestamp (- end-date (* renewal-eligibility-days seconds-in-day)))
        )
        (ok (and
            (get active rental-info)
            (>= current-time eligibility-timestamp)
            (< current-time end-date)
        ))
    )
)

(define-read-only (get-renewal-request (renewal-id uint))
    (ok (map-get? renewal-requests renewal-id))
)

(define-read-only (get-days-until-renewal-eligible (tenant principal))
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tenant) (err u1100)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (end-date (get end-date rental-info))
            (eligibility-timestamp (- end-date (* renewal-eligibility-days seconds-in-day)))
        )
        (if (>= current-time eligibility-timestamp)
            (ok u0)
            (ok (/ (- eligibility-timestamp current-time) seconds-in-day))
        )
    )
)

(define-public (create-maintenance-request
        (issue-type (string-ascii 50))
        (description (string-ascii 300))
        (priority uint)
        (escrow-amount uint)
    )
    (let (
            (rental-info (unwrap! (map-get? rental-agreements tx-sender) (err u1200)))
            (maintenance-id (var-get maintenance-counter))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
        )
        (asserts! (get active rental-info) (err u1201))
        (asserts!
            (or
                (is-eq priority maintenance-priority-low)
                (or
                    (is-eq priority maintenance-priority-medium)
                    (is-eq priority maintenance-priority-high)
                )
            )
            (err u1202)
        )
        (asserts! (>= escrow-amount u0) (err u1203))
        (if (> escrow-amount u0)
            (begin
                (try! (stx-transfer? escrow-amount tx-sender (as-contract tx-sender)))
                (map-set escrow-holdings maintenance-id {
                    tenant: tx-sender,
                    landlord: (get landlord rental-info),
                    amount: escrow-amount,
                    released: false,
                })
            )
            true
        )
        (var-set maintenance-counter (+ maintenance-id u1))
        (map-set maintenance-requests maintenance-id {
            tenant: tx-sender,
            landlord: (get landlord rental-info),
            issue-type: issue-type,
            description: description,
            priority: priority,
            status: maintenance-status-open,
            escrow-amount: escrow-amount,
            created-at: current-time,
            completed-at: none,
        })
        (ok maintenance-id)
    )
)

(define-public (complete-maintenance-request (maintenance-id uint))
    (let (
            (maintenance-info (unwrap! (map-get? maintenance-requests maintenance-id) (err u1300)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (escrow-amount (get escrow-amount maintenance-info))
        )
        (asserts! (is-eq tx-sender (get landlord maintenance-info)) (err u1301))
        (asserts! (is-eq (get status maintenance-info) maintenance-status-open)
            (err u1302)
        )
        (map-set maintenance-requests maintenance-id
            (merge maintenance-info {
                status: maintenance-status-completed,
                completed-at: (some current-time),
            })
        )
        (if (> escrow-amount u0)
            (begin
                (let ((escrow-info (unwrap! (map-get? escrow-holdings maintenance-id)
                        (err u1303)
                    )))
                    (asserts! (not (get released escrow-info)) (err u1304))
                    (try! (as-contract (stx-transfer? escrow-amount tx-sender
                        (get landlord maintenance-info)
                    )))
                    (map-set escrow-holdings maintenance-id
                        (merge escrow-info { released: true })
                    )
                    (ok true)
                )
            )
            (ok true)
        )
    )
)

(define-public (cancel-maintenance-request (maintenance-id uint))
    (let (
            (maintenance-info (unwrap! (map-get? maintenance-requests maintenance-id) (err u1400)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (escrow-amount (get escrow-amount maintenance-info))
        )
        (asserts! (is-eq tx-sender (get tenant maintenance-info)) (err u1401))
        (asserts! (is-eq (get status maintenance-info) maintenance-status-open)
            (err u1402)
        )
        (map-set maintenance-requests maintenance-id
            (merge maintenance-info {
                status: maintenance-status-cancelled,
                completed-at: (some current-time),
            })
        )
        (if (> escrow-amount u0)
            (begin
                (let ((escrow-info (unwrap! (map-get? escrow-holdings maintenance-id)
                        (err u1403)
                    )))
                    (asserts! (not (get released escrow-info)) (err u1404))
                    (try! (as-contract (stx-transfer? escrow-amount tx-sender
                        (get tenant maintenance-info)
                    )))
                    (map-set escrow-holdings maintenance-id
                        (merge escrow-info { released: true })
                    )
                    (ok true)
                )
            )
            (ok true)
        )
    )
)

(define-public (release-escrow-to-tenant (maintenance-id uint))
    (let (
            (maintenance-info (unwrap! (map-get? maintenance-requests maintenance-id) (err u1500)))
            (current-time (unwrap-panic (get-stacks-block-info? time u0)))
            (escrow-info (unwrap! (map-get? escrow-holdings maintenance-id) (err u1501)))
        )
        (asserts! (is-eq tx-sender contract-owner) (err u1502))
        (asserts! (is-eq (get status maintenance-info) maintenance-status-open)
            (err u1503)
        )
        (asserts!
            (>= (- current-time (get created-at maintenance-info))
                maintenance-response-time
            )
            (err u1504)
        )
        (asserts! (not (get released escrow-info)) (err u1505))
        (try! (as-contract (stx-transfer? (get amount escrow-info) tx-sender
            (get tenant maintenance-info)
        )))
        (map-set escrow-holdings maintenance-id
            (merge escrow-info { released: true })
        )
        (ok true)
    )
)

(define-read-only (get-maintenance-request (maintenance-id uint))
    (ok (map-get? maintenance-requests maintenance-id))
)

(define-read-only (get-escrow-status (maintenance-id uint))
    (ok (map-get? escrow-holdings maintenance-id))
)

(define-read-only (get-total-maintenance-requests)
    (ok (var-get maintenance-counter))
)

(define-map autopay-authorizations
    principal
    {
        landlord: principal,
        max-amount: uint,
        active: bool,
    }
)

(define-map autopay-balances
    principal
    { amount: uint }
)

(define-public (autopay-authorize
        (landlord principal)
        (max-amount uint)
    )
    (begin
        (asserts! (> max-amount u0) (err u7001))
        (map-set autopay-authorizations tx-sender {
            landlord: landlord,
            max-amount: max-amount,
            active: true,
        })
        (ok true)
    )
)

(define-public (autopay-revoke)
    (let ((auth (default-to {
            landlord: tx-sender,
            max-amount: u0,
            active: false,
        }
            (map-get? autopay-authorizations tx-sender)
        )))
        (map-set autopay-authorizations tx-sender (merge auth { active: false }))
        (ok true)
    )
)

(define-public (autopay-fund (amount uint))
    (let ((bal (default-to { amount: u0 } (map-get? autopay-balances tx-sender))))
        (asserts! (> amount u0) (err u7002))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set autopay-balances tx-sender { amount: (+ (get amount bal) amount) })
        (ok true)
    )
)

(define-public (autopay-withdraw (amount uint))
    (let (
            (bal (default-to { amount: u0 } (map-get? autopay-balances tx-sender)))
            (recipient tx-sender)
        )
        (asserts! (> amount u0) (err u7003))
        (asserts! (>= (get amount bal) amount) (err u7004))
        (map-set autopay-balances tx-sender { amount: (- (get amount bal) amount) })
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (ok true)
    )
)

(define-public (autopay-execute
        (tenant principal)
        (amount uint)
    )
    (let (
            (auth (unwrap! (map-get? autopay-authorizations tenant) (err u7005)))
            (bal (default-to { amount: u0 } (map-get? autopay-balances tenant)))
            (recipient tx-sender)
        )
        (asserts! (is-eq recipient (get landlord auth)) (err u7006))
        (asserts! (get active auth) (err u7007))
        (asserts! (> amount u0) (err u7008))
        (asserts! (<= amount (get max-amount auth)) (err u7009))
        (asserts! (>= (get amount bal) amount) (err u7010))
        (map-set autopay-balances tenant { amount: (- (get amount bal) amount) })
        (try! (as-contract (stx-transfer? amount tx-sender recipient)))
        (ok true)
    )
)

(define-read-only (get-autopay-status (tenant principal))
    (ok {
        settings: (map-get? autopay-authorizations tenant),
        balance: (map-get? autopay-balances tenant),
    })
)
