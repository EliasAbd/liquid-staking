;; Liquid Staking Contract with sSTX Token

(define-constant admin 'SP000000000000000000002Q6VF78)

;; Penalty rate in basis points (e.g. 500 = 5%)
(define-data-var withdrawal-penalty-bps uint u500)
;; Minimum staking period in blocks (e.g. 2 years 1054080 blocks at 10s/block)
(define-data-var min-stake-period uint u1054080)

;; Pool accounting
(define-data-var total-staked uint u0)
(define-data-var total-sstx uint u0)

;; Map user staked-amount, sstx-amount, last-stake-block}
(define-map stakes
  { user: principal }
  { staked: uint, sstx: uint, last-block: uint }
)

;; ------- sSTX SIP-010 Token Implementation --------

;; Define the token
(define-fungible-token sstx-token)

;; SIP-010: Standard Fungible Token contract interface
(define-trait sip-010-trait
  (
    ;; SIP-010 required functions
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    ;; Optional mint/burn
    (mint! (uint principal) (response bool uint))
    (burn! (uint principal) (response bool uint))
  )
)


(define-read-only (get-name)
    (ok "Staked STX Token"))

(define-read-only (get-symbol)
    (ok "sSTX"))

(define-read-only (get-decimals)
    (ok u6))

(define-read-only (get-balance (account principal))
    (ok (ft-get-balance sstx-token account)))

(define-read-only (get-total-supply)
    (ok (ft-get-supply sstx-token)))

(define-read-only (get-token-uri)
    (ok none))

(define-public (transfer (amount uint) (sender principal) (recipient principal) (memo (optional (buff 34))))
    (begin
        (asserts! (is-eq tx-sender sender) (err u1))
        (match (ft-transfer? sstx-token amount sender recipient)
            success (begin 
                (print (default-to 0x memo))
                (ok true))
            error (err error))))

(define-public (mint! (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender (as-contract tx-sender)) (err u403))
    (try! (ft-mint? sstx-token amount recipient))
    (ok true)))

(define-public (burn! (amount uint) (owner principal))
    (begin 
        (asserts! (is-eq tx-sender (as-contract tx-sender)) (err u403))
        (match (ft-burn? sstx-token amount owner)
            success (ok true)
            error (err error))))

(define-read-only (get-exchange-rate)
  ;; When no sSTX exists, rate is 1:1
  (let ((ts (var-get total-staked))
        (tsx (var-get total-sstx)))
    (if (or (is-eq ts u0) (is-eq tsx u0))
        (ok u1000000)
        ;; rate = (total_staked / total_sstx) * 1e6 for precision
        (ok (/ (* ts u1000000) tsx))
    )
  )
)

;; ------- Stake Function --------

(define-public (stake)
  (let ((amt (stx-get-balance tx-sender))
        (rate (unwrap! (get-exchange-rate) (err u101))))
    (begin
      (asserts! (> amt u0) (err u100))
      ;; Calculate sSTX to mint
      (let ((sstx-minted 
              (if (is-eq (var-get total-sstx) u0)
                amt  ;; initial 1:1
                (/ (* amt u1000000) rate))))
        ;; Transfer STX into contract
        (try! (stx-transfer? amt tx-sender (as-contract tx-sender)))
        ;; Update pools
        (var-set total-staked (+ (var-get total-staked) amt))
        (var-set total-sstx (+ (var-get total-sstx) sstx-minted))
        ;; Mint sSTX tokens
        (try! (mint! sstx-minted tx-sender))
        ;; Record stake info
        (let ((current-stake (default-to
              { staked: u0, sstx: u0, last-block: burn-block-height }
              (map-get? stakes { user: tx-sender }))))
          (map-set stakes { user: tx-sender }
            { staked: (+ (get staked current-stake) amt)
            , sstx: (+ (get sstx current-stake) sstx-minted)
            , last-block: burn-block-height })
          (ok { staked: amt, sstx: sstx-minted, rate: rate }))))))

;; ------- Redeem Function --------

(define-public (redeem (amount-sstx uint))
  (begin
    (asserts! (> amount-sstx u0) (err u200))
    (let ((stake-info (unwrap! (map-get? stakes { user: tx-sender }) (err u201))))
            (let ((user-sstx (get sstx stake-info))
                  (user-last (get last-block stake-info)))
                (asserts! (>= user-sstx amount-sstx) (err u202))
                (asserts! (>= (- burn-block-height user-last) (var-get min-stake-period)) (err u203))
                (let ((rate (unwrap! (get-exchange-rate) (err u204)))
                      (raw-stx (/ (* amount-sstx rate) u1000000))
                      (penalty (/ (* raw-stx (var-get withdrawal-penalty-bps)) u10000))
                      (stx-out (- raw-stx penalty)))
                    ;; burn users sSTX and update pools
                    (try! (burn! amount-sstx tx-sender))
                    (var-set total-sstx (- (var-get total-sstx) amount-sstx))
                    (var-set total-staked (- (var-get total-staked) raw-stx))
                    ;; transfer STX minus penalty back
                    (try! (stx-transfer? stx-out (as-contract tx-sender) tx-sender))
                    ;; update user map
                    (let ((new-staked (- (get staked stake-info) raw-stx))
                          (new-sstx (- user-sstx amount-sstx)))
                        (if (or (is-eq new-staked u0) (is-eq new-sstx u0))
                            (map-delete stakes { user: tx-sender })
                            (map-set stakes { user: tx-sender }
                                { staked: new-staked, sstx: new-sstx, last-block: user-last }))
                        (ok { redeemed: stx-out, penalty: penalty, raw: raw-stx })))))))

;; ------- Admin Functions --------

(define-public (set-penalty (bps uint))
  (begin
    (asserts! (is-eq tx-sender admin) (err u403))
    (asserts! (<= bps u10000) (err u404))
    (var-set withdrawal-penalty-bps bps)
    (ok bps)
  )
)

(define-public (set-min-stake-period (blocks uint))
  (begin
    (asserts! (is-eq tx-sender admin) (err u403))
    (var-set min-stake-period blocks)
    (ok blocks)
  )
)

;; Allow admin to withdraw penalties accrued (any STX > total-staked)
(define-public (withdraw-penalties (min-amount uint))
  (let ((balance (stx-get-balance (as-contract tx-sender)))
        (ts (var-get total-staked)))
    (asserts! (is-eq tx-sender admin) (err u403))
    (asserts! (> balance ts) (err u405))
    (let ((available (- balance ts)))
      (asserts! (>= available min-amount) (err u406))
      (try! (stx-transfer? available (as-contract tx-sender) admin))
      (ok available))))

