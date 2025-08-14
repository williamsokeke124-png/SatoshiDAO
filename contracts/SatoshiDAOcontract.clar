;; title: SatoshiDAO
;; version: 1.0.0
;; summary: Bitcoin-backed DAO with BTC-based voting system
;; description: A decentralized autonomous organization where voting power is proportional to locked BTC.
;;              Users can lock BTC to receive voting tokens, create proposals, vote on governance decisions,
;;              and manage a shared treasury. BTC is automatically unlocked after voting periods.

;; traits
(define-trait governance-token-trait
  (
    (transfer (uint principal principal (optional (buff 34))) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-total-supply () (response uint uint))
  )
)

;; token definitions
(define-fungible-token satoshi-vote-token)

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-AUTHORIZED (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-INSUFFICIENT-BALANCE (err u103))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u104))
(define-constant ERR-PROPOSAL-EXPIRED (err u105))
(define-constant ERR-ALREADY-VOTED (err u106))
(define-constant ERR-VOTING-PERIOD-ACTIVE (err u107))
(define-constant ERR-VOTING-PERIOD-ENDED (err u108))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u109))
(define-constant ERR-BTC-LOCKED (err u110))
(define-constant ERR-UNLOCK-NOT-READY (err u111))

(define-constant MIN-VOTING-PERIOD u1008) ;; ~1 week in blocks
(define-constant MAX-VOTING-PERIOD u4032) ;; ~4 weeks in blocks
(define-constant MIN-PROPOSAL-THRESHOLD u1000000) ;; 0.01 BTC in sats
(define-constant VOTE-TOKEN-MULTIPLIER u1) ;; 1:1 ratio BTC to vote tokens

;; data vars
(define-data-var proposal-count uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var contract-active bool true)
(define-data-var emergency-pause bool false)

;; data maps
(define-map btc-locks
  { user: principal }
  {
    amount: uint,
    lock-height: uint,
    unlock-height: uint,
    active: bool
  }
)

(define-map proposals
  { id: uint }
  {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-utf8 500),
    voting-start: uint,
    voting-end: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    cancelled: bool,
    treasury-amount: uint,
    recipient: (optional principal)
  }
)

(define-map votes
  { proposal-id: uint, voter: principal }
  {
    vote: bool, ;; true for yes, false for no
    voting-power: uint,
    block-height: uint
  }
)

(define-map user-voting-power
  { user: principal, proposal-id: uint }
  { power: uint }
)


;; public functions

;; Lock BTC and receive voting tokens
(define-public (lock-btc (amount uint) (lock-duration uint))
  (let (
    (sender tx-sender)
    (current-height block-height)
    (unlock-height (+ current-height lock-duration))
    (existing-lock (map-get? btc-locks { user: sender }))
  )
    (asserts! (get contract-active (var-get contract-active)) (err u200))
    (asserts! (not (var-get emergency-pause)) (err u201))
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (>= lock-duration MIN-VOTING-PERIOD) (err u202))
    (asserts! (is-none existing-lock) ERR-BTC-LOCKED)
    
    ;; In a real implementation, this would interact with Bitcoin network
    ;; For now, we simulate BTC transfer
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    ;; Mint voting tokens
    (try! (ft-mint? satoshi-vote-token (* amount VOTE-TOKEN-MULTIPLIER) sender))
    
    ;; Record the BTC lock
    (map-set btc-locks
      { user: sender }
      {
        amount: amount,
        lock-height: current-height,
        unlock-height: unlock-height,
        active: true
      }
    )
    
    (ok true)
  )
)