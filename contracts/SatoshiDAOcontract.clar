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


;; Unlock BTC after lock period expires
(define-public (unlock-btc)
  (let (
    (sender tx-sender)
    (lock-info (unwrap! (map-get? btc-locks { user: sender }) ERR-NOT-AUTHORIZED))
  )
    (asserts! (get active lock-info) ERR-NOT-AUTHORIZED)
    (asserts! (>= block-height (get unlock-height lock-info)) ERR-UNLOCK-NOT-READY)
    
    ;; Burn voting tokens
    (try! (ft-burn? satoshi-vote-token (* (get amount lock-info) VOTE-TOKEN-MULTIPLIER) sender))
    
    ;; Return BTC (STX in this simulation)
    (try! (as-contract (stx-transfer? (get amount lock-info) tx-sender sender)))
    
    ;; Mark lock as inactive
    (map-set btc-locks
      { user: sender }
      {
        amount: (get amount lock-info),
        lock-height: (get lock-height lock-info),
        unlock-height: (get unlock-height lock-info),
        active: false
      }
    )
    
    (ok true)
  )
)

;; Create a new proposal
(define-public (create-proposal (title (string-ascii 100)) (description (string-utf8 500)) 
                               (voting-duration uint) (treasury-amount uint) (recipient (optional principal)))
  (let (
    (sender tx-sender)
    (proposal-id (+ (var-get proposal-count) u1))
    (current-height block-height)
    (voting-start (+ current-height u144)) ;; ~24 hours delay
    (voting-end (+ voting-start voting-duration))
    (sender-balance (ft-get-balance satoshi-vote-token sender))
  )
    (asserts! (get contract-active (var-get contract-active)) (err u200))
    (asserts! (not (var-get emergency-pause)) (err u201))
    (asserts! (>= sender-balance MIN-PROPOSAL-THRESHOLD) ERR-INSUFFICIENT-VOTING-POWER)
    (asserts! (>= voting-duration MIN-VOTING-PERIOD) (err u203))
    (asserts! (<= voting-duration MAX-VOTING-PERIOD) (err u204))
    (asserts! (<= treasury-amount (var-get treasury-balance)) (err u205))
    
    (map-set proposals
      { id: proposal-id }
      {
        proposer: sender,
        title: title,
        description: description,
        voting-start: voting-start,
        voting-end: voting-end,
        yes-votes: u0,
        no-votes: u0,
        executed: false,
        cancelled: false,
        treasury-amount: treasury-amount,
        recipient: recipient
      }
    )
    
    (var-set proposal-count proposal-id)
    (ok proposal-id)
  )
)

;; Vote on a proposal
(define-public (vote (proposal-id uint) (support bool))
  (let (
    (sender tx-sender)
    (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (current-height block-height)
    (voting-power (ft-get-balance satoshi-vote-token sender))
    (existing-vote (map-get? votes { proposal-id: proposal-id, voter: sender }))
  )
    (asserts! (get contract-active (var-get contract-active)) (err u200))
    (asserts! (not (var-get emergency-pause)) (err u201))
    (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
    (asserts! (>= current-height (get voting-start proposal)) (err u206))
    (asserts! (< current-height (get voting-end proposal)) ERR-PROPOSAL-EXPIRED)
    (asserts! (> voting-power u0) ERR-INSUFFICIENT-VOTING-POWER)
    (asserts! (not (get executed proposal)) (err u207))
    (asserts! (not (get cancelled proposal)) (err u208))
    
    ;; Record the vote
    (map-set votes
      { proposal-id: proposal-id, voter: sender }
      {
        vote: support,
        voting-power: voting-power,
        block-height: current-height
      }
    )
    
    ;; Update proposal vote counts
    (map-set proposals
      { id: proposal-id }
      (merge proposal {
        yes-votes: (if support 
                     (+ (get yes-votes proposal) voting-power)
                     (get yes-votes proposal)),
        no-votes: (if support 
                    (get no-votes proposal)
                    (+ (get no-votes proposal) voting-power))
      })
    )
    
    (ok true)
  )
)

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
  (let (
    (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
    (current-height block-height)
    (yes-votes (get yes-votes proposal))
    (no-votes (get no-votes proposal))
    (total-votes (+ yes-votes no-votes))
  )
    (asserts! (get contract-active (var-get contract-active)) (err u200))
    (asserts! (not (var-get emergency-pause)) (err u201))
    (asserts! (>= current-height (get voting-end proposal)) ERR-VOTING-PERIOD-ACTIVE)
    (asserts! (not (get executed proposal)) (err u209))
    (asserts! (not (get cancelled proposal)) (err u208))
    (asserts! (> yes-votes no-votes) (err u210))
    (asserts! (> total-votes u0) (err u211))
    
    ;; Execute treasury transfer if specified
    (if (> (get treasury-amount proposal) u0)
      (match (get recipient proposal)
        recipient-addr (try! (as-contract (stx-transfer? (get treasury-amount proposal) tx-sender recipient-addr)))
        (err u212)
      )
      true
    )
    
    ;; Update treasury balance
    (var-set treasury-balance (- (var-get treasury-balance) (get treasury-amount proposal)))
    
    ;; Mark proposal as executed
    (map-set proposals
      { id: proposal-id }
      (merge proposal { executed: true })
    )
    
    (ok true)
  )
)

;; Add funds to treasury (only owner)
(define-public (add-to-treasury (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (var-set treasury-balance (+ (var-get treasury-balance) amount))
    (ok true)
  )
)

;; Emergency pause (only owner)
(define-public (toggle-emergency-pause)
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (var-set emergency-pause (not (var-get emergency-pause)))
    (ok (var-get emergency-pause))
  )
)
