;; Decentralized Dating Platform - Core Contract
;; Privacy-focused dating service with user-controlled data and matching

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_INSUFFICIENT_FUNDS (err u104))
(define-constant ERR_PRIVACY_VIOLATION (err u105))
(define-constant ERR_PROFILE_INCOMPLETE (err u106))

;; Profile privacy levels
(define-constant PRIVACY_PUBLIC u0)
(define-constant PRIVACY_SEMI_PRIVATE u1)
(define-constant PRIVACY_PRIVATE u2)

;; Minimum reputation for messaging
(define-constant MIN_REPUTATION_FOR_MESSAGING u50)

;; Data Maps
(define-map user-profiles
  principal
  {
    display-name: (string-ascii 50),
    age: uint,
    location-hash: (buff 32), ;; Hashed location for privacy
    interests-hash: (buff 32), ;; Hashed interests
    bio: (string-utf8 500),
    privacy-level: uint,
    active: bool,
    created-at: uint,
    last-active: uint
  }
)

(define-map user-preferences
  principal
  {
    min-age: uint,
    max-age: uint,
    max-distance: uint, ;; In kilometers
    preferred-interests: (list 10 (buff 32)), ;; Hashed interest categories
    blocked-users: (list 100 principal)
  }
)

(define-map user-reputation
  principal
  {
    score: uint,
    positive-interactions: uint,
    negative-interactions: uint,
    verified: bool,
    verification-stake: uint
  }
)

(define-map matches
  {requester: principal, target: principal}
  {
    status: (string-ascii 20), ;; "pending", "accepted", "rejected", "expired"
    created-at: uint,
    expires-at: uint,
    compatibility-score: uint
  }
)

(define-map conversations
  {user1: principal, user2: principal}
  {
    initiated-by: principal,
    started-at: uint,
    last-message-at: uint,
    message-count: uint,
    active: bool
  }
)

;; Data Variables
(define-data-var platform-fee uint u1000000) ;; 1 STX in microSTX
(define-data-var verification-stake uint u5000000) ;; 5 STX for verification
(define-data-var match-expiry-blocks uint u1008) ;; ~1 week
(define-data-var total-users uint u0)

;; Public Functions

;; Create user profile
(define-public (create-profile
  (display-name (string-ascii 50))
  (age uint)
  (location-hash (buff 32))
  (interests-hash (buff 32))
  (bio (string-utf8 500))
  (privacy-level uint)
)
  (let
    (
      (user tx-sender)
      (current-block burn-block-height)
    )
    (asserts! (is-none (map-get? user-profiles user)) ERR_ALREADY_EXISTS)
    (asserts! (and (>= age u18) (<= age u100)) ERR_INVALID_INPUT)
    (asserts! (<= privacy-level PRIVACY_PRIVATE) ERR_INVALID_INPUT)
    (asserts! (> (len display-name) u0) ERR_INVALID_INPUT)

    (try! (stx-transfer? (var-get platform-fee) user CONTRACT_OWNER))

    (map-set user-profiles user {
      display-name: display-name,
      age: age,
      location-hash: location-hash,
      interests-hash: interests-hash,
      bio: bio,
      privacy-level: privacy-level,
      active: true,
      created-at: current-block,
      last-active: current-block
    })

    (map-set user-reputation user {
      score: u100, ;; Starting reputation
      positive-interactions: u0,
      negative-interactions: u0,
      verified: false,
      verification-stake: u0
    })

    (var-set total-users (+ (var-get total-users) u1))
    (ok true)
  )
)

;; Update user profile
(define-public (update-profile
  (display-name (optional (string-ascii 50)))
  (bio (optional (string-utf8 500)))
  (privacy-level (optional uint))
)
  (let
    (
      (user tx-sender)
      (current-profile (unwrap! (map-get? user-profiles user) ERR_NOT_FOUND))
    )
    (map-set user-profiles user (merge current-profile {
      display-name: (default-to (get display-name current-profile) display-name),
      bio: (default-to (get bio current-profile) bio),
      privacy-level: (default-to (get privacy-level current-profile) privacy-level),
      last-active: burn-block-height
    }))
    (ok true)
  )
)

;; Set user preferences
(define-public (set-preferences
  (min-age uint)
  (max-age uint)
  (max-distance uint)
  (preferred-interests (list 10 (buff 32)))
)
  (let ((user tx-sender))
    (asserts! (is-some (map-get? user-profiles user)) ERR_NOT_FOUND)
    (asserts! (and (>= min-age u18) (<= max-age u100) (<= min-age max-age)) ERR_INVALID_INPUT)

    (map-set user-preferences user {
      min-age: min-age,
      max-age: max-age,
      max-distance: max-distance,
      preferred-interests: preferred-interests,
      blocked-users: (default-to (list) (get blocked-users (map-get? user-preferences user)))
    })
    (ok true)
  )
)

;; Request match with another user
(define-public (request-match (target principal))
  (let
    (
      (requester tx-sender)
      (current-block burn-block-height)
      (requester-profile (unwrap! (map-get? user-profiles requester) ERR_NOT_FOUND))
      (target-profile (unwrap! (map-get? user-profiles target) ERR_NOT_FOUND))
      (requester-rep (unwrap! (map-get? user-reputation requester) ERR_NOT_FOUND))
      (match-key {requester: requester, target: target})
      (reverse-match-key {requester: target, target: requester})
    )

    (asserts! (not (is-eq requester target)) ERR_INVALID_INPUT)
    (asserts! (get active requester-profile) ERR_INVALID_INPUT)
    (asserts! (get active target-profile) ERR_INVALID_INPUT)
    (asserts! (>= (get score requester-rep) MIN_REPUTATION_FOR_MESSAGING) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? matches match-key)) ERR_ALREADY_EXISTS)

    ;; Check if target already requested match with requester
    (match (map-get? matches reverse-match-key)
      existing-match
      (begin
        ;; Mutual match - create conversation
        (map-set matches reverse-match-key (merge existing-match {status: "accepted"}))
        (map-set conversations {user1: requester, user2: target} {
          initiated-by: requester,
          started-at: current-block,
          last-message-at: current-block,
          message-count: u0,
          active: true
        })
        (ok "mutual-match")
      )
      ;; Create new match request
      (begin
        (map-set matches match-key {
          status: "pending",
          created-at: current-block,
          expires-at: (+ current-block (var-get match-expiry-blocks)),
          compatibility-score: (calculate-compatibility requester target)
        })
        (ok "match-requested")
      )
    )
  )
)

;; Accept or reject match
(define-public (respond-to-match (requester principal) (accept bool))
  (let
    (
      (target tx-sender)
      (match-key {requester: requester, target: target})
      (match-data (unwrap! (map-get? matches match-key) ERR_NOT_FOUND))
    )
    (asserts! (is-eq (get status match-data) "pending") ERR_INVALID_INPUT)
    (asserts! (< burn-block-height (get expires-at match-data)) ERR_INVALID_INPUT)

    (if accept
      (begin
        (map-set matches match-key (merge match-data {status: "accepted"}))
        (map-set conversations {user1: requester, user2: target} {
          initiated-by: target,
          started-at: burn-block-height,
          last-message-at: burn-block-height,
          message-count: u0,
          active: true
        })
        (ok "match-accepted")
      )
      (begin
        (map-set matches match-key (merge match-data {status: "rejected"}))
        (ok "match-rejected")
      )
    )
  )
)

;; Verify user profile by staking STX
(define-public (verify-profile)
  (let
    (
      (user tx-sender)
      (current-rep (unwrap! (map-get? user-reputation user) ERR_NOT_FOUND))
      (stake-amount (var-get verification-stake))
    )
    (asserts! (not (get verified current-rep)) ERR_ALREADY_EXISTS)
    (try! (stx-transfer? stake-amount user (as-contract tx-sender)))

    (map-set user-reputation user (merge current-rep {
      verified: true,
      verification-stake: stake-amount,
      score: (+ (get score current-rep) u50) ;; Bonus for verification
    }))
    (ok true)
  )
)

;; Rate interaction (positive or negative)
(define-public (rate-interaction (other-user principal) (positive bool))
  (let
    (
      (rater tx-sender)
      (current-rep (unwrap! (map-get? user-reputation other-user) ERR_NOT_FOUND))
      (conversation-exists (or
        (is-some (map-get? conversations {user1: rater, user2: other-user}))
        (is-some (map-get? conversations {user1: other-user, user2: rater}))
      ))
    )

    (asserts! conversation-exists ERR_UNAUTHORIZED)
    (asserts! (not (is-eq rater other-user)) ERR_INVALID_INPUT)

    (if positive
      (map-set user-reputation other-user (merge current-rep {
        score: (+ (get score current-rep) u10),
        positive-interactions: (+ (get positive-interactions current-rep) u1)
      }))
      (map-set user-reputation other-user (merge current-rep {
        score: (if (> (get score current-rep) u10) (- (get score current-rep) u10) u0),
        negative-interactions: (+ (get negative-interactions current-rep) u1)
      }))
    )
    (ok true)
  )
)

;; Block user
(define-public (block-user (user-to-block principal))
  (let
    (
      (blocker tx-sender)
      (current-prefs (default-to
        {min-age: u18, max-age: u65, max-distance: u50, preferred-interests: (list), blocked-users: (list)}
        (map-get? user-preferences blocker)
      ))
      (current-blocked (get blocked-users current-prefs))
    )

    (asserts! (not (is-eq blocker user-to-block)) ERR_INVALID_INPUT)
    (asserts! (< (len current-blocked) u100) ERR_INVALID_INPUT)

    (map-set user-preferences blocker (merge current-prefs {
      blocked-users: (unwrap! (as-max-len? (append current-blocked user-to-block) u100) ERR_INVALID_INPUT)
    }))
    (ok true)
  )
)

;; Private Functions

;; Calculate compatibility score between two users
(define-private (calculate-compatibility (user1 principal) (user2 principal))
  (let
    (
      (profile1 (unwrap-panic (map-get? user-profiles user1)))
      (profile2 (unwrap-panic (map-get? user-profiles user2)))
      (prefs1 (map-get? user-preferences user1))
      (prefs2 (map-get? user-preferences user2))
    )
    ;; Simplified compatibility calculation
    ;; In production, this would include more sophisticated matching algorithms
    (+
      ;; Age compatibility (basic check)
      (if (and
        (is-some prefs1)
        (>= (get age profile2) (get min-age (unwrap-panic prefs1)))
        (<= (get age profile2) (get max-age (unwrap-panic prefs1)))
      ) u25 u0)

      ;; Interest similarity (simplified - would need proper hash comparison)
      (if (is-eq (get interests-hash profile1) (get interests-hash profile2)) u25 u10)

      ;; Base compatibility
      u50
    )
  )
)

;; Read-only Functions

;; Get user profile (respects privacy settings)
(define-read-only (get-user-profile (user principal))
  (match (map-get? user-profiles user)
    profile
    (let ((privacy-level (get privacy-level profile)))
      (if (is-eq privacy-level PRIVACY_PRIVATE)
        (some {
          display-name: (get display-name profile),
          age: (get age profile),
          active: (get active profile),
          privacy-level: privacy-level
        })
        (some profile)
      )
    )
    none
  )
)

;; Get user reputation
(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation user)
)

;; Get match status
(define-read-only (get-match-status (requester principal) (target principal))
  (map-get? matches {requester: requester, target: target})
)

;; Get conversation info
(define-read-only (get-conversation (user1 principal) (user2 principal))
  (match (map-get? conversations {user1: user1, user2: user2})
    conversation (some conversation)
    (map-get? conversations {user1: user2, user2: user1})
  )
)

;; Check if users can message each other
(define-read-only (can-message (sender principal) (recipient principal))
  (let
    (
      (conversation (get-conversation sender recipient))
      (sender-rep (map-get? user-reputation sender))
    )
    (and
      (is-some conversation)
      (get active (unwrap-panic conversation))
      (is-some sender-rep)
      (>= (get score (unwrap-panic sender-rep)) MIN_REPUTATION_FOR_MESSAGING)
    )
  )
)

;; Get platform statistics
(define-read-only (get-platform-stats)
  {
    total-users: (var-get total-users),
    platform-fee: (var-get platform-fee),
    verification-stake: (var-get verification-stake),
    min-reputation-messaging: MIN_REPUTATION_FOR_MESSAGING
  }
)
