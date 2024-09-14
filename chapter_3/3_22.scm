;; By searching "start-adventure" and inspecting that func with "*.rkt,*.scm" in VSCode with "chapter_3/*,sdf/" "files to exclude", no sample implementation.
(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)
(load "section-3-5-lib/adventure-lib.scm")
;; > Extend the adventure game so that there can be multiple players, each controlling a personal avatar.
;; IGNORE: IMHO just give each avatar index and change the related funcs to be compatible with that change (the latter implies "a pretty big project" but that is not difficult IMHO).
(define avatars)
(define (start-adventure my-name . names)
  (set! the-clock (make-clock))
  (set! all-places (create-mit))
  (set! heaven (create-place 'heaven))
  (set! all-people (create-people all-places))
  (set! my-avatar
    (create-avatar my-name (random-choice all-places)))
  (set! avatars
    (cons my-avatar (create-avatars names all-places)))
  (whats-here))

(load "../common-lib/utils.scm")
(define (do-for-all-avatars handler)
  (for-each
    (lambda (avatar idx) 
      (displayln (list idx "th avatar"))
      (handler avatar))
    avatars
    (iota (length avatars))))
(define (whats-here)
  ;; trivially all avatar? will do `(add-thing! (get-location thing) thing)`, so all of them can be seen by others nearby.
  ;; Other behaviors also work.
  (do-for-all-avatars look-around)
  unspecific)

(define (create-avatars names places)
  (map
    (lambda (name) 
      (make-avatar 'name name
                   'location (random-choice places)
                   'screen (make-screen 'name 'the-screen)))
    names))

;; I only give one demo of one interface
;; 's avatar-idx is 0.
; (define (go direction #!optional avatar-idx)
;   (define (go-mod direction avatar-idx)
;     (let ((my-avatar (list-ref avatars avatar-idx)))
;       ;; original code
;       (let ((exit
;               (find-exit-in-direction direction
;                                       (get-location my-avatar))))
;         (if exit
;           (take-exit! exit my-avatar)
;           (narrate! (list "No exit in" direction "direction")
;                     my-avatar)))
;       unspecific))
;   (go-mod direction (if (default-object? avatar-idx) 0 avatar-idx)))

(start-adventure 'anonymous 'foo 'bar)
(do-for-all-avatars move-somewhere!)

;; > multiple players, each controlling a personal avatar.
(define cur-player-idx 0)
(define (next-player)
  (let ((play-cnt (length avatars)))
    (displayln "next player")
    (set! cur-player-idx (modulo (n:+ cur-player-idx 1) play-cnt))
    (set! my-avatar (list-ref avatars cur-player-idx))))
(do ((i 0 (+ i 1)))
  ((= i 5) 'done)
  (next-player)
  (move-somewhere! my-avatar))

;; > Make it possible for players to be on different terminals.
;; IMHO this needs communication between different scheme instance and different envs.
;; I won't dig into that since it depends on communication lib 
;; (for network, I learnt in csapp with some unix libs. That are all conventional things where we need to conform to something to make communicatiion work).
