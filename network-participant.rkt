#lang racket

(require racket/tcp)
(require json)

;network-participant
;  a basic implementation of a node participating in a single network
;  take in new events on a port, send to network
;  when receive events, print them out to output port
;  follow pre-defined network procedure

(provide
 (contract-out
  [participant:make (-> natural-number/c input-port? output-port? port-number? participant?)]
  [participant:run (-> participant? void?)]))

(struct participant [id input-port output-port listening-port] #:transparent)

(define (log s p)
  (with-output-to-file (string-append (number->string (participant-id p)) "-log.txt")
    (λ () (println s))
    #:exists 'append))

(define (participant:make id input-port output-port listening-port)
  (with-output-to-file (string-append (number->string id) ".txt")
    (λ () (println (current-seconds)))
    #:exists 'replace)
  (write "starting up" output-port)
  (participant id input-port output-port listening-port))

(define (participant:run partic)
  (parameterize ([current-custodian (make-custodian)])
      (define state-channel (make-channel))
      (sync
       (thread (thunk (state-thread partic state-channel)))
       (thread (thunk (listening-thread partic state-channel)))
       (thread (thunk (input-thread partic state-channel))))
    (log "shutting-down" partic)
    (custodian-shutdown-all (current-custodian))))
    

(define (state-thread partic state-ch)
  (let state-loop ([node-state (node-state:make)])
    (define action (channel-get state-ch))
    (define new-state (action-apply action node-state))
    (state-loop new-state)))

#;((
    (cond
      [(message? action) (begin (write "AHHHHHH" (participant-output-port partic))
                                (flush-output (participant-output-port partic)))])
    (state-loop)))

(define (input-thread partic state-channel)
  (let input-loop ()
    (define input-value (read-json (participant-input-port partic)))
    (write "found a message!" (participant-output-port partic))
    (write input-value (participant-output-port partic))
    (cond
      [(input-action:send-message? input-value) (send-message partic input-value)])))

(define (listening-thread partic state-ch)
  (log "listening" partic)
  (parameterize ([current-custodian (make-custodian)])
    (call-with-exception-handler
     (λ (e) (begin
              (custodian-shutdown-all (current-custodian))
              (displayln (e))))
     (thunk
      (define listener (tcp-listen (participant-listening-port partic)))
      (let listening-loop ()
        (log "waiting" partic)
        (define-values (in out) (tcp-accept listener))
        (define recv-value (read-json in))
        (log (string-append "received: " (network-message->string recv-value)) partic)
        (if (valid-message? recv-value)
            (channel-put state-ch (network-message:make recv-value out))
            #f)
        (listening-loop))))
    (custodian-shutdown-all (current-custodian))))

#;
(define (message? inp)
  (and
   (hash? inp)
   (hash-has-key? inp 'id)
   (hash-has-key? inp 'msg)))

#;
(define (network-message->string v)
  (string-append "id:" (number->string (hash-ref v 'id)) " " "msg:" (hash-ref v 'msg)))

#;
(define (send-message partic input-message)
  (define msg-to-be-sent (input-message->network-message partic input-message))
  (log (string-append "sending: " ))
  (define-values (in out) (tcp-connect "127.0.0.1" (hash-ref input-message 'addr)))
  (write-json msg-to-be-sent out)
  (flush-output out)
  (close-input-port in)
  (close-output-port out))

#;
(define (valid-message? v)
  #t)


