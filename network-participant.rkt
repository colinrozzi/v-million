#lang racket

(require racket/tcp)
(require json)
(require "node-state.rkt")
(require "action.rkt")

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
    ;(define state-channel (make-channel))
    
    (define action-ch (make-channel))
    (define data-ch (make-channel))
    (define executor-ch (make-channel))
    
    (sync
     (thread (thunk (executor-thread  partic task-ch executor-ch)))
     (thread (thunk (state-thread     partic tack-ch)))
     (thread (thunk (listening-thread partic executor-ch)))
     (thread (thunk (input-thread     partic executor-ch))))
    
    (log "shutting-down" partic)
    (custodian-shutdown-all (current-custodian))))

(struct executor-request [actions response-channel])

(define (executor-thread partic task-ch executor-ch)
  (let executor-loop ()
    ; lists of actions come in on executor channel
    ; executor goes over all requests, collects the responses, and sends back when all are completed
    (define executor-request (channel-get executor-ch))
    (define results (for/list ([action (executor-request-actions executor-request)])
                      (let* ([response-channel (make-channel)]
                             [task (task:make action response-channel)])
                        (channel-put task-ch task)
                        (channel-get response-channel))))
    (channel-put (executor-request-response-channel executor-request) results)
    (executor-loop)))

(struct task [action response-channel])
(define (task:make action response-channel)
  (task action response-channel))

(define (state-thread partic task-ch)
  ;turn this into a sync to handle both action and data requests
  ;changed my mind-an action can both modify the state and return a result
  (let state-loop ([node-state (node-state:make)])
    (define task (channel-get task-ch))
    (define-values (new-state result) (action:apply (task-action task) node-state))
    (channel-put (task-response-channel task) response)
    (state-loop new-state)))
  

(define (input-thread partic executor-ch)
  (let input-loop ()
    (define input-value (read-json (participant-input-port partic)))
    
    ;--- printing
    (write "found a message!" (participant-output-port partic))
    (write input-value (participant-output-port partic))
    ;--- printing

    (cond
      [(input-action? input-value) (channel-put executor-ch (input->action-list input-value partic))]
      [else #f])
    (input-loop)))


(define (listening-thread partic executor-ch)
  (log "starting-listening-thread" partic)
  (call-with-exception-handler
   (λ (e) (begin
            (custodian-shutdown-all (current-custodian))
            (displayln (e))))
   (thunk
    (define listener (tcp-listen (participant-listening-port partic)))
    (let listening-loop ()
      (log "listening" partic)
      
      (define-values (in out) (tcp-accept listener))
      (define recv-value (read-json in))
      
      (log (string-append "received: " (network-message->string recv-value)) partic)

      (cond
        [(network-action? recv-value) (channel-put executor-ch (network-msg->action-list recv-value partic))]
        [else #f])

      (listening-loop)))))

(define (handle-network-connection in out)
  #f)


  #;
  (let state-loop ([node-state (node-state:make)])
    (define action (channel-get action-ch))
    (define new-state (action:apply action node-state))
    (state-loop new-state))


    #;
    (define new-state
      (sync
       (handle-evt action-ch
                   (λ (action)
                     (action:apply action node-state)
                     ())
       (handle-evt data-ch
                   (λ (dr)
                     (channel-put (data-request-return-channel dr)
                                  (data-request-to-be-applied node-state))
                     node-state))))
    
    (state-loop new-state))

#;((
    (cond
      [(message? action) (begin (write "AHHHHHH" (participant-output-port partic))
                                (flush-output (participant-output-port partic)))])
    (state-loop)))


    #;((
    (cond
      [(input-action:send-message? input-value) (send-message partic input-value)])))

      #;
      (if (valid-message? recv-value)
          (channel-put state-ch (network-message:make recv-value out))
          #f)

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


