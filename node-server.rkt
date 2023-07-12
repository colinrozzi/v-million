#lang racket
(require json)

; Should always be running, waiting for connections from other server-nodes
; take in messages and update state
; I want the node server to take a connection from the client and deliver the current state in html format
; Then, wait for the client to make a change to the state, notify other nodes, and keep on chugging

(define (run-server port)
  (parameterize ([current-custodian (make-custodian)])
    (with-handlers ([exn:fail? (lambda (e) (begin
                                             (custodian-shutdown-all)
                                             (displayln (e))))])
      ;start up with no network connections
      ;wait for client input to connect us to a network
      ;join that network, set up network state
      ;be an active participant on the network
      ;wait for more client input

      (define state-channel (make-channel))
      (thread (thunk (manage-network-state state-channel)))

      
      
      (define receiving-thread-channel (make-channel))
      (define network-thread-channel (make-channel))
      (thread (thunk (receiving-thread receiving-thread-channel network-thread-channel))
      (thread (thunk (network-thread network-thread-channel)))
      ))))

(define (manage-network-state state-ch)
  (let loop ([curr-state (make-network-state)]
             [request (channel-get state-ch)])
    (define new-state (cond
                        [(update? request) (update-network-state curr-state req)]
                        [(pull? request)   (return-data curr-state req)]))
    (loop new-state (channel-get state-ch))))

(define (update? req)
  (equal? (car req) 'update))

(define (pull? req)
  (equal? (car req) 'pull))

(define (receiving-thread receving-tc network-tc)
  #f)

(define (network-thread network-tc)
  #f)




      
;      (define state-thread-channel (make-channel))
;      (define decision-thread-channel (make-channel))
      