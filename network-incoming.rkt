#lang racket

(struct network-message [value respond-port] #:transparent)

(define (network-message:make v r)
  (network-message v r))

(define (input-message->network-message partic input-value)
  (hash
   'id (participant-id partic)
   'msg (hash-ref input-value 'msg)))