#lang racket

(require "action.rkt")
(require json)

(provide
 (contract-out
  [input->action (-> natural-number/c hash? action?)]))

(define (input->action id inp)
  (cond
    [(input:send-message? inp) (action:make-message-action id (hash-ref inp 'addr) (hash-ref inp 'msg))]
    [(input:end? inp) (action:make-shutdown-action)]))

(define (action:make-message-action id addr msg)
  (action:make
   (λ (ht)
     (define ht-to-be-sent (hash 'id (hash-ref ht 'id) 'msg (hash-ref ht 'msg)))
     (define-values (in out) (tcp-connect "127.0.0.1" (hash-ref ht 'addr)))
     (write-json ht-to-be-sent out)
     (close-input-port in)
     (close-output-port out))
   (hash 'addr addr 'id id 'msg msg)))

(define (input:send-message? ht)
  (and
   (hash? ht)
   (hash-has-key? ht 'addr)
   (hash-has-key? ht 'msg)))

(define (input:end? ht)
  (and
   (hash? ht)
   (hash-has-key? ht 'end)
   (hash-ref ht 'end)))

#;
(define (input-message->action partic input-value)
  (hash
   'id (participant-id partic)
   'msg (hash-ref input-value 'msg)))