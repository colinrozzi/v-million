#lang racket

(require "action.rkt")

(define (input-action:send-message? ht)
  (and
   (hash? ht)
   (hash-has-key? ht 'addr)
   (hash-has-key? ht 'msg)))

(define (input-action:end? ht)
  (and
   (hash? ht)
   (hash-has-key? ht 'end)
   (hash-ref ht 'end)))

(define (input-message->action partic input-value)
  (hash
   'id (participant-id partic)
   'msg (hash-ref input-value 'msg)))