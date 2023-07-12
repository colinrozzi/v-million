#lang racket

(require "node-state.rkt")

(provide
 (contract-out
  [action? contract?]
  [action:make (-> (-> hash? node-state? node-state? any) hash? action?)]
  [action:apply (-> action? node-state? node-state? any)]))

(struct action [script ht])

;An action is a function that can be applied to the node state and a hash table,
; that returns a node state and a result
(define (action:make lamb ht)
  (action lamb ht))

;Apply the action to the node state and the internal hash table
; returns the resulting node state and result
(define (action:apply act node-state)
  ((action-script act) (action-ht act) node-state))