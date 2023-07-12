#lang racket

(provide
 (contract-out
  [action? contract?]
  [action:make (-> (-> hash? any) hash? action?)]
  [action:apply (-> action? node-state? any)]))

(struct action [script ht])

(define (action:make lamb ht)
  (action lamb ht))

(define (action:apply act node-state)
  ((action-script act) (action-ht act) node-state))