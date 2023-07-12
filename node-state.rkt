#lang racket

(provide
 (contract-out
  [node-state? contract?]
  [node-state:make (-> node-state?)]))

(struct node-state [])

(define (node-state:make)
  (node-state))