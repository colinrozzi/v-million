ø#lang racket

(require "network-participant.rkt")
(require json)

(define-values (p1i p1-input) (make-pipe))
(define-values (p1-output p1o) (make-pipe))
(define p1 (participant:make 1 p1i p1o 8888))

(define-values (p2i p2-input) (make-pipe))
(define-values (p2-output p2o) (make-pipe))
(define p2 (participant:make 2 p2i p2o 8887))

(with-output-to-file "1-output.txt"
  (λ () (print ""))
  #:exists 'replace)

(with-output-to-file "2-output.txt"
  (λ () (print ""))
  #:exists 'replace)

(thread (thunk (participant:run p1)))
(thread (thunk ((participant:run p2))))
(thread (thunk
         (let output-print-loop ()
           (sync
            (handle-evt
             p1-output
             (λ (port)
               (define msg (read port))
               (with-output-to-file "1-output.txt"
                 (λ () (println msg))
                 #:exists 'append)))
            (handle-evt
             p2-output
             (λ (port)
               (define msg (read port))
               (with-output-to-file "2-output.txt"
                 (λ () (println msg))
                 #:exists 'append))))
           (output-print-loop))))

(sleep 1)

(write-json (hash 'addr 8887 'msg "hey") p1-input)
