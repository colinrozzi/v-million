#lang racket

(require json)

(module+ main
  (let input-loop ()
    (define inp (read-json (current-input-port)))
    (print inp)
    (input-loop)))