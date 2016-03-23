#lang racket

; utilities
; not touch it

(require "4go-def.rkt")  
(provide search-xy) ; (search-xy x y) returns (list country row col) or null

;==================================================================================================

(define (with-in x y xy ab) ; detect if point (x, y) lies within the rectangle defined by top left xy and size ab
  (let* ([x0 (first xy)] [y0 (second xy)]
           [a (first ab)] [b (second ab)]
           [ratio-x (/ (- x x0) a)] [ratio-y (/ (- y y0) b)])
    (and (>= ratio-x 0) (<= ratio-x 1)
           (>= ratio-y 0) (<= ratio-y 1))))

;==================================================================================================

(define (search-xy x y) ; search the parameters of chess according to its coordinates (x, y)
  (let ([result null]
         [quit #f] )
     (for* ([country (list up left down right middle)]
              [row (range (row-num country))]
              [col (range (col-num country))])
       #:break quit
     (let ([xy (get-top-left-corner country row col)]
            [ab (get-size-xy country)])
       (if (with-in x y  xy ab)
          (begin (set! result (list country row col)) (set! quit #t))          
          )))
    result
    ))

;==================================================================================================
