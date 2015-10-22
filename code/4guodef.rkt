#lang racket

(require racket/class racket/gui/base)

(provide up left down right blue-pen red-pen white-pen
        blue-dashed-pen my-draw-rectangle coordinatex
        lsize frame-size)
 
; =================================
;
; an operator on list and constant
(define (posv op list . const)
    (map (lambda (var) (apply op (cons var const))) list))

; constant values
; =================================
(define-values (up left down right) (values '(0 -1) '(-1 0) '(0 1) '(1 0))) ; country

(define lsize 17) 
(define rsize (* lsize 2) )
(define size (list rsize lsize))

(define lspace (quotient lsize 2) ) 
(define rspace (quotient rsize 3) )
(define space (list rspace lspace))

(define arena-height (+ (* 6 lsize) (* 5 lspace))) ; height of "one country"
(define arena-width (+ (* 5 rsize) (* 4 rspace))) ; width of "one country"
(define arena (list arena-width arena-height))
(define half-arena (posv * arena 1/2))

(define margin0 (truncate (* lsize 5/10))) ; zero point
(define margin1 (truncate (* lsize 27/10))) ; extra margins for frame
(define margin2 (truncate (* lsize 9/10))) ; separation between each country

(define frame-size (+ margin0 margin1 margin2 margin2 arena-height arena-height arena-width))
; =================================
; draw the board
(define blue-pen (new pen% [color "blue"] [width 2]))
(define red-pen (new pen% [color "red"] [width 2]))
(define white-pen (new pen% [color "white"] [width 2]))
(define blue-dashed-pen (new pen% [color "blue"] [width 2] [style 'short-dash]))
(define (my-draw-rectangle dc xy xy2)
  (let* ([x (first xy)] [y (second xy)] [a (- (first xy2) (first xy))] [b (- (second xy2) (second xy))]
           [xx x] [yy y] [aa a] [bb b])
     (if (< a 0) (begin (set! xx (+ x a)) (set! aa (- a))) null)
     (if (< b 0) (begin (set! yy (+ y b)) (set! bb (- b))) null)        
     (send dc draw-rounded-rectangle xx yy aa bb)
  ))                  


; middle point for each country
(define (middle-point country) (posv +  (posv * country (+ (/ arena-height 2) (/ arena-width 2) margin2)) arena-height (/ arena-width 2) margin2 margin0))
  
; new axis for each country 
(define (new-x country) (list (second country) (- (first country))))
(define (new-y country) country)  
;

; starting point for each country  
(define (starting-point country)
    (map - (middle-point country) (posv * (new-x country) (first half-arena)) (posv * (new-y country) (second half-arena))))
  
(define (coordinate i j country) ; i from 0 to 5, j from 0 to 4
    (map + (starting-point country) (posv * (new-x country) j (+ rsize rspace)) (posv * (new-y country) i (+ lsize lspace)) ))

(define (coordinatex i j x y country) ; x and y are offset -- calculating the coordinates
    (map + (starting-point country) (posv * (new-x country) (+ (* j (+ rsize rspace))  (* x rsize) ))
                                                  (posv * (new-y country) (+ (* i (+ lsize lspace)) (* y lsize) ))))

; ====================================================