#lang racket

(require racket/class racket/gui/base)

(provide up left down right middle lsize rsize frame-size
        blue-pen red-pen white-pen blue-dashed-pen 
        my-font get-top-left-corner get-size-xy 
        left-country right-country row-num col-num
        coordinatex is-camp is-base )
 
; =================================
;
; an operator on list and constant
(define (posv op list . const)
    (map (lambda (var) (apply op (cons var const))) list))

; constant values
; =================================
(define-values (up left down right middle) (values '(0 -1) '(-1 0) '(0 1) '(1 0) '(0 0) )) ; country

(define lsize 20) ; mutable data 
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

; ===================================================================
; draw the board
(define blue-pen (new pen% [color "blue"] [width 2]))
(define red-pen (new pen% [color "red"] [width 2]))
(define white-pen (new pen% [color "white"] [width 2]))
(define blue-dashed-pen (new pen% [color "blue"] [width 2] [style 'short-dash]))
(define my-font
      (make-object font% (round (* lsize 57/100)) 'default)) ; here we have size of text!

; ===================================================================

; middle point for each country
(define (middle-point country) (posv +  (posv * country (+ (/ arena-height 2) (/ arena-width 2) margin2)) arena-height (/ arena-width 2) margin2 margin0))
  
; new axis for each country 
(define (new-x country) (list (second country) (- (first country))))
(define (new-y country) country)  
;
(define (row-num country) (if (eq? country middle) 3 6)) ; how many rows in total
(define (col-num country) (if (eq? country middle) 3 5))  ; how many cols in total

; starting point for each country  
(define (starting-point country)
    (map - (middle-point country) (posv * (new-x country) (first half-arena)) (posv * (new-y country) (second half-arena))))
  
(define (coordinatex row col x y country) ; x and y are offset -- calculating the coordinates
    (if (and (eq? country middle)  (< row (row-num country)) (< col (col-num country))) ; conditions
       
       (list (first (coordinatex 0 (+ col col) x 0 down)) (+ (second (coordinatex 0 (+ row row) y 0 left)) (* 1/4 rsize)) ) ; coordinates in the middle
    ; else
       (map + (starting-point country) (posv * (new-x country) (+ (* col (+ rsize rspace))  (* x rsize) ))
                                                  (posv * (new-y country) (+ (* row (+ lsize lspace)) (* y lsize) )))

))
  
; ===================================================================
; utilities

(define (get-top-left-corner country row col)
     (match country
           [(== down) (coordinatex row col 0 0 country)]
           [(== up)     (coordinatex row col 1 1 country)]
           [(== left)    (coordinatex row col 0 1 country)] 
           [(== right)  (coordinatex row col 1 0 country)]       
           [(== middle)  (coordinatex row col 0 0 country)]
))

(define (get-size-xy country x y)
  (match country
    [(== down) (list x y)]
    [(== up)     (list x y)]
    [(== left)    (list y x)]
    [(== right)  (list y x)]
    [(== middle) (list x y)]
    ))

(define (right-country country)
   (match country
     [(== down) right]
     [(== right) up]
     [(== up) left]
     [(== left) down]
     ))

(define (left-country country)
   (match country
     [(== down) left]
     [(== right) down]
     [(== up) right]
     [(== left) up] 
     ))

(define (is-camp row col) ; is camp or not
  (and (or (= row col) (= (+ row col) 4))
          (>= row 1) (<= row 3)
 ))

(define (is-base row col) ; is base or not
  (and (= row 5) (or (= col 1) (= col 3)))
)

; ===================================================================
