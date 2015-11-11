#lang racket

(require racket/class racket/gui/base)

(provide up left down right blue-pen red-pen white-pen
        blue-dashed-pen my-font left-country right-country
        get-top-left-corner get-size-xy coordinatex
        lsize rsize frame-size is-camp is-base legal-move)
 
; =================================
;
; an operator on list and constant
(define (posv op list . const)
    (map (lambda (var) (apply op (cons var const))) list))

; constant values
; =================================
(define-values (up left down right) (values '(0 -1) '(-1 0) '(0 1) '(1 0) )) ; country

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

(define (get-top-left-corner country row col)
     (cond
           [(eq? country down) (coordinatex row col 0 0 country)]
           [(eq? country up)     (coordinatex row col 1 1 country)]
           [(eq? country left)    (coordinatex row col 0 1 country)] 
           [(eq? country right)  (coordinatex row col 1 0 country)] )
)

(define (get-size-xy country x y)
  (cond
    [(eq? country down) (list x y)]
    [(eq? country up)     (list x y)]
    [(eq? country left)    (list y x)]
    [(eq? country right)  (list y x)]
    ))

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

; starting point for each country  
(define (starting-point country)
    (map - (middle-point country) (posv * (new-x country) (first half-arena)) (posv * (new-y country) (second half-arena))))
  
(define (coordinate i j country) ; i from 0 to 5, j from 0 to 4
    (map + (starting-point country) (posv * (new-x country) j (+ rsize rspace)) (posv * (new-y country) i (+ lsize lspace)) ))

(define (coordinatex i j x y country) ; x and y are offset -- calculating the coordinates
    (map + (starting-point country) (posv * (new-x country) (+ (* j (+ rsize rspace))  (* x rsize) ))
                                                  (posv * (new-y country) (+ (* i (+ lsize lspace)) (* y lsize) ))))

; ====================================================
; utilities

(define (is-camp row col) ; is camp or not
  (and (or (= row col) (= (+ row col) 4))
          (>= row 1) (<= row 3)
 ))

(define (is-base row col) ; is base or not
  (and (= row 5) (or (= col 1) (= col 3)))
)

(define (right-country country)
   (match country
     [(== down) right]
     [(== right) up]
     [(== up) left]
     [(== left) down]))

(define (left-country country)
   (match country
     [(== down) left]
     [(== right) down]
     [(== up) right]
     [(== left) up] ))
  
(define (legal-move country row col country2 row2 col2)        
  (if (eq? country country2)
     (or
     (and (or (= col 0) (= col 4)) (= col2 col) (not (= row2 row)) (< row 5) (< row2 5)) ; case 1
     (and (or (= row 0) (= row 4)) (= row2 row) (not (= col2 col))) ; case 2
     (= (+ (abs (- row2 row)) (abs (- col2 col))) 1); case 3
     (and (or (is-camp row col) (is-camp row2 col2)) (= (abs (- row2 row)) 1) (= (abs (- col2 col)) 1)) ; case 4
  ) 
  ; else                 
  (or   
  (and (eq? country2 (right-country country))
          (= col 4) (not (= row 5)) (= col2 0) (not (= row2 5)))
  (and (eq? country2 (left-country country))
          (= col 0) (not (= row 5)) (= col2 4) (not (= row2 5)))
  (and (eq? country2 (right-country (right-country country)))
          (even? col) (= (+ col2 col) 4) 
          (or (and (= col 2) (= row 0) (= row2 0)) 
               (and (not (= col 2)) (not (= row 5)) (not (= row2 5))))
  )
)))


; ===================================================================
