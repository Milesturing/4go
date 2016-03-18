#lang racket

(require racket/class racket/gui/base)

(provide iff
         up left down right middle lsize rsize frame-size
         blue-pen red-pen white-pen blue-dashed-pen 
         my-font get-top-left-corner get-size-xy valid?
         left-country right-country row-num col-num
         coordinatex is-camp is-base not-middle on-rail
         chess-code beat-it ally? chess-color draw-chess 
         whole-chess-set
)
 
; =================================
; syntax sugar
; (iff a b) is equal to (if a b null)

(define-syntax iff
  (syntax-rules ()
    [(_ cond then-do)
     (if cond then-do null)]
    [(_ cond then-do else-do)
     (if cond then-do else-do)]
))

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

(define (row-num country) (if (eq? country middle) 3 6)) ; how many rows in total
(define (col-num country) (if (eq? country middle) 3 5))  ; how many cols in total

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

(define (get-size-xy country)
  (match country
    [(== down) (list rsize lsize)]
    [(== up)     (list rsize lsize)]
    [(== left)    (list lsize rsize)]
    [(== right)  (list lsize rsize)]
    [(== middle) (list rsize lsize)]
    ))

(define (right-country country)
   (match country
     [(== down) right]
     [(== right) up]
     [(== up) left]
     [(== left) down]
     [else null]
     ))

(define (left-country country)
   (match country
     [(== down) left]
     [(== right) down]
     [(== up) right]
     [(== left) up] 
     [else null]
     ))

(define (not-middle country)
  (not (eq? country middle))
)

(define (ally? country1 country2) ; if they are allies or same side
  (and
  (or (eq? country1 country2)
     (eq? country1 (left-country (left-country country2))))
  (not (eq? country1 middle)) (not (eq? country2 middle))))     
       
(define (is-camp country row col) ; is camp or not
  (and (not (eq? country middle))
          (or (= row col) (= (+ row col) 4)) (>= row 1) (<= row 3)
 ))

(define (is-base country row col) ; is base or not
  (and (not (eq? country middle)) (= row 5) (or (= col 1) (= col 3)))
)

(define (on-rail country row col) ; is on railway or not
  (or (eq? country middle) (= row 0) (= row 4) 
       (and (= col 0) (< row 5)) (and (= col 4) (< row 5))))

(define (valid? country row col) ; if a position is valid
     (and (>= row 0) (< row (row-num country))
            (>= col 0) (< col (col-num country))))
         
; ===================================================================

(define (chess-code num)
  (match num [40 "司令"] [39 "军长"] [38 "师长"] [37 "旅长"] [36 "团长"] [35 "营长"] [34 "连长"] [33 "排长"] [30 "工兵"] [10 "军旗"] [100 "地雷"] [0 "炸弹"] [else ""])
)

(define whole-chess-set
    (list 10 100 100 100 0 0 30 30 30 40 39 38 38 37 37 36 36 35 35 34 34 34 33 33 33) ; order is relevant
)     

(define (beat-it num1 num2) ; win = 1, lose = -1, equal = 0
  (if (= (* num1 num2) 0) 0 ; if either is bomb then equal
     (if (and (= num1 30) (= num2 100)) 1 ; laborer > landmine
         (sgn (- num1 num2)))))

(define (chess-color belong-to-country) ; chooses different colors for different belonging-to-countries
  (match belong-to-country
    [(== down) "red"]
    [(== up)     "fuchsia"]
    [(== left)    "green"]
    [(== right)  "yellow"]
    [else "Light Gray"]
    )) 

; ===================================================================
; draw a chess somewhere with its text and color

(define (draw-chess dc country row col chess color filled-style) ; dc is the device
  
   (let ([xy (get-top-left-corner country row col)]
          [ab (get-size-xy country)] 
          [code (chess-code chess)]
          [iota 0.1]) ; iota is a small offset
     
    (send dc set-brush color filled-style) ; can be 'solid     
    (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) )
     
    (send dc set-font my-font)  
    (cond 
      [(eq? country left)  (send dc draw-text code  (+ (first xy) (first ab)) (+ (second xy) (* (second ab) iota)) #f 0 (/ pi -2))]
      [(eq? country right) (send dc draw-text code (first xy)  (+ (second xy) (* (second ab) (- 1 iota))) #f 0 (/ pi 2))]   
      [ else (send dc draw-text code (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)] ; when country = up, down, middle
     ) ; show text
))

; ===================================================================    