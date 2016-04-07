#lang racket

(require racket/class racket/gui/base)

(provide if add get-from
         up left down right middle lsize rsize frame-size
         blue-pen red-pen white-pen blue-dashed-pen 
         my-font get-top-left-corner get-size-xy valid?
         left-country right-country row-num col-num
         coordinatex is-camp? is-base? not-middle? on-rail?
         movable? is-labor? is-flag? beat-it? ally?
         draw-chess whole-rank-set
)
 
; =================================
; syntax sugars

; (if a b) is equal to (if a b null)  
(define-syntax if
  (syntax-rules ()
   [(_ condition then-clause)
    (if condition then-clause null)]
   [(_ condition then-clause else-clause)
    (cond (condition then-clause)
          ((not condition) else-clause))]
    ))

; add an element to an existing list
(define-syntax add
  (syntax-rules ()
    [(_ lst element)
     (set! lst (cons element lst))]
    ))

; extract variables values from a list
; example: (get-from (a b c) a-b-c-list)
(define-syntax get-from
  (syntax-rules ()
    [(_ vars lst)
     (define-values vars (apply values
                                (let [(lst-value lst) (var-length (length (quote vars)))]
        (if (and lst-value (not (null? lst-value))) ; if it is not #f or null
            (take lst-value var-length) ; take first several elements
            (make-list var-length #f)) ; make a list consisting of #f
        )))
    ]))

; =================================
;
; an operator on list and constant
(define (posv op list . const)
    (map (lambda (var) (apply op (cons var const))) list))

; constant values
; =================================
(define-values (up left down right middle) (values 'up 'left 'down 'right 'middle)) ; country

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
; pens and fonts
(define blue-pen (new pen% [color "blue"] [width 2]))
(define red-pen (new pen% [color "red"] [width 2]))
(define white-pen (new pen% [color "white"] [width 2]))
(define blue-dashed-pen (new pen% [color "blue"] [width 2] [style 'short-dash]))
(define my-font
      (make-object font% (round (* lsize 57/100)) 'default)) ; here we have size of text!

; ===================================================================

(define (country-orientation country)
  (match country
    [(== down)  '(0 1)]
    [(== up)    '(0 -1)]
    [(== left)  '(-1 0)]
    [(== right) '(1 0)]
    [(== middle)'(0 0)]
    ))

; middle point for each country
(define (middle-point country) (posv +  (posv * (country-orientation country) (+ (/ arena-height 2) (/ arena-width 2) margin2)) arena-height (/ arena-width 2) margin2 margin0))
  
; new axis for each country 
(define (new-x country) (list (second (country-orientation country)) (- (first (country-orientation country)))))
(define (new-y country) (country-orientation country))  

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
   (right-country (right-country (right-country country)))
)

(define (not-middle? country)
  (not (eq? country middle))
)

(define (ally? country1 country2) ; if they are allies or same side
  (and
  (or (eq? country1 country2)
     (eq? country1 (left-country (left-country country2))))
  (not (eq? country1 middle)) (not (eq? country2 middle))))     
       
(define (is-camp? country row col) ; is camp or not
  (and (not (eq? country middle))
          (or (= row col) (= (+ row col) 4)) (>= row 1) (<= row 3)
 ))

(define (is-base? country row col) ; is base or not
  (and (not (eq? country middle)) (= row 5) (or (= col 1) (= col 3)))
)

(define (on-rail? country row col) ; is on railway or not
  (or (eq? country middle) (= row 0) (= row 4) 
       (and (= col 0) (< row 5)) (and (= col 4) (< row 5))))

(define (valid? country row col) ; if a position is valid
     (and (>= row 0) (< row (row-num country))
            (>= col 0) (< col (col-num country))))
         
; ===================================================================

(define (rank-code num)
  (match num [40 "司令"] [39 "军长"] [38 "师长"] [37 "旅长"] [36 "团长"] [35 "营长"] [34 "连长"] [33 "排长"] [30 "工兵"] [10 "军旗"] [100 "地雷"] [0 "炸弹"] [else ""])
)

(define whole-rank-set
    (list 10 100 100 100 0 0 30 30 30 40 39 38 38 37 37 36 36 35 35 34 34 34 33 33 33) ; order is relevant
)     

(define (beat-it? num1 num2) ; win = 1, lose = -1, equal = 0
  (if (= (* num1 num2) 0) 0 ; if either is bomb then equal
     (if (and (= num1 30) (= num2 100)) 1 ; laborer > landmine
         (sgn (- num1 num2)))))

(define (is-labor? rank)
  (= rank 30)
)

(define (is-flag? rank)
  (= rank 10)
)  

(define (movable? rank)
  (not (or (= rank 10) (= rank 100)))
)

; ===================================================================
; draw a chess somewhere with regards to its text and color

(define (chess-color belong-to-country) ; chooses diferent colors for different belonging-to-countries
      (match belong-to-country
         [(== down) "red"]
         [(== up)     "fuchsia"]
         [(== left)    "green"]
         [(== right)  "yellow"]
         [else "Light Gray"]
  ))


(define (draw-chess dc country row col rank belong-to state) ; dc is the device
  
   (let ([xy (get-top-left-corner country row col)]
          [ab (get-size-xy country)] 
          [code (rank-code rank)]
          [iota 0.1]) ; iota is a small offset

    (define color (chess-color belong-to))
    (define filled-style 'solid)

    (if (eq? state 'picked-up) (set! color "Light Gray"))
    (if (eq? state 'extra) (set! filled-style 'crossdiag-hatch))
     
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