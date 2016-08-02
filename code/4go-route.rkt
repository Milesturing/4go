#lang racket

(require "4go-def.rkt" "4go-obj.rkt" "4go-labor.rkt")

(provide route-list  ; (route-list board occupied? chess new-pos) 

         )
 
; route-list modules
;=================================================================================================  

(define (direct-row country row row2 col) ; from (country row col) to (country row2 col)
  (cond [(= row2 row) (list (list country row col))]
           [(> row2 row) (cons (list country row col) (direct-row country (add1 row) row2 col))]
           [(< row2 row) (reverse (direct-row country row2 row col))] ))

(define (direct-col country row col col2) ; from (country row col?) to (country row col2)
  (cond [(= col2 col) (list (list country row col))]
           [(> col2 col) (cons (list country row col) (direct-col country row (add1 col) col2))]
           [(< col2 col) (reverse (direct-col country row col2 col))] ))

;================================================================================================= 

(define (route-list-0 board-occupied? pos1 pos2 labor?) ; rank = 30 to check if it is labor
  ; move from one position to another position, according to current states of the board
  ; if somewhere is blocked, the move is not allowed, returns original position of length 1
  
  ; if the move is successful, returns the list of route, and prepare to move to or "fight with" the chess 
  ; in destination.

  (define-values (country row col) (send pos1 get-values))
  (define-values (country2 row2 col2) (send pos2 get-values))
  
  (define result (list (list country row col)))
  ; 
  (if (and (eq? country country2)
             (or
                   (= (+ (abs (- row2 row)) (abs (- col2 col))) 1); case 1
                   (and (or (is-camp? country row col) (is-camp? country2 row2 col2)) (= (abs (- row2 row)) 1) (= (abs (- col2 col)) 1)) ; case 2
              )) ; one step cases
     (set! result (list (list country row col)  (list country2 row2 col2)) )
     )
  (if (and (on-rail? country row col) (on-rail? country2 row2 col2))
     (set! result
     (if labor? ; is labor
              (labor-fly board-occupied? country row col country2 row2 col2)
              ; else
              (cond [(and (eq? country country2) (or (= row 0) (= row 4)) (= row2 row)) (direct-col country row col col2)]
                       [(and (eq? country country2) (or (= col 0) (= col 4)) (= col2 col)) (direct-row country row row2 col)]
                       [(and (eq? country middle) (eq? country2 middle) (= row2 row)) (direct-col middle row col col2)]
                       [(and (eq? country middle) (eq? country2 middle) (= col2 col)) (direct-row middle row row2 col)]
                       [(and (eq? country2 (left-country country)) (= col 0) (= col2 4)) (append (direct-row country row 0 col) (direct-row country2 0 row2 col2))]
                       [(and (eq? country2 (right-country country)) (= col 4) (= col2 0)) (append (direct-row country row 0 col) (direct-row country2 0 row2 col2))]
                       [(and (eq? country2 (right-country (right-country country))) (even? col) (= (+ col2 col) 4) (not (and (= col 2) (or (= row 4) (= row2 4)))))
                          (append (direct-row country row 0 col) 
                                       (cond [(eq? country down) (direct-row middle 2 0 (/ col 2))]
                                                [(eq? country up)     (direct-row middle 0 2 (- 2 (/ col 2)))]
                                                [(eq? country left)    (direct-col  middle (/ col 2) 0 2)]
                                                [(eq? country right)  (direct-col  middle (- 2 (/ col 2)) 2 0)])
                                       (direct-row country2 0 row2 col2))] ; middle part is important!
                      [(and (not-middle? country) (eq? country2 middle) (not (and (= col 2) (or (= row 4) (= row2 4)))))
                         (cond [(and (eq? country down) (= col2 (/ col 2))) (append (direct-row country row 0 col) (direct-row middle 2 row2 col2))]
                                 [(and (eq? country up)     (= col2 (- 2 (/ col 2)))) (append (direct-row country row 0 col) (direct-row middle 0 row2 col2))]
                                 [(and (eq? country left)    (= row2 (/ col 2))) (append (direct-row country row 0 col) (direct-col middle row2 0 col2))]
                                 [(and (eq? country right)  (= row2 (- 2 (/ col 2)))) (append (direct-row country row 0 col) (direct-col middle row2 2 col2))]
                                 [else null])]
                      [(and (eq? country middle) (not-middle? country2)) (reverse (route-list-0 board-occupied? pos2 pos1 labor?))] ; recursive
                      [else null])
     ))
  )
  ; if blocked, set it to one position
  (if (> (length result) 2)
      (if  (eval (cons 'or (map (lambda (x) (apply board-occupied? x)) (drop-right (cdr result) 1)) )) 
          (set! result null) 
      )
   )    
  ; return
  result
)

;=================================================================================================
  
(define (route-list board occupied? chess new-pos)
  
  (define board-occupied? (occupied? board)) ; a function

  (define result (route-list-0 board-occupied? (get-position chess) new-pos (is-labor? chess))) 
  (map set-position result) ; must put here instead of in route-list-0
  
)
