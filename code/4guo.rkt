#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4guodef.rkt")

; ===================================================================
(define dc null)
(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

; ===================================================================

(define (draw-all-chesses lst)
  (if (null? lst)
    null         
    (let*-values ([(country row col chess belong-to) (apply values (car lst))])      
      (draw-chess dc country row col chess (chess-color belong-to) 'solid)      
      (draw-all-chesses (cdr lst))
    )))


(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (draw-all-chesses occupied-list)
     
     (for* ([country (list down up left right)]) ; draw some extra flag

       (draw-chess dc country 5 5 null (chess-color country) 
                         (if (eq? country which-turn) 'crossdiag-hatch 'solid) )
      ) 
)

; ===================================================================

(define occupied-list null) ; empty list

(define (sub-list? list1 list2) ; the length of list2 must be greater than that of list1
   (if (null? list1) #t
        (if (eq? (car list1) (car list2)) (sub-list? (cdr list1) (cdr list2)) #f)
  ))
                        
(define (occupy country row col chess belong-to)
   (set! occupied-list (cons (list country row col chess belong-to) occupied-list))   
)

(define (delete-occupied country row col)
  (set! occupied-list 
       (remove (list country row col) occupied-list sub-list?)))

(define (find-chess country row col) ; find the chess based on the coordinates
    (let ([item (filter (lambda (lst) (sub-list? (list country row col) lst)) occupied-list)])
        (if (null? item) (list country row col null null) (car item)  )))

(define (occupied? country row col) 
      (not (null? (fourth (find-chess country row col)))))

(define (delete-side belong-to) ; delete everything of a country
  (set! occupied-list 
        (remove* (list belong-to) occupied-list
                 (lambda (item lst) (eq? item (last lst)))
                 )))

(define (empty? belong-to)
  (not (member belong-to (map last occupied-list)))
)
  
(define (next-country country)
  (if (empty? (right-country country))
      (next-country (right-country country))
      (right-country country)
  )    
)      

;
  
; ====================================================================
; assignments

(define (forbidden chess country row col)
  
  (define forb #f)
  (if (and (= chess 10) (not (is-base country row col))) 
      (set! forb #t) null)
  (if (and (is-base country row col) (not (member chess (list 10 100 33 34))))
      (set! forb #t) null)
  (if (and (= chess 100) (not (>= row 4)))
      (set! forb #t) null)
  (if (and (= chess 0) (= row 0))
      (set! forb #t) null)
      
  forb
)

(define (assign-country-row-col belong-to chess)

  (define country null)
  (define row null)
  (define col null)
  
  (set! country belong-to)
  
  (set! row (list-ref (range (row-num belong-to)) (random (row-num belong-to))))
  (set! col (list-ref (range (col-num belong-to)) (random (col-num belong-to))))
                               
  (if (or (occupied? country row col) (is-camp country row col) (forbidden chess country row col))
       (assign-country-row-col belong-to chess) (list country row col))
)

(define (init-board)  
  
  (set! occupied-list null)
  (define country null)

  (for* ([belong-to (list up down left right)]
         [chess whole-chess-set])
      
         (let*-values ([(country row col) (apply values (assign-country-row-col belong-to chess))])
            (occupy country row col chess belong-to)
         )
  )   
)

; ====================================================

(define (direct-row country row row2 col) ; from (country row col) to (country row2 col)
  (cond [(= row2 row) (list (list country row col))]
           [(> row2 row) (cons (list country row col) (direct-row country (add1 row) row2 col))]
           [(< row2 row) (reverse (direct-row country row2 row col))] ))

(define (direct-col country row col col2) ; from (country row col?) to (country row col2)
  (cond [(= col2 col) (list (list country row col))]
           [(> col2 col) (cons (list country row col) (direct-col country row (add1 col) col2))]
           [(< col2 col) (reverse (direct-col country row col2 col))] ))

; ====================================================

(define (neighbours-on-rail country row col) ; successful, do not touch it
    (define they null)
    (define neighbours 
         (list (list country (add1 row) col)
               (list country (sub1 row) col)
               (list country row (add1 col))
               (list country row (sub1 col))
         ))  
    (if (and (not-middle country) (= row 0) (= col 0))
       (set! neighbours (cons (list (left-country country) 0 4) neighbours))
       null
     )  
    (if (and (not-middle country) (= row 0) (= col 4))
       (set! neighbours (cons (list (right-country country) 0 0) neighbours))
       null
     )
  (if (and (not-middle country) (= row 0) (even? col))
      (set! neighbours (cons
           (cond [(eq? country down) (list middle 2 (/ col 2))]
                    [(eq? country up) (list middle 0 (- 2 (/ col 2)))]
                    [(eq? country left) (list middle (/ col 2) 0)]
                    [(eq? country right) (list middle (- 2 (/ col 2)) 2)]
                    ) neighbours))
      null
     )
     (if (eq? country middle)
        (for* ([country2 (list down up left right)]
               [col2 (list 0 2 4)])
          (if (member (list country row col) (neighbours-on-rail country2 0 col2))
             (set! neighbours (cons (list country2 0 col2) neighbours))             
              null
          ))
        null
     )        
    (if (not (on-rail country row col)) null
       (set! they (filter (lambda (x) (and (apply on-rail x) (apply valid? x)))
                              neighbours))
    )
   they
)  
        
  
  
(define (labor-fly country row col country2 row2 col2) ; successful, do not touch it
; if the chess is a "labor", on the rail assumed
; a very hard to code module, requires lots of endeavor
  
  (define (search-next-step fly-route-list)
    
    (define chosen (list (list country row col)))
    (define new-route null)
    (define new-list null)
    (define final-pos null)
    (define quit #f)  
    
    (for* ([route fly-route-list])
     #:break quit
        (set! final-pos (last route))

        (if (equal? final-pos (list country2 row2 col2))
           (begin
               (set! chosen route)
               (set! quit #t)
           )
           ; else
           (for* ([next-pos (apply neighbours-on-rail final-pos)])
               (set! new-route (append route (list next-pos)))
            ; else if
              (if (or (and (apply occupied? next-pos) (not (equal? next-pos (list country2 row2 col2)))) 
                       (member next-pos route)) ; have to ensure the route does not pass the same place twice
               null
               (set! new-list (append new-list (list new-route)))
           )))
     )
    
  (if quit
       chosen 
       (if (null? new-list) (list (list country row col)) 
          (search-next-step new-list)
          )
  ))
  
  (if (and (on-rail country row col) (on-rail country2 row2 col2)) ; to check if on the rail
       (search-next-step (list (list (list country row col))))
       null
   )    
) 

; ====================================================

(define (route-list country row col is-labor? country2 row2 col2) ; chess = 30 to check if it is labor
  ; move from one position to another position, according to current states of the board
  ; if somewhere is blocked, the move is not allowed, returns original position of length 1
  
  ; if the move is successful, returns the list of route, and prepare to move to or "fight with" the chess 
  ; in destination.
  (define result (list (list country row col)))
  ; 
  (if (and (eq? country country2)
             (or
                   (= (+ (abs (- row2 row)) (abs (- col2 col))) 1); case 1
                   (and (or (is-camp country row col) (is-camp country2 row2 col2)) (= (abs (- row2 row)) 1) (= (abs (- col2 col)) 1)) ; case 2
              )) ; one step cases
     (set! result (list (list country row col)  (list country2 row2 col2)) )
     null)
  (if (and (on-rail country row col) (on-rail country2 row2 col2))
     (set! result
     (if is-labor?
              (labor-fly country row col country2 row2 col2)        
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
                      [(and (not-middle country) (eq? country2 middle) (not (and (= col 2) (or (= row 4) (= row2 4)))))
                         (cond [(and (eq? country down) (= col2 (/ col 2))) (append (direct-row country row 0 col) (direct-row middle 2 row2 col2))]
                                 [(and (eq? country up)     (= col2 (- 2 (/ col 2)))) (append (direct-row country row 0 col) (direct-row middle 0 row2 col2))]
                                 [(and (eq? country left)    (= row2 (/ col 2))) (append (direct-row country row 0 col) (direct-col middle row2 0 col2))]
                                 [(and (eq? country right)  (= row2 (- 2 (/ col 2)))) (append (direct-row country row 0 col) (direct-col middle row2 2 col2))]
                                 [else null])]
                      [(and (eq? country middle) (not-middle country2)) (reverse (route-list country2 row2 col2 is-labor? country row col))]
                      [else null])
     ))
     null)
  ; if blocked, set it to one position
  (if (> (length result) 2)
      (if  (eval (cons 'or (map (lambda (x) (apply occupied? x)) (drop-right (cdr result) 1)) )) 
          (set! result null) 
      null)
  null)    
  ; return
  result
)
  

; ====================================================

(define (with-in x y xy ab) ; detect if point (x, y) lies within the rectangle defined by top left xy and size ab
  (let* ([x0 (first xy)] [y0 (second xy)]
           [a (first ab)] [b (second ab)]
           [ratio-x (/ (- x x0) a)] [ratio-y (/ (- y y0) b)])
    (and (>= ratio-x 0) (<= ratio-x 1)
           (>= ratio-y 0) (<= ratio-y 1))))

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
          (begin (set! result (find-chess country row col)) (set! quit #t))          
          null
          )))
    result
    ))

; ====================================================

(define chess-picked-up #f)
(define chess-from null) 
(define which-turn down)

(define (click-chess the-chess)
    
   (if (not (null? the-chess))  
       (let-values ([(t-country t-row t-col t-chess t-belong-to) (apply values the-chess)])
                              
       (if (not chess-picked-up)
       ; chess-not-picked-up
       (if (and (occupied? t-country t-row t-col) (eq? t-belong-to which-turn)
                   (not (or (is-base t-country t-row t-col) (= t-chess 100)) ))
        (begin
                   (set! chess-picked-up #t)
                   (set! chess-from the-chess)
                   (draw-chess dc  t-country t-row t-col t-chess (chess-color 0) 'solid)
         )                                 
         null) 
         
        ; chess-picked-up
        (let*-values ([(c-country c-row c-col c-chess c-belong-to) (apply values chess-from)]
                           [(r-list) (route-list c-country c-row c-col (= c-chess 30) t-country t-row t-col)]) 
          (if (<= (length r-list) 1)
             (begin
                     (set! chess-picked-up #f)
                     (set! chess-from null)
                     (re-draw)
              )
           (if (not (occupied? t-country t-row t-col))                            
             (begin
                     (delete-occupied c-country c-row c-col) 
                     (occupy t-country t-row t-col c-chess c-belong-to)
                     (set! chess-picked-up #f)
                     (set! chess-from null)
                     (set! which-turn (next-country which-turn))
                     (re-draw)
             )
             ; else occupied
                (if (or (ally? c-belong-to t-belong-to)  (is-camp t-country t-row t-col)) null
                   (let ([beat (beat-it c-chess t-chess)])
                       (if (> beat -1) 
                           (begin
                               (delete-occupied t-country t-row t-col)
                               (if (= t-chess 10)
                                   (delete-side t-belong-to); delete everything!
                                   null
                               ))    
                           null)      
                       (if (> beat 0) (occupy t-country t-row t-col c-chess c-belong-to) null)
                       (delete-occupied c-country c-row c-col) 
                       (set! chess-picked-up #f)
                       (set! chess-from null)
                       (set! which-turn (next-country which-turn))
                       (re-draw)
              )))                                                   
       ))))   
  null))


; ====================================================
; draw the animation
;
(define my-frame (new frame% [label "米勒酷四国军棋"] ; define a new frame
                                  [width frame-size] [height frame-size]
                                  [alignment '(center center)] ))

(define my-canvas ; set up a canvas for pictures
      (new (class canvas%
             (super-new [parent my-frame])
             [define/override (on-paint)
                 (set! dc (send my-canvas get-dc))
                 (re-draw) ; draw the current chess board
              ]
              [define/override (on-event event) ; mouse event
                 (set! dc (send my-canvas get-dc))
                 (if (send event button-down? 'any) ; if the mouse is pressed
                     (click-chess (search-xy (send event get-x) (send event get-y))) ; derive the chess from the mouse's x, y
                  null) 
               ]             
  )))

(init-board) ; initialize it!
(send my-frame show #t) ; show it!

