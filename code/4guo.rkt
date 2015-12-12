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
      (draw-chess dc country row col chess (chess-color belong-to))      
      (draw-all-chesses (cdr lst))
    )))


(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (draw-all-chesses occupied-list)
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
;
  
(define (init-board)  
  
  (set! occupied-list null) ; 
  
  (occupy down 1 4 39 down)
  (occupy up     1 0 38 up)
  (occupy left    0 0 40 left)
  (occupy right  2 2 0 down)
  (occupy right    5 2 38 right)
  (occupy left    5 4 100 left)
  (occupy left    4 2 34 down)
  
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


(define (route-list country row col country2 row2 col2 is-labor?)
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
     ; we should do something here
     (set! result
     (cond [(and (eq? country country2) (or (= row 0) (= row 4)) (= row2 row)) (direct-col country row col col2)]
              [(and (eq? country country2) (or (= col 0) (= col 4)) (= col2 col)) (direct-row country row row2 col)]
              [(and (eq? country2 (right-country country)) (= col 4) (= col2 0)) (append (direct-row country row 0 col) (direct-row country2 0 row2 col2))]
              [(and (eq? country2 (left-country country)) (= col 0) (= col2 4)) (append (direct-row country row 0 col) (direct-row country2 0 row2 col2))]
              [(and (eq? country2 (right-country (right-country country))) (even? col) (= (+ col2 col) 4))
                (append (direct-row country row 0 col) null (direct-row country2 0 row2 col2))] ; middle is important!
              [(and (eq? country middle) (eq? country2 middle) (= row2 row)) (direct-col middle row col col2)]
              [(and (eq? country middle) (eq? country2 middle) (= col2 col)) (direct-row middle row row2 col)]
              
              [else (list (list country row col))])
     )
     null)
  ; if blocked, set it to one position
  (if (> (length result) 2)
      (if  (eval (cons 'or (map (lambda (x) (apply occupied? x)) (drop-right (cdr result) 1)) )) 
          (set! result (list (list country row col))) 
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

(define chess-picked-up #f)
(define chess-from null) 

(define (click-chess the-chess)
    
   (if (not (null? the-chess))  
       (let-values ([(t-country t-row t-col t-chess t-belong-to) (apply values the-chess)])
                              
       (if (not chess-picked-up)
       ; chess-not-picked-up
       (if (and (occupied? t-country t-row t-col)
                   (not (or (is-base t-country t-row t-col) (= t-chess 100)) ))
        (begin
                   (set! chess-picked-up #t)
                   (set! chess-from the-chess)
                   (draw-chess dc  t-country t-row t-col t-chess (chess-color 0))
         )                                 
         null) 
         
        ; chess-picked-up
        (let*-values ([(c-country c-row c-col c-chess c-belong-to) (apply values chess-from)]
                           [(r-list) (route-list c-country c-row c-col t-country t-row t-col (eq? c-chess 30) )]) 
          (if (= (length r-list) 1)
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
                     (re-draw)
             )
             ; else occupied
                (if (or (ally? c-belong-to t-belong-to)  (is-camp t-country t-row t-col)) null
                   (let ([beat (beat-it c-chess t-chess)])
                       (if (> beat -1) (delete-occupied t-country t-row t-col) null)      
                       (if (> beat 0) (occupy t-country t-row t-col c-chess c-belong-to) null)
                       (delete-occupied c-country c-row c-col) 
                       (set! chess-picked-up #f)
                       (set! chess-from null)
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

