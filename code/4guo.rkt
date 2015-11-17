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

(define (show-chess country row col chess color)
  
   (let ([xy (get-top-left-corner country row col)]
          [ab (get-size-xy country)]
          [iota 0.1] [the-code (chess-code chess)]) ; iota is little offset
     
    (send dc set-brush color 'solid)     
    (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) )
     
    (send dc set-font my-font) 
    (match country 
      [(== up) (send dc draw-text the-code  (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)]     
      [(== down) (send dc draw-text the-code (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)]
      [(== left)  (send dc draw-text the-code  (+ (first xy) (first ab)) (+ (second xy) (* (second ab) iota)) #f 0 (/ pi -2))]
      [(== right) (send dc draw-text the-code (first xy)  (+ (second xy) (* (second ab) (- 1 iota))) #f 0 (/ pi 2))]   
      [(== middle) (send dc draw-text the-code (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)]
     ) ; show text
))

; ===================================================================

(define (show-all-chess lst)
  (if (null? lst)
    null     
    (let* ([fst (car lst)]
            [country (first fst)]
            [row (second fst)]
            [col (third fst)]
            [chess (fourth fst)]
            [belong-to (fifth fst)] )
      (show-chess country row col chess 
                 (if (or (eq? belong-to up) (eq? belong-to down)) "red" "green") 
                 )
      (show-all-chess (cdr lst))
      )))


(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (show-all-chess occupied-list)
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
  
  (occupy down 1 4 39 left)
  (occupy up     1 0 38 left)
  (occupy left    0 0 40 down)
  (occupy right  2 2 0 right)
  (occupy left    5 2 38 up)
  (occupy left    5 4 100 up)
  
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

(define (can-move country row col country2 row2 col2) 
 (if (eq? country middle) 
   (or
        (and (eq? country2 left) (= col2 (+ row row)) (if (= row 1) (= row2 0) (< row2 5)))
        (and (eq? country2 right) (= col2 (- 4 row row)) (if (= row 1) (= row2 0)  (< row2 5)))
        (and (eq? country2 middle) (= row2 row) (not (= col2 col)))

        (and (eq? country2 down) (= col2 (+ col col)) (if (= col 1) (= row2 0) (< row2 5)))
        (and (eq? country2 up) (= col2 (- 4 col col)) (if (= col 1) (= row2 0) (< row2 5)))
        (and (eq? country2 middle) (= col2 col) (not (= row2 row)))
    )    
  (if (eq? country2 middle)
    (can-move country2 row2 col2 country row col)
  (if (eq? country country2)
     (or
     (and (or (= col 0) (= col 4)) (= col2 col) (not (= row2 row)) (< row 5) (< row2 5)) ; case 1
     (and (or (= row 0) (= row 4)) (= row2 row) (not (= col2 col))) ; case 2
     (= (+ (abs (- row2 row)) (abs (- col2 col))) 1); case 3
     (and (or (is-camp country row col) (is-camp country2 row2 col2)) (= (abs (- row2 row)) 1) (= (abs (- col2 col)) 1)) ; case 4
  ) 
  ; else                 
  (or   
  (and (eq? country2 (right-country country))
          (= col 4) (not (= row 5)) (= col2 0) (not (= row2 5))) ; case 1'
  (and (eq? country2 (left-country country))
          (= col 0) (not (= row 5)) (= col2 4) (not (= row2 5))) ; case 2'
  (and (eq? country2 (right-country (right-country country)))
          (even? col) (= (+ col2 col) 4) 
          (or (and (= col 2) (= row 0) (= row2 0)) 
               (and (not (= col 2)) (not (= row 5)) (not (= row2 5)))) ; case 3'
  )))
)))
  
; ====================================================
; draw the animation
(define chess-picked-up #f)
(define chess-from null)

(define my-frame (new frame% [label "米勒酷四国军棋"] ; define a new frame
                                  [width frame-size] [height frame-size]
                                  [alignment '(center center)] ))

(define my-canvas ; set up a canvas for pictures
      (new (class canvas%
             (super-new [parent my-frame])
             [define/override (on-paint)
               (set! dc (send my-canvas get-dc))
               (re-draw) ; draw the board according to current status
              ]
             
              [define/override (on-event event)
                (define button-pressed (send event button-down? 'any))
                ;
                (define which-chess null)
                (set! dc (send my-canvas get-dc))

                (if button-pressed ; if the button is pressed
                   (begin
                     (set! which-chess (search-xy (send event get-x) (send event get-y)))

                     (if (not (null? which-chess))
                         (let ([t-country (first which-chess)] [t-row (second which-chess)] [t-col (third which-chess)] [t-chess (fourth which-chess)] [t-belong-to (fifth which-chess)]) 
                               
                         (if (not chess-picked-up)
                        ; chess-not-picked-up
                        (if (and (occupied? t-country t-row t-col)
                                    (not (or (is-base t-country t-row t-col) (= t-chess 100)) ))
                            (begin
                                (set! chess-picked-up #t)
                                (set! chess-from which-chess)
                                (show-chess  t-country t-row t-col t-chess  "CornflowerBlue")
                              )                                 
                            null) 
                        ; chess-picked-up
                          (let ([c-country (first chess-from)] [c-row (second chess-from)] [c-col (third chess-from)] [c-chess (fourth chess-from)] [c-belong-to (fifth chess-from)]) 
                          (if  (can-move c-country c-row c-col t-country t-row t-col)
                            
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
                          
                          null))))
                          null)   
                   ) null
                )]                
             ))
  )

(init-board) ; initialize it!
(send my-frame show #t) ; show it!

