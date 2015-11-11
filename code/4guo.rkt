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

(define (show-chess row col country chess color)
  
   (let ([xy (get-top-left-corner country row col)]
          [ab (get-size-xy country rsize lsize)]
          [iota 0.1]) ; iota is little offset
     
    (send dc set-brush color 'solid)     
    (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) )
     
    (send dc set-font my-font) 
    (match country
      [(== up) (send dc draw-text chess (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)]
      [(== down) (send dc draw-text chess (+ (first xy) (* (first ab) iota)) (second xy) #f 0 0)]
      [(== left)  (send dc draw-text chess  (+ (first xy) (first ab)) (+ (second xy) (* (second ab) iota)) #f 0 (/ pi -2))]
      [(== right) (send dc draw-text chess (first xy)  (+ (second xy) (* (second ab) (- 1 iota))) #f 0 (/ pi 2))]   
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
            [chess (fourth fst)])
      (show-chess row col country chess "red")
      (show-all-chess (cdr lst))
      )))


(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (show-all-chess occupied-list)
)

; ===================================================================

(define occupied-list null) ; empty list

(define (occupy country row col chess)
   (set! occupied-list (cons (list country row col chess) occupied-list))   
)

(define (occupied? country row col)
  (eval (cons 'or ; tricky, needs to be modified
     (map (lambda (item) 
               (and (eq? (first item) country)
                      (eq? (second item) row)
                      (eq? (third item) col)))
      occupied-list))
  ))

(define (delete-occupied country row col)
  (set! occupied-list 
       (remove (list country row col) occupied-list
             (lambda (list1 list2)
               (and (eq? (first list1) (first list2))
                      (eq? (second list1) (second list2))
                      (eq? (third list1) (third list2))
                      ))))
)
  
(define (chessboard)  

  (occupy down 0 0 "军长")
  (occupy up     1 0 "军长")
  
)

; ====================================================

(define (with-in x y xy xyp) ; detect if point (x, y) lies within the rectangle defined by xy and xyp
  (let* ([x0 (first xy)] [y0 (second xy)]
           [xp (first xyp)] [yp (second xyp)]
           [delta-x (- x x0)] [delta-y (- y y0)]
           [delta-xp (- xp x0)] [delta-yp (- yp y0)]
           [ratio-x (/ delta-x delta-xp)] [ratio-y (/ delta-y delta-yp)])
    (and (>= ratio-x 0) (<= ratio-x 1)
           (>= ratio-y 0) (<= ratio-y 1))))
  
(define (search-xy x y) ;
  (let ([result null]
         [quit #f] )
     (for* ([row (range 6)]
              [col (range 5)]
              [country (list up left down right)])
       #:break quit
     (let ([xy (coordinatex row col 0 0 country)]
            [xyp (coordinatex row col 1 1 country)])
       (if (with-in x y  xy xyp)
          (begin (set! result (list country row col xy xyp)) (set! quit #t))          
          null
          )))
    result
    ))

; ====================================================

(define (can-move country row col country2 row2 col2)        
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
          (= col 4) (not (= row 5)) (= col2 0) (not (= row2 5))) ; case 1
  (and (eq? country2 (left-country country))
          (= col 0) (not (= row 5)) (= col2 4) (not (= row2 5))) ; case 2
  (and (eq? country2 (opposite-country country))
          (even? col) (= (+ col2 col) 4) 
          (or (and (= col 2) (= row 0) (= row2 0)) 
               (and (not (= col 2)) (not (= row 5)) (not (= row2 5)))) ; case 3
  )
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
               (chessboard) ; draw the empty board
               (re-draw)

               ]
              [define/override (on-event event)
                (define button-pressed (send event button-down? 'any))
                ;
                (define which-chess null)
                (set! dc (send my-canvas get-dc))
                (if button-pressed ; the button is pressed
                   (begin
                     (set! which-chess (search-xy (send event get-x) (send event get-y)))
                       
                     (if (not (null? which-chess))
                         (let ([t-country (first which-chess)] [t-row (second which-chess)] [t-col (third which-chess)])
                         (if (not chess-picked-up)
                        ; chess-not-picked-up
                        (if (and (occupied? t-country t-row t-col)
                                    (not (is-base t-row t-col)))
                            (begin
                                (set! chess-picked-up #t)
                                (set! chess-from which-chess)
                                (send dc set-brush "green" 'solid)
                                (show-chess t-row t-col t-country  "军长" "green"))
                                 
                            null) 
                        ; chess-picked-up
                        (if (and (not (occupied? t-country t-row t-col))
                                    (can-move (first chess-from) (second chess-from) (third chess-from) t-country t-row t-col))
                                
                          (begin
                            (delete-occupied (first chess-from) (second chess-from) (third chess-from)) 
                            (occupy t-country t-row t-col "军长")
                            (set! chess-picked-up #f)
                            (set! chess-from null)
                            (re-draw)
                            ) null)))
                          null)   
                   ) null
                )]
                
             ))
  )

(send my-frame show #t) ; show it!

