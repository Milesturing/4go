#lang racket
; four-country battle game

(require racket/class racket/gui/base)
(require "4guodef.rkt")

(define my-font
      (make-object font% (round (* lsize 57/100)) 'default)) ; here we have size of text!

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
(define dc null)

(define target (make-bitmap frame-size frame-size))

(send target load-file "chessboard.png" 'png)

; ===================================================================

(define (show-chess row col country chess color)
  
   (let ([xy (get-top-left-corner country row col)]
          [ab (get-size-xy country rsize lsize)])
     
    (send dc set-brush color 'solid)
     
;   (send dc draw-bitmap-section target38 (first xy) (second xy) 0 0 45 30 )
     
    (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) )
     
    (send dc set-font my-font) 
    (send dc draw-text chess (+ (first xy) (* (first ab) 0.1) ) (second xy) #f 0  
             (cond [(eq? country up) 0] 
                      [(eq? country down) 0]
                      [(eq? country left) (/ pi -2)]
                      [(eq? country right) (/ pi 2)] )
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
               ;(send dc clear)
               ;(send dc draw-bitmap target 0 0)
               (chessboard)
               (re-draw)

               ]
              [define/override (on-event event)
                (define button-pressed (send event button-down? 'any))
                ;
                (define which-chess null)
                (set! dc (send my-canvas get-dc))
                (if button-pressed
                   (begin
                     (set! which-chess (search-xy (send event get-x) (send event get-y)))
                     (if (not (null? which-chess))
                     (if (not chess-picked-up)
                        ; chess-not-picked-up
                        (if (occupied? (first which-chess) (second which-chess) (third which-chess))
                            (begin
                                (set! chess-picked-up #t)
                                (set! chess-from which-chess)
                                (send dc set-brush "green" 'solid)
                                (show-chess (second which-chess) (third which-chess) (first which-chess) "军长" "green"))
                                 
                            null) 
                        ; chess-picked-up
                        (if (not (occupied? (first which-chess) (second which-chess) (third which-chess)))
                          (begin
                            (delete-occupied (first chess-from) (second chess-from) (third chess-from)) 
                            (occupy (first which-chess) (second which-chess) (third which-chess) "军长")
                            (set! chess-picked-up #f)
                            (set! chess-from null)
                            (re-draw)
                            ) null))
                          null)   
                   ) null
                )]
                
             ))
  )

(send my-frame show #t) ; show it!

