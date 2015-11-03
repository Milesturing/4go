#lang racket
; four-country battle game

(require racket/class racket/gui/base)
(require "4guodef.rkt")

; ===================================================================

(define target (make-bitmap frame-size frame-size))
(define dc null)

(send target load-file "chessboard.png" 'png)

; ===================================================================


(define occupied-list null) ; empty list

(define (occupy country row col chess)
  (let ([xy (coordinatex row col 0 0 country)] 
         [xyp (coordinatex row col 1 1 country)])     
   (send dc set-brush "red" 'solid)
   (my-draw-rectangle dc xy xyp)
   (set! occupied-list (cons (list country row col chess) occupied-list))   
))

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
   (send dc set-brush "white" 'solid)
  ; (draw-empty-chess row col country)
   (send dc draw-bitmap target 0 0)
)
  
(define (chessboard)  

  
  (occupy down 0 0 39)
;  (occupy up     1 0 40)
  
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
               (send dc clear)
               (send dc draw-bitmap target 0 0)
               (chessboard)
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
                                (my-draw-rectangle dc (fourth which-chess) (fifth which-chess)))
                            null) 
                        ; chess-picked-up
                        (if (not (occupied? (first which-chess) (second which-chess) (third which-chess)))
                          (begin
                            (delete-occupied (first chess-from) (second chess-from) (third chess-from)) 
                            (occupy (first which-chess) (second which-chess) (third which-chess) 39)
                            (set! chess-picked-up #f)
                            (set! chess-from null)
                            ) null))
                          null)   
                   ) null
                )]
                
             ))
  )

(send my-frame show #t) ; show it!

