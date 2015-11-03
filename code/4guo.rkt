#lang racket
; four-country battle game

(require racket/class racket/gui/base)
(require "4guodef.rkt")

; ===================================================================

(define dc null)

(define (draw-board)
  (for* ([row (range 6)] ; 6 rows
           [col (range 5)] ; 5 columns
           [country (list up left down right)]) ; we have 4 countries
   (draw-empty-chess row col country)))

; ===================================================================

(define (is-camp row col) ; is camp or not
  (and (or (= row col) (= (+ row col) 4))
          (>= row 1) (<= row 3)
 ))

(define (is-base row col) ; is base or not
  (and (= row 5) (or (= col 1) (= col 3)))
)
  
; ===================================================================

(define (draw-empty-chess i j country)
     (let* ([xy (coordinatex i j 0 0 country)] 
              [xyp (coordinatex i j 1 1 country)]
              [xy2 (coordinatex i j 1 1/2 country)]
              [xyn (coordinatex i (+ j 1) 0 1/2 country)] 
              [xy3 (coordinatex i j 1/2 1 country)] 
              [xyd (coordinatex (+ i 1) j 1/2 0 country)] 
              [xy0 (coordinatex i j 1/2 1/2 country)]
              [radius1 (abs (/ (- (first xyp) (first xy)) 2))]
              [radius2 (abs (/ (- (second xyp) (second xy)) 2))])
       (begin
        (send dc set-pen blue-pen)
         (if (is-camp i j) ; draw circle
            (begin
               (send dc set-pen white-pen)
               (send dc set-brush "white" 'solid)
               (my-draw-rectangle dc xy xyp) ; erase rectangle
               (send dc set-pen blue-pen)
               (for* ([ii '(-1 1)]
                        [jj '(-1 1)]) ; draw diagonal line
                 (if (and (= i 2) (= j 2)) null  (if (and (= (+ i ii) 2) (= (+ j jj) 2)) 
                  (let ([xynew (coordinatex (+ i ii) (+ j jj) (- 1/2 (* jj 1/2 0.8)) (- 1/2 (* ii 1/2 0.8)) country)])
                   (send dc draw-line (first xy0) (second xy0) (first xynew) (second xynew)))                 
                  (let ([xynew (coordinatex (+ i ii) (+ j jj) (- 1/2 (* jj 1/2)) (- 1/2 (* ii 1/2)) country)])
                   (send dc draw-line (first xy0) (second xy0) (first xynew) (second xynew)))
                  ))) 
                (send dc set-pen red-pen)
                (send dc draw-ellipse (- (first xy0) radius1) (- (second xy0) radius2) (* 2 radius1) (* 2 radius2))
                (send dc set-pen blue-pen)
             )
            ; else
            (begin
             (if (is-base i j) (send dc set-pen red-pen) (send dc set-pen blue-pen))        
             (my-draw-rectangle dc xy xyp) ; draw rectangle
             (send dc set-pen blue-pen)
             )
         )
         (if (< j 4) (send dc draw-line (first xy2) (second xy2) (first xyn) (second xyn)) null)
         (if (< i 5) (send dc draw-line (first xy3) (second xy3) (first xyd) (second xyd)) null)
         )))
  
  ;  (send dc draw-text "军长" 130 130)

; ===================================================================

(define (draw-middle)
  (send dc set-pen blue-pen)
  (for ([j '(0 2 4)])
   (let ([xyup (coordinatex 0 j 1/2 0 up)]
          [xydown (coordinatex 0 (- 4 j) 1/2 0 down)]
          [xyleft (coordinatex 0 j 1/2 0 left)]
          [xyright (coordinatex 0 (- 4 j) 1/2 0 right)])
     (send dc draw-line (first xyup) (second xyup) (first xydown) (second xydown))
     (send dc draw-line (first xyleft) (second xyleft) (first xyright) (second xyright))     
     ))
  ; draw trails
  (let ([leftp (coordinatex 0 0 1/2 0 left)]
        [leftq (coordinatex 0 4 1/2 0 left)]
        [downp (coordinatex 0 0 1/2 0 down)]
        [downq (coordinatex 0 4 1/2 0 down)]
        [rightp (coordinatex 0 0 1/2 0 right)]
        [rightq (coordinatex 0 4 1/2 0 right)]
        [upp (coordinatex 0 0 1/2 0 up)]
        [upq (coordinatex 0 4 1/2 0 up)]
        )
  (send dc set-pen blue-dashed-pen)
  (send dc draw-line (first leftq) (second leftq) (first downp) (second downp))
  (send dc draw-line (first downq) (second downq) (first rightp) (second rightp))
  (send dc draw-line (first rightq) (second rightq) (first upp) (second upp))
  (send dc draw-line (first upq) (second upq) (first leftp) (second leftp))
  (send dc set-pen blue-pen) 
   )
  ; draw circle  
   (for* ([i '(0 2 4)] [j '(0 2 4)])
   (let ([xyup (coordinatex 0 i 1/2 0 up)]
          [xyleft (coordinatex 0 j 1/2 0 left)]
          [radius (* lsize 0.9)])
     (send dc draw-rounded-rectangle (- (first xyup) radius) (- (second xyleft) radius) (* 2 radius) (* 2 radius))   
  )))


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
   (draw-empty-chess row col country)
)
  
(define (chessboard)  
  (draw-board)
  (draw-middle)
  
  (occupy down 0 0 39)
  (occupy up     1 0 40)
  
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
