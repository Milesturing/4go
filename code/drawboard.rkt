#lang racket
; four-country battle game

(require racket/class racket/gui/base)
(require "4guodef.rkt")

; ===================================================================

(define target (make-bitmap frame-size frame-size))
(define dc (new bitmap-dc% [bitmap target]))

(define (draw-board)
  (for* ([row (range 6)] ; 6 rows
           [col (range 5)] ; 5 columns
           [country (list up left down right)]) ; we have 4 countries
   (draw-empty-chess row col country)))

; ===================================================================

(define (draw-empty-chess i j country)
     (let* ([xy (get-top-left-corner country i j)]
              [ab (get-size-xy country)]
              [xy2 (coordinatex i j 1 1/2 country)]
              [xyn (coordinatex i (+ j 1) 0 1/2 country)] 
              [xy3 (coordinatex i j 1/2 1 country)] 
              [xyd (coordinatex (+ i 1) j 1/2 0 country)] 
              [xy0 (coordinatex i j 1/2 1/2 country)]
              [radius1 (/ (first ab) 2)]
              [radius2 (/ (second ab) 2)])
       (begin
        (send dc set-pen blue-pen)
         (if (is-camp i j) ; draw circle
            (begin
               (send dc set-pen white-pen)
               (send dc set-brush "white" 'solid)
               (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) ) ; erase rectangle
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
             (send dc draw-rounded-rectangle (first xy) (second xy) (first ab) (second ab) ) ; draw rectangle
             (send dc set-pen blue-pen)
             )
         )
         (if (< j 4) (send dc draw-line (first xy2) (second xy2) (first xyn) (second xyn)) null)
         (if (< i 5) (send dc draw-line (first xy3) (second xy3) (first xyd) (second xyd)) null)
         )))

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


; ====================================================

(define (chessboard)  
  (draw-board)
  (draw-middle)

  )

; ====================================================

(chessboard) ; draw it!

(send target save-file "chessboard.png" 'png) ; save it!

(make-object image-snip% target) ; show it!

; ====================================================