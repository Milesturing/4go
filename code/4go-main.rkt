#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-route.rkt" "4go-utils.rkt")

; ===================================================================
; global variables
(define dc null) ; drawing context
(define which-turn down) ; starting country

(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

; ===================================================================
; whether each side is human or computer

(define (player country)

  (match country
    [(== down) 'human]
    [(== right) 'human]
    [(== up) 'human]
    [(== left) 'human]
    )
  
)

; ====================================================

(define (computer-run belong-to)
  
   ; testing
  
   (if (and (occupied? belong-to 2 0) (not (occupied? belong-to 3 1)))

         (move-to belong-to 2 0 belong-to 3 1)

     (if (and (occupied? belong-to 3 1) (not (occupied? belong-to 2 0)))

         (move-to belong-to 3 1 belong-to 2 0)

     )    
         
   )
  
)


; ===================================================================

(define (draw-all-chesses)
  (for ([occupied-item occupied-list])
    
      (get-from (country row col rank belong-to state) occupied-item)    
      (draw-chess dc country row col rank belong-to state)
      
    ))

(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (draw-all-chesses)
     
     (for* ([country (list down up left right)]) ; draw some extra flag

       (draw-chess dc country 5 5 null country (if (eq? country which-turn) 'extra 'normal) )
      ) 
)

; ===================================================================

(define occupied-list null) ; occupied-list is a list of (country row col rank belong-to state)

(define (occupy country row col rank belong-to state)
   (add occupied-list (list country row col rank belong-to state))   
)

(define (same-country-row-col? country row col)
  (lambda (lst)
     (and (eq? (first lst) country)
          (eq? (second lst) row)
          (eq? (third lst) col)))
)

(define (same-belong-to? belong-to)
  (lambda (lst)
    (eq? (fifth lst) belong-to))
)

(define (delete-occupied country row col)
  (set! occupied-list
        (filter-not (same-country-row-col? country row col) occupied-list)
  ))

(define (find-picked-up)
  (findf
    (lambda (lst)
      (eq? (sixth lst) 'picked-up)
      ) occupied-list)
)

(define (find-whole-chess country row col)

     (findf
       (same-country-row-col? country row col)
      occupied-list)
)

(define (find-rank country row col) ; find the rank based on the coordinates
  
  (define item (find-whole-chess country row col))
  
  (if item (fourth item) #f)
)

(define (occupied? country row col) 
      (if (find-whole-chess country row col) #t #f))
            
(define (delete-country belong-to) ; delete everything of a country
  (set! occupied-list
        (filter-not (same-belong-to? belong-to) occupied-list)))
        
(define (empty? belong-to)
  (null? (filter (same-belong-to? belong-to) occupied-list)))
  
; ====================================================================
; assignments

(define (forbidden rank country row col)
  
  (define forb #f)
  (if (and (= rank 10) (not (is-base? country row col))) 
      (set! forb #t))
  (if (and (is-base? country row col) (not (member rank (list 10 100 33))))
      (set! forb #t))
  (if (and (= rank 100) (not (>= row 4)))
      (set! forb #t))
  (if (and (= rank 0) (= row 0))
      (set! forb #t))
  (if (and (= rank 100) (= row 4) (even? col)
           (member (find-rank country 5 col) (list 40 39 38)))
      (set! forb #t))
  (if (and (= row 5) (even? col) (member rank (list 40 39 38))
           (eq? (find-rank country 4 col) 100))
      (set! forb #t))
           
      
  forb
)

(define (assign-country-row-col belong-to rank)

  (define country belong-to)
  (define row null)
  (define col null)
  
  (set! row (list-ref (range (row-num belong-to)) (random (row-num belong-to))))
  (set! col (list-ref (range (col-num belong-to)) (random (col-num belong-to))))
                               
  (if (or (occupied? country row col) (is-camp? country row col) (forbidden rank country row col))
       (assign-country-row-col belong-to rank) (list country row col))
)

(define (init-board)  
  
  (set! occupied-list null)
  (define country null)

  (for* ([belong-to (list up down left right)]
         [rank whole-rank-set])
      
         (get-from (country row col) (assign-country-row-col belong-to rank))
    
         (occupy country row col rank belong-to 'normal)
   )   
)

; ====================================================

(define (go-to-next-country)

  (set! which-turn (right-country which-turn))

   (if (empty? which-turn)
       (go-to-next-country)

       (when (eq? (player which-turn) 'computer)
           (computer-run which-turn)
           (re-draw)
           (go-to-next-country)
       )          
   )    
)  

; ====================================================

(define (move-to o-country o-row o-col country row col)

   (get-from (o-c o-r o-l o-rank o-belong-to o-state) (find-whole-chess o-country o-row o-col))
   (get-from (c r l rank belong-to state) (find-whole-chess country row col))
        
   (when (not (occupied? country row col))
     (delete-occupied o-country o-row o-col)
     (occupy country row col o-rank o-belong-to 'normal)
   )
 
)

; ====================================================

(define (click-chess country-row-col)

  (when country-row-col
    
    (get-from (country row col) country-row-col)
    (get-from (c r l rank belong-to state) (find-whole-chess country row col))
    (get-from (o-country o-row o-col o-rank o-belong-to o-state) (find-picked-up)) ; original info
        
    (when (and (not o-state) (eq? belong-to which-turn) (movable? rank) (not (is-base? country row col)))

        (delete-occupied country row col)
        (occupy country row col rank belong-to 'picked-up)

    )
    
    (when o-state

      (define move-list (route-list occupied? o-country o-row o-col o-rank country row col))      
      (define accessible (> (length move-list) 1))
      
      (when (not accessible) 

         (delete-occupied o-country o-row o-col)
         (occupy o-country o-row o-col o-rank o-belong-to 'normal)

      )

      (when (and accessible (not state)) ; empty position

          (delete-occupied o-country o-row o-col)
          (occupy country row col o-rank o-belong-to 'normal)
          (go-to-next-country)

      )

      (when (and accessible state (not (ally? o-belong-to belong-to))
                 (not (is-camp? country row col)) ) ; fight with it!

        (define beat? (beat-it? o-rank rank))
        (when (> beat? -1)
              (delete-occupied country row col)
              (if (is-flag? rank) (delete-country belong-to))
         )
        (delete-occupied o-country o-row o-col)
        (if (= beat? 1) (occupy country row col o-rank o-belong-to 'normal))
        (go-to-next-country)

      )
      
    )
    
    (re-draw)

  )
   
)

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
                  ) 
               ]             
  )))

(init-board) ; initialize it!
(send my-frame show #t) ; show it!

