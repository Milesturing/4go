#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-route.rkt" "4go-utils.rkt")

; ===================================================================
; global variables
(define dc null) ; drawing context

(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

; ===================================================================

(define (draw-all-chesses)
  (for ([item occupied-list])
    
      (get-from (country row col chess belong-to) item)    
      (draw-chess dc country row col chess (chess-color belong-to) 'solid)
      
    ))

(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (draw-all-chesses)
     
     (for* ([country (list down up left right)]) ; draw some extra flag

       (draw-chess dc country 5 5 null (chess-color country) 
                         (if (eq? country which-turn) 'crossdiag-hatch 'solid) )
      ) 
)

; ===================================================================

(define occupied-list null) ; occupied-list is a list of (country row col chess belong-to)

(define (occupy country row col chess belong-to)
   (add occupied-list (list country row col chess belong-to))   
)

(define (same? country-row-col-else) ; judge if an element in occupied-list is
                                ; the same as country-row-col except #f  
  (lambda (lst)
    (andmap (lambda (e1 e2)
              (or (eq? e1 #f) (eq? e1 e2)))
            country-row-col-else
            lst
            ))    

)    
         
(define (delete-occupied country row col)
  (set! occupied-list
        (filter-not (same? (list country row col #f #f)) occupied-list)
  ))

(define (find-chess country row col) ; find the chess based on the coordinates
  
  (define item (findf
                 (same? (list country row col #f #f))
                 occupied-list))

  (if item (fourth item) #f)
)

(define (occupied? country row col) 
      (not (null? (filter (same? (list country row col #f #f)) occupied-list))))
            
(define (delete-side belong-to) ; delete everything of a country
  (set! occupied-list
        (filter-not (same? (list #f #f #f #f belong-to)) occupied-list)))
        
(define (empty? belong-to)
  (null? (filter (same? (list #f #f #f #f belong-to)) occupied-list)))
  
(define (next-country country)
  (if (empty? (right-country country))
      (next-country (right-country country))
      (right-country country)
  )    
)      

  
; ====================================================================
; assignments

(define (forbidden chess country row col)
  
  (define forb #f)
  (if (and (= chess 10) (not (is-base country row col))) 
      (set! forb #t))
  (if (and (is-base country row col) (not (member chess (list 10 100 33))))
      (set! forb #t))
  (if (and (= chess 100) (not (>= row 4)))
      (set! forb #t))
  (if (and (= chess 0) (= row 0))
      (set! forb #t))
  (if (and (= chess 100)  (= row 4) (even? col)
           (member (find-chess country 5 col) (list 40 39 38)))
      (set! forb #t))
           
      
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
      
         (get-from (country row col) (assign-country-row-col belong-to chess))
    
         (occupy country row col chess belong-to)
   )   
)

; ====================================================

(define (check-die? belong-to)

  #f
  
)
  
; ====================================================
(define set-picked-up null) 
(define which-turn down)

(define (click-chess country-row-col)

   (define cur-set #f)

   (unless (null? country-row-col)
       (set! cur-set
         (findf (same? (append country-row-col (list #f #f))) occupied-list) 
       )
       (if (not cur-set)
           (set! cur-set (append country-row-col (list null null)))
       )
   )

   (when cur-set
     
     (get-from (cur-country cur-row cur-col cur-chess belong-to) cur-set)
                              
     (if (null? set-picked-up)
       ; chess-not-picked-up
       (when (and (occupied? cur-country cur-row cur-col) (eq? belong-to which-turn)
                   (not (or (is-base cur-country cur-row cur-col) (= cur-chess 100)) ))
             (set! set-picked-up cur-set)
             (draw-chess dc cur-country cur-row cur-col cur-chess (chess-color 0) 'solid)
        )                                 
          
        ; set-picked-up
        (begin
          (get-from (from-country from-row from-col from-chess from-belong-to) set-picked-up)
          (get-from (r-list) (list (route-list occupied? from-country from-row from-col from-chess cur-country cur-row cur-col)))
          
          (if (<= (length r-list) 1)
             (begin
                     (set! set-picked-up null)
                     (re-draw)
              )
              ; else
              (if (not (occupied? cur-country cur-row cur-col))                            
                 (begin
                     (delete-occupied from-country from-row from-col) 
                     (occupy cur-country cur-row cur-col from-chess from-belong-to)
                     (set! set-picked-up null)
                     (set! which-turn (next-country which-turn))
                     (re-draw)
                  )
                  ; else occupied
                  (unless (or (ally? from-belong-to belong-to) (is-camp cur-country cur-row cur-col))
                     (let ([beat (beat-it from-chess cur-chess)])
                       (when (> beat -1) 
                             (delete-occupied cur-country cur-row cur-col)
                             (if (= cur-chess 10) (delete-side belong-to) ); delete everything!
                       )      
                       (if (> beat 0) (occupy cur-country cur-row cur-col from-chess from-belong-to))
                       (delete-occupied from-country from-row from-col) 
                       (set! set-picked-up null)
                       (set! which-turn (next-country which-turn))
                       (if (check-die? which-turn) (delete-side which-turn))
                       (re-draw)
                   ))))))))


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

