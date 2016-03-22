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
    (let*-values ([(country row col chess belong-to) (apply values item)])      
      (draw-chess dc country row col chess (chess-color belong-to) 'solid)      
    )))

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

(define (is-prefix? list1 list2) ; the length of list2 must be greater than that of list1
   (if (null? list1) #t
        (if (eq? (car list1) (car list2)) (is-prefix? (cdr list1) (cdr list2)) #f)
  ))
; in Racket 6.3 the function is-prefix? is named as list-prefix?

(define (occupy country row col chess belong-to)
   (add occupied-list (list country row col chess belong-to))   
)

(define (extract a-list) ; very well written!
  
  (define (same-except-#f lst)
    (andmap (lambda (e1 e2)
              (or (eq? e1 #f) (eq? e1 e2)))
            a-list
            lst
            ))    
       
  (filter same-except-#f occupied-list)
)    
  
         
(define (delete-occupied country row col)
  (set! occupied-list 
       (remove (list country row col) occupied-list is-prefix?)))

(define (find-chess country row col) ; find the chess based on the coordinates
  
  (define item (extract (list country row col #f #f)))

  (if (null? item) (list country row col null null) (car item)  )
)

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
           (member (fourth (find-chess country 5 col)) (list 40 39 38)))
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
      
         (let*-values ([(country row col) (apply values (assign-country-row-col belong-to chess))])
            (occupy country row col chess belong-to)
         )
  )   
)

; ====================================================

(define (check-die? belong-to)

  #f
  
)
  
; ====================================================
(define chess-picked-up #f)
(define chess-from null) 
(define which-turn down)

(define (click-chess the-chess)
    
   (unless (null? the-chess)  
     (let-values ([(t-country t-row t-col t-chess t-belong-to) (apply values the-chess)])
                              
     (if (not chess-picked-up)
       ; chess-not-picked-up
       (when (and (occupied? t-country t-row t-col) (eq? t-belong-to which-turn)
                   (not (or (is-base t-country t-row t-col) (= t-chess 100)) ))
             (set! chess-picked-up #t)
             (set! chess-from the-chess)
             (draw-chess dc t-country t-row t-col t-chess (chess-color 0) 'solid)
        )                                 
          
        ; chess-picked-up
        (let*-values ([(c-country c-row c-col c-chess c-belong-to) (apply values chess-from)]
                           [(r-list) (route-list occupied? c-country c-row c-col c-chess t-country t-row t-col)]) 
          (if (<= (length r-list) 1)
             (begin
                     (set! chess-picked-up #f)
                     (set! chess-from null)
                     (re-draw)
              )
              ; else
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
                  (unless (or (ally? c-belong-to t-belong-to) (is-camp t-country t-row t-col))
                     (let ([beat (beat-it c-chess t-chess)])
                       (when (> beat -1) 
                             (delete-occupied t-country t-row t-col)
                             (if (= t-chess 10) (delete-side t-belong-to) ); delete everything!
                       )      
                       (if (> beat 0) (occupy t-country t-row t-col c-chess c-belong-to))
                       (delete-occupied c-country c-row c-col) 
                       (set! chess-picked-up #f)
                       (set! chess-from null)
                       (set! which-turn (next-country which-turn))
                       (if (check-die? which-turn) (delete-side which-turn))
                       (re-draw)
                   )))))))))


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
                     (click-chess (search-xy find-chess (send event get-x) (send event get-y))) ; derive the chess from the mouse's x, y
                  ) 
               ]             
  )))

(init-board) ; initialize it!
(send my-frame show #t) ; show it!

