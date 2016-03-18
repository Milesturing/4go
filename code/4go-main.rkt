#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-route.rkt" "4go-utils.rkt")

; ===================================================================
(define dc null)
(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

; ===================================================================

(define (draw-all-chesses lst)
  (if (null? lst)
    null         
    (let*-values ([(country row col chess belong-to) (apply values (car lst))])      
      (draw-chess dc country row col chess (chess-color belong-to) 'solid)      
      (draw-all-chesses (cdr lst))
    )))


(define (re-draw)
     (send dc clear)
     (send dc draw-bitmap target 0 0)
     (draw-all-chesses occupied-list)
     
     (for* ([country (list down up left right)]) ; draw some extra flag

       (draw-chess dc country 5 5 null (chess-color country) 
                         (if (eq? country which-turn) 'crossdiag-hatch 'solid) )
      ) 
)

; ===================================================================

(define occupied-list null) ; empty list

(define (is-prefix? list1 list2) ; the length of list2 must be greater than that of list1
   (if (null? list1) #t
        (if (eq? (car list1) (car list2)) (is-prefix? (cdr list1) (cdr list2)) #f)
  ))
; in Racket 6.3 the function is-prefix? is named as list-prefix?

(define (occupy country row col chess belong-to)
   (set! occupied-list (cons (list country row col chess belong-to) occupied-list))   
)

(define (delete-occupied country row col)
  (set! occupied-list 
       (remove (list country row col) occupied-list is-prefix?)))

(define (find-chess country row col) ; find the chess based on the coordinates
  (define item (filter (lambda (lst) (is-prefix? (list country row col) lst)) occupied-list))
  
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
      (set! forb #t) null)
  (if (and (is-base country row col) (not (member chess (list 10 100 33))))
      (set! forb #t) null)
  (if (and (= chess 100) (not (>= row 4)))
      (set! forb #t) null)
  (if (and (= chess 0) (= row 0))
      (set! forb #t) null)
  (if (and (= chess 100)  (= row 4) (even? col)
           (member (fourth (find-chess country 5 col)) (list 40 39 38)))
      (set! forb #t) null)
           
      
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


(define chess-picked-up #f)
(define chess-from null) 
(define which-turn down)

(define (click-chess the-chess)
    
   (if (not (null? the-chess))  
       (let-values ([(t-country t-row t-col t-chess t-belong-to) (apply values the-chess)])
                              
       (if (not chess-picked-up)
       ; chess-not-picked-up
       (if (and (occupied? t-country t-row t-col) (eq? t-belong-to which-turn)
                   (not (or (is-base t-country t-row t-col) (= t-chess 100)) ))
        (begin
                   (set! chess-picked-up #t)
                   (set! chess-from the-chess)
                   (draw-chess dc  t-country t-row t-col t-chess (chess-color 0) 'solid)
         )                                 
         null) 
         
        ; chess-picked-up
        (let*-values ([(c-country c-row c-col c-chess c-belong-to) (apply values chess-from)]
                           [(r-list) (route-list occupied? c-country c-row c-col (= c-chess 30) t-country t-row t-col)]) 
          (if (<= (length r-list) 1)
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
                     (set! which-turn (next-country which-turn))
                     (re-draw)
             )
             ; else occupied
                (if (or (ally? c-belong-to t-belong-to)  (is-camp t-country t-row t-col)) null
                   (let ([beat (beat-it c-chess t-chess)])
                       (if (> beat -1) 
                           (begin
                               (delete-occupied t-country t-row t-col)
                               (if (= t-chess 10)
                                   (delete-side t-belong-to); delete everything!
                                   null
                               )
                           )    
                           null)      
                       (if (> beat 0) (occupy t-country t-row t-col c-chess c-belong-to) null)
                       (delete-occupied c-country c-row c-col) 
                       (set! chess-picked-up #f)
                       (set! chess-from null)
                       (set! which-turn (next-country which-turn))
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
                     (click-chess (search-xy find-chess (send event get-x) (send event get-y))) ; derive the chess from the mouse's x, y
                  null) 
               ]             
  )))

(init-board) ; initialize it!
(send my-frame show #t) ; show it!

