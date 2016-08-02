#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-utils.rkt"
         "4go-obj.rkt" "4go-route.rkt")

;(require "AutoStrategies/4go-str0.rkt"
;         "AutoStrategies/4go-str1.rkt"
;                                      ) ; load strategies

; ===================================================================
; global variables
(define dc null) ; drawing context
(define which-turn down) ; starting country

(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

(define board (new chess-board%)) ; initialize a new board

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

(define (player-name player)

  (match player
    [(== 'strategy0) "ST0"]
    [(== 'strategy1) "ST1"]
    [(== 'strategy2) "ST2"]
    [(== 'test) "TST"]
    [(== 'human) "人类"]
    [else null]
   )
)

; ====================================================

(define (re-draw)

        (send dc clear)
        (send dc draw-bitmap target 0 0)
        (send board draw-all-chesses dc)
     
        (for* ([country (list down up left right)]) ; draw some extra flags

              (if (eq? country which-turn)
                  (draw-chess dc country 5 5 "" country 'extra)
               ; else
                  (draw-chess dc country 5 5 (player-name (player country)) country 'normal)
              )
        ) 
)

; ====================================================

(define (draw-route move-list chess time)
  
    (for* ([route-step (drop-right move-list 1)])
         
           (send board delete-occupied route-step)
           (send board occupy chess route-step 'picked-up)
      
           (re-draw)
           (sleep (/ time (length move-list)))

           (send board delete-occupied route-step)
     )

     (send board delete-occupied (last move-list))
  
)

; ====================================================

(define (move-to o-pos c-pos)

   (define o-chess (send board find-whole-chess o-pos))
   (define c-chess (send board find-whole-chess c-pos))

   (define move-list (route-list board occupied? o-chess c-pos))      
   (define accessible (> (length move-list) 1))

   (when (and accessible (not-exist? c-chess))

      (draw-route move-list o-chess 0.7)

      (send board occupy o-chess c-pos 'normal)

   )

   (when (and accessible (exist? c-chess) (fight-able o-chess c-chess)) ; fight with it!

        (define beat? (fight-result o-chess c-chess))
        (draw-route move-list o-chess 0.7)

        (when (> beat? -1)
              (send board delete-occupied c-pos)
              (if (is-flag? c-chess) (send board delete-nation c-chess))
         )     

        (if (= beat? -1)
            (send board occupy c-chess c-pos 'normal)
        )    
            
        (if (= beat? 1)
            (send board occupy o-chess c-pos 'normal)
        )  

   )
)

; ====================================================

(define (click-chess country-row-col)

  (define c-pos (set-position country-row-col)) ; c = clicked
  
  (when c-pos    
    
    (define c-chess (send board find-whole-chess c-pos)) ; clicked chess
    (define o-chess (send board find-picked-up)) ; o = original

    (when (and
               (exist? c-chess)
               (not-exist? o-chess)
               (move-able? c-chess)
               (eq? (get-belong-to c-chess) which-turn)
          )

         (send board change-state c-pos 'picked-up) ; change the status

    )
    
    (when (exist? o-chess) ; ready to move

      (define move-list (route-list board occupied? o-chess c-pos))      
      (define accessible (> (length move-list) 1))
      
      (when (not accessible) ; put down the chess

         (define o-pos (send o-chess get-position))
         (send board change-state o-pos 'normal)
        
      )

      (when (and accessible (not-exist? c-chess)) ; empty position

          (draw-route move-list o-chess 0.1)

          (send board occupy o-chess c-pos 'normal)
        
          (go-to-next-country)

      )

      (when (and accessible (exist? c-chess) (fight-able o-chess c-chess)) ; fight with it!

        (draw-route move-list o-chess 0.1)
        
        (define beat? (fight-result o-chess c-chess))
        
        (when (> beat? -1)
              (send board delete-occupied c-pos)
              (if (send c-chess is-flag?) (send board delete-nation c-chess))
         )

        (when (= beat? -1)
            (send board occupy c-chess c-pos 'normal)
        )    

        (when (= beat? 1)
            (send board occupy o-chess c-pos 'normal)
        )
      
        (go-to-next-country)

      )
      
    )
    
    (re-draw)

  )
)

; ====================================================================
; initialization

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
           (member
             (let ([chess (send board find-whole-chess (set-position (list country 5 col)))])
                  (if chess (send chess get-rank) #f)) 
            (list 40 39 38 0)))
      (set! forb #t))
  (if (and (= row 5) (even? col) (member rank (list 40 39 38 0))
           (eq?
             (let ([chess (send board find-whole-chess (set-position (list country 4 col)))])
                  (if chess (send chess get-rank) #f)) 
            100))
      (set! forb #t))
                 
  forb
)

(define (assign-country-row-col belong-to rank)

  (define country belong-to)
  (define row null)
  (define col null)
  
  (set! row (list-ref (range (row-num belong-to)) (random (row-num belong-to))))
  (set! col (list-ref (range (col-num belong-to)) (random (col-num belong-to))))
                               
  (if (or ((occupied? board) country row col) (is-camp? country row col) (forbidden rank country row col))
          (assign-country-row-col belong-to rank) (list country row col))
)

(define (init-board)  
  
  (send board set-occupied-list null)
  (define country null)

  (if #t ; for debug
      
  (for* ([belong-to (list up down left right)]
         [rank whole-rank-set])
      
         (get-from (country row col) (assign-country-row-col belong-to rank))
    
         (send board throw country row col rank belong-to)
   )  

  (begin ; for debug only
  (send board throw down 4 1 30 down)
  (send board throw right 4 1 30 right)

  
  )

  )
)

; ====================================================

(define (go-to-next-country)

  (set! which-turn (right-country which-turn))

   (if (send board is-empty? which-turn)
       (go-to-next-country)
       ; else
       (unless (eq? (player which-turn) 'human)
           (re-draw)

           (computer-run which-turn (player which-turn)) ; the second argument being the
                                                         ; strategy number that computer adopts
           (go-to-next-country)
       )          
   )    
)  

; ====================================================

(define (computer-run belong-to strategy)

  (define the-move (eval (list strategy board belong-to)) )

  (unless (null? the-move) (apply move-to the-move))  
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

