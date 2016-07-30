#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-utils.rkt"
         "4go-obj.rkt" "4go-route.rkt")

(require "Strategies/4go-str0.rkt" "Strategies/4go-str1.rkt") ; load strategies

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
    [(== right) 'strategy1]
    [(== up) 'strategy1]
    [(== left) 'strategy1]
    )
  
)

(define (player-name player)

  (match player
    [(== 'strategy0) "ST0"]
    [(== 'strategy1) "ST1"]
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

(define (draw-route move-list rank belong-to time)

    (for* ([route move-list])
      
           (get-from (r-country r-row r-col) route)      
           (send board delete-occupied r-country r-row r-col)
           (send board occupy r-country r-row r-col rank belong-to 'picked-up)      
           (re-draw)
           (sleep (/ time (length move-list))) 
           (send board delete-occupied r-country r-row r-col)
     )
  
)

; ====================================================

(define (move-to o-country o-row o-col country row col)

   (define (board-occupied? x y z) (send board occupied? x y z)) 

   (get-from (_c _r _l o-rank o-belong-to o-state) (send board find-whole-chess o-country o-row o-col))
  
   (get-from (_cc _rr _ll rank belong-to state) (send board find-whole-chess country row col))

   (define move-list (route-list board-occupied? o-country o-row o-col o-rank country row col))      
   (define accessible (> (length move-list) 1))

   (when (and accessible (not (board-occupied? country row col)))

      (draw-route move-list o-rank o-belong-to 0.7)

      (send board occupy country row col o-rank o-belong-to 'normal)

   )

   (when (and accessible state (board-occupied? country row col) (enemy? o-belong-to belong-to) (not (is-camp? country row col)) ) ; fight with it!

        (define beat? (beat-it? o-rank rank))
        (draw-route move-list o-rank o-belong-to 0.7)

        (when (> beat? -1)
              (send board delete-occupied country row col)
              (if (is-flag? rank) (send board delete-country belong-to))
         )     

        (if (= beat? -1)
            (send board occupy country row col rank belong-to 'normal)
        )    
            
        (if (= beat? 1)
          (send board occupy country row col o-rank o-belong-to 'normal)
        )  

   )
)

; ====================================================

(define (click-chess country-row-col)

  (when country-row-col
    
    (get-from (country row col) country-row-col)
    (get-from (_c _r _l rank belong-to state) (send board find-whole-chess country row col))
    (get-from (o-country o-row o-col o-rank o-belong-to o-state) (send board find-picked-up)) ; original info
        
    (when (and (not o-state) (eq? belong-to which-turn) (movable? rank) (not (is-base? country row col)))

        (send board delete-occupied country row col)
        (send board occupy country row col rank belong-to 'picked-up)

    )
    
    (when o-state

      (define (board-occupied? x y z) (send board occupied? x y z))
      
      (define move-list (route-list board-occupied? o-country o-row o-col o-rank country row col))      
      (define accessible (> (length move-list) 1))
      
      (when (not accessible) 

         (send board delete-occupied o-country o-row o-col)
         (send board occupy o-country o-row o-col o-rank o-belong-to 'normal)

      )

      (when (and accessible (not state)) ; empty position

          (draw-route move-list o-rank o-belong-to 0.1)

          (send board occupy country row col o-rank o-belong-to 'normal)
        
          (go-to-next-country)

      )

      (when (and accessible state (enemy? o-belong-to belong-to)
                 (not (is-camp? country row col)) ) ; fight with it!

        (define beat? (beat-it? o-rank rank))

        (draw-route move-list o-rank o-belong-to 0.1)

        (when (> beat? -1)
              (send board delete-occupied country row col)
              (if (is-flag? rank) (send board delete-country belong-to))
         )

        (if (= beat? -1)
            (send board occupy country row col rank belong-to 'normal)
        )    

        (when (= beat? 1)
          (send board occupy country row col o-rank o-belong-to 'normal)
        )
      
        (go-to-next-country)

      )
      
    )
    
    (re-draw)

  )
   
)

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
           (member (send board find-its-rank country 5 col) (list 40 39 38 0)))
      (set! forb #t))
  (if (and (= row 5) (even? col) (member rank (list 40 39 38 0))
           (eq? (send board find-its-rank country 4 col) 100))
      (set! forb #t))
                 
  forb
)

(define (assign-country-row-col belong-to rank)

  (define country belong-to)
  (define row null)
  (define col null)
  
  (set! row (list-ref (range (row-num belong-to)) (random (row-num belong-to))))
  (set! col (list-ref (range (col-num belong-to)) (random (col-num belong-to))))
                               
  (if (or (send board occupied? country row col) (is-camp? country row col) (forbidden rank country row col))
          (assign-country-row-col belong-to rank) (list country row col))
)

(define (init-board)  
  
  (send board set-occupied-list null)
  (define country null)

  (if #t ; for debug
      
  (for* ([belong-to (list up down left right)]
         [rank whole-rank-set])
      
         (get-from (country row col) (assign-country-row-col belong-to rank))
    
         (send board occupy country row col rank belong-to 'normal)
   )  

  (begin ; for debug only
  (send board occupy down 4 1 37 down 'normal)
  (send board occupy right 4 1 39 right 'normal)
  
  )

  )
)

; ====================================================

(define (computer-run belong-to strategy)

  (define the-move
     
     (cond [(eq? strategy 'strategy0)
            (strategy0 board belong-to)]

           [(eq? strategy 'strategy1)
            (strategy1 board belong-to)]

           ; may add more strategies here
         
           [else
            (error "No such strategy!")]
     )
  )

  (apply move-to the-move)
)  

; ====================================================

(define (go-to-next-country)

  (set! which-turn (right-country which-turn))

   (if (send board is-empty? which-turn)
       (go-to-next-country)
       ; else
       (unless (eq? (player which-turn) 'human)
           (re-draw)
;           (sleep 0.7)
           (computer-run which-turn (player which-turn)) ; the second argument being the
                                                         ; strategy number that computer adopts
           (go-to-next-country)
       )          
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

