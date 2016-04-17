#lang racket
; four-country battle game
; Be sure to run drawboard.rkt first to generate chessboard.png

(require racket/class racket/gui/base)
(require "4go-def.rkt" "4go-route.rkt" "4go-utils.rkt")

; ===================================================================
; global variables
(define dc null) ; drawing context
(define which-turn down) ; starting country
(define occupied-list null) ; occupied-list is a list of (country row col rank belong-to state)

(define target (make-bitmap frame-size frame-size))
(send target load-file "chessboard.png" 'png)

; ===================================================================
; whether each side is human or computer

(define (player country)

  (match country
    [(== down) 'human]
    [(== right) 'computer]
    [(== up) 'computer]
    [(== left) 'computer]
    )
  
)

; ====================================================

(define (under-attack chess)

  (define result 0)

  (get-from (country row col rank belong-to) chess)
  
  (when (and chess (not (is-camp? country row col)))

    (define enemy-chesses

       (append (find-belong-to (right-country belong-to))
               (find-belong-to (left-country belong-to)))

    )
    
    (define quit #f)
    
    (for* [(e-chess enemy-chesses)]
       #:break quit

      (get-from (e-country e-row e-col e-rank) e-chess)

      (when (movable? e-rank)

         (define beat? (beat-it? e-rank rank))
        
         (when (and (= beat? 1) (not (= rank 100)))

           (define move-list (route-list occupied? e-country e-row e-col e-rank country row col))      
           (define accessible (> (length move-list) 1))

           (when accessible
              (set! result (- (score rank)))
              (set! quit #t) ; quit for loop
           )
         )

         (when (= beat? 0)

           (define move-list (route-list occupied? e-country e-row e-col e-rank country row col))      
           (define accessible (> (length move-list) 1))

           (when accessible
              (set! result (- (score e-rank) (score rank)))
              (if (> result 0) (set! result 0))
              (set! quit #t) ; quit for loop
           )
         )

      )
     )
   )

   result

)

(define (score rank)
  
      (match rank
        ([== 10] 500)
        ([== 40] 40)
        ([== 39] 30)
        ([== 38] 20)
        ([== 37] 10)
        ([== 36] 4)
        ([== 35] 3)
        ([== 0]  22)
        ([== 30] 6)
        ([== 100] 3)
        (else    2)
       )
)

(define ratio 0.2) ; a constant

(define (position-value row col flag-col)

  (if (= flag-col 3)
 
      (position-value row (- 4 col) 1)

      ; else flag-col = 1

      (match (list row col)
        
        ([== (list 5 0)] 5)
        ([== (list 5 1)] 0)
        ([== (list 5 2)] 5)
        ([== (list 5 3)] 0)
        ([== (list 4 0)] 5)
        ([== (list 4 1)] 5)
        ([== (list 4 2)] 5)
        ([== (list 4 3)] 2)
        ([== (list 4 4)] 2)
        ([== (list 3 1)] 5)
        ([== (list 3 2)] 2)
        ([== (list 3 3)] 2)
        ([== (list 2 2)] 4)
        (else 1)

      )

  )      
)


(define (calculate-value belong-to)

  (define all-chess (find-belong-to belong-to)) ; all chesses belonging to this side
  (define sum 0)


  (when (not (empty? belong-to)) ; if the side is not empty
 

  (for* ([chess all-chess]) ; for every chess in this side

    (get-from (country row col rank) chess)

    (set! sum (+ sum (score rank)))

    (when (not (is-labor? rank))

          (set! sum (+ sum (* ratio (under-attack chess))))

    )
     
   ) ; for

   )

  sum

)

;

(define (calculate-position-value belong-to)

  (define all-chess (find-belong-to belong-to)) ; all chesses belonging to this side
  (define sum 0)

  (when (not (empty? belong-to)) ; if the side is not empty
 

   (define flag-list (filter (same-rank? 10) all-chess))

   (when (not (null? flag-list))

     (define flag (car flag-list))

     (get-from (_c _r flag-col) flag)

     (for* ([m-country (list belong-to)]
            [m-row (range (row-num m-country))]
            [m-col (range (col-num m-country))])

      (when (occupied? m-country m-row m-col)
        
        (get-from (_cc _rr _ll m-rank m-belong-to) (find-whole-chess m-country m-row m-col))

        (when m-belong-to

          (define sign (if (enemy? m-belong-to belong-to) -1 1/2))

          (set! sum (+ sum (* sign 1/10 (score m-rank) (position-value m-row m-col flag-col))))
        )
        
      )
     ) ; for
     
    )
    )
  
   sum

)


(define (computer-run belong-to)

    (define whole-list (find-belong-to belong-to))
    (define one-move null)
    (define value 0)
    (define max-value -10000)
    (define save-occupied null)

    (for* ([some-chess whole-list]) ; choose all possible chesses

      (get-from (s-country s-row s-col s-rank s-belong-to) some-chess) ; s is source
      
      (if (and (movable? s-rank) (not (is-base? s-country s-row s-col)))
  
      (for* ([d-country (list middle up left down right)] ; d is destination
             [d-row (range (row-num d-country))]
             [d-col (range (col-num d-country))])

         (get-from (_c _r _l d-rank d-belong-to) (find-whole-chess d-country d-row d-col))


         (define move-list (route-list occupied? s-country s-row s-col s-rank d-country d-row d-col))
         (define accessible (> (length move-list) 1))
        

         (define go-able   (or (not (occupied? d-country d-row d-col))
                          (and (occupied? d-country d-row d-col)
                               (enemy? s-belong-to d-belong-to)
                               (not (is-camp? d-country d-row d-col))
                               (not (and (is-base? d-country d-row d-col)
                                         (not (eq? d-rank 10))))
                              )))

         (when (and accessible go-able)

             (set! save-occupied occupied-list)

             (delete-occupied s-country s-row s-col)

             (if (not (occupied? d-country d-row d-col))

                  (occupy d-country d-row d-col s-rank s-belong-to 'normal)
                 
                  ; else

                 (match (beat-it? s-rank d-rank)
                        ([== 1] (begin
                                  (delete-occupied d-country d-row d-col)
                                  (occupy d-country d-row d-col s-rank s-belong-to 'normal)
                                 ))
                        ([== 0] (delete-occupied d-country d-row d-col))
                        ([== -1] null)
                   )
                 )

           
              (set! value (- (+ (calculate-value belong-to)
                                (calculate-position-value belong-to)
                                (calculate-value (right-country (right-country belong-to))))
                             (+ (calculate-value (right-country belong-to))
                                (calculate-value (left-country belong-to)))))
           

              (set! value (+ value (random 5)))
   
              (when (>= value max-value)

                    (set! max-value value)
                    (set! one-move (list s-country s-row s-col d-country d-row d-col))
              )
                  
              (set! occupied-list save-occupied)
            )
        )

    ))


    (apply move-to one-move)

  
)


; ====================================================

(define (draw-route move-list rank belong-to time)

    (for* ([route move-list])
      
           (get-from (r-country r-row r-col) route)      
           (delete-occupied r-country r-row r-col)
           (occupy r-country r-row r-col rank belong-to 'picked-up)      
           (re-draw)
           (sleep (/ time (length move-list))) 
           (delete-occupied r-country r-row r-col)
     )
  
)

(define (move-to o-country o-row o-col country row col)

   (get-from (_c _r _l o-rank o-belong-to o-state) (find-whole-chess o-country o-row o-col))
   (get-from (_cc _rr _ll rank belong-to state) (find-whole-chess country row col))

   (define move-list (route-list occupied? o-country o-row o-col o-rank country row col))      
   (define accessible (> (length move-list) 1))

   (when (and accessible (not (occupied? country row col)))

      (draw-route move-list o-rank o-belong-to 0.7)

      (occupy country row col o-rank o-belong-to 'normal)

   )

   (when (and accessible state (occupied? country row col) (enemy? o-belong-to belong-to) (not (is-camp? country row col)) ) ; fight with it!

        (define beat? (beat-it? o-rank rank))
        (draw-route move-list o-rank o-belong-to 0.7)

        (when (> beat? -1)
              (delete-occupied country row col)
              (if (is-flag? rank) (delete-country belong-to))
         )     

        (if (= beat? -1)
            (occupy country row col rank belong-to 'normal)
        )    
            
        (if (= beat? 1)
          (occupy country row col o-rank o-belong-to 'normal)
        )  

   )
)

; ====================================================

(define (click-chess country-row-col)

  (when country-row-col
    
    (get-from (country row col) country-row-col)
    (get-from (_c _r _l rank belong-to state) (find-whole-chess country row col))
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

          (draw-route move-list o-rank o-belong-to 0.1)

          (occupy country row col o-rank o-belong-to 'normal)
        
          (go-to-next-country)

      )

      (when (and accessible state (enemy? o-belong-to belong-to)
                 (not (is-camp? country row col)) ) ; fight with it!

        (define beat? (beat-it? o-rank rank))

        (draw-route move-list o-rank o-belong-to 0.1)

        (when (> beat? -1)
              (delete-occupied country row col)
              (if (is-flag? rank) (delete-country belong-to))
         )

        (if (= beat? -1)
            (occupy country row col rank belong-to 'normal)
        )    

        (when (= beat? 1)
          (occupy country row col o-rank o-belong-to 'normal)
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
           (member (find-its-rank country 5 col) (list 40 39 38 0)))
      (set! forb #t))
  (if (and (= row 5) (even? col) (member rank (list 40 39 38 0))
           (eq? (find-its-rank country 4 col) 100))
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

  (if #t ; for debug
      
  (for* ([belong-to (list up down left right)]
         [rank whole-rank-set])
      
         (get-from (country row col) (assign-country-row-col belong-to rank))
    
         (occupy country row col rank belong-to 'normal)
   )  

  (begin
  (occupy down 5 1 10 down 'normal)
  (occupy right 5 1 10 right 'normal)
  (occupy down 0 2 38 down 'normal)
  (occupy down 1 1 0 down 'normal)
  (occupy up 3 4 0 right 'normal)
  (occupy down 4 1 35 down 'normal)
  (occupy right 0 0 35 right 'normal)
  (occupy right 1 0 34 right 'normal)
  (occupy down 0 4 0 down 'normal)
  (occupy up 0 2 33 up 'normal)
  (occupy up 5 1 10 up 'normal)
  (occupy up 4 1 100 up 'normal)
  (occupy up 4 0 100 up 'normal)
  
  )

  )
)

; ===================================================================

(define (occupy country row col rank belong-to state)
   (add occupied-list (list country row col rank belong-to state))   
)

(define (same-country-row-col? country row col)
  (lambda (lst)
     (and (eq? (first lst) country)
          (eq? (second lst) row)
          (eq? (third lst) col)))
)

(define (same-rank? rank)
  (lambda (lst)
    (eq? (fourth lst) rank))
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

(define (find-its-rank country row col) ; find the rank based on the coordinates
  
  (define item (find-whole-chess country row col))
  
  (if item (fourth item) #f)
)

(define (occupied? country row col) 
      (if (find-whole-chess country row col) #t #f))

(define (find-belong-to belong-to)
        (filter (same-belong-to? belong-to) occupied-list)
)

(define (find-rank rank)
        (filter (same-rank? rank) occupied-list)
)  

(define (delete-country belong-to) ; delete everything of a country
  (set! occupied-list
        (filter-not (same-belong-to? belong-to) occupied-list)))
        
(define (empty? belong-to)
  (null? (find-belong-to belong-to))
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

; ====================================================

(define (go-to-next-country)

  (set! which-turn (right-country which-turn))

   (if (empty? which-turn)
       (go-to-next-country)
       ; else
       (when (eq? (player which-turn) 'computer)
           (re-draw)
;           (sleep 0.7)
           (computer-run which-turn)
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

