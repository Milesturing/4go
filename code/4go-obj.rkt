#lang racket

(require "4go-def.rkt")

(provide one-position% one-chess% chess-board%
         get-belong-to get-rank occupied?
         move-able? in-camp? is-flag? is-labor?
         exist? not-exist?
         set-position get-position
         find-picked-up fight-able fight-result)

; ===================================================================

(define one-position%

  (class object%

    (super-new)
    (init-field [country null])
    (init-field [row null])
    (init-field [col null])
      
    (define/public (get-values)
      (values country row col)
    )

    (define/public (get-country)
      country
    )

    (define/public (get-row)
      row
    )

    (define/public (get-col)
      col
    )

    (define (valid?)
      (valid? country row col)
    )

    (define/public (in-camp?)
      (is-camp? country row col)
    )

    (define/public (in-base?)
      (is-base? country row col)
    )

    (define/public (on-rail?)
      (on-rail? country row col)
    )
    
 )        
)
   
; ===================================================================
  
(define one-chess%

  (class one-position%

    (super-new)
    (inherit-field country)
    (inherit-field row)
    (inherit-field col)
    (init-field [rank null])
    (init-field [belong-to null])
    (init-field [state null])

    (define/public (get-position)
      (set-position (list country row col))
    )
       
    (define/public (get-rank)
      rank
    )

    (define/public (get-belong-to)
      belong-to
    )

    (define/public (get-state)
      state
    )

    (define/public (get-rest)
      (values rank belong-to state)
    )
    
    (define/public (get-full-values)
      (values country row col rank belong-to state)
    )

    (define/public (is-labor?)
      (= rank 30)
    )

    (define/public (is-flag?)
      (= rank 10)
    )

    (define/public (is-mine?)
      (= rank 100)
    )

    (define/public (is-bomb?)
      (= rank 0)
    )
    
    (define/public (move-able?)
      (and (not (send this in-base?))
           (not (is-flag?))
           (not (is-mine?))
      )
    )

    (define/public (draw dc)
      (draw-chess dc country row col (rank-code rank) belong-to state)
    )
    
  )
  
)


; ===================================================================

(define chess-board% ; define a new class
  
  (class object%
    
    (super-new)
    (init-field [occupied-list null])

    (define/public (get-occupied-list)
         occupied-list
    )

    (define/public (set-occupied-list x)
         (set! occupied-list x)
    )

    (define/public (draw-all-chesses dc)      
         (for ([item occupied-list])

              (send (set-chess item) draw dc)
           
         )
    )

    (define/public (occupy chess pos state) ; occupy a position with the rank and belong-to of a chess

         (define-values (_c _r _l rank belong-to _s) (send chess get-full-values))
         (define-values (country row col) (send pos get-values))
                                          
         (set! occupied-list
               (cons (list country row col rank belong-to state)
                     occupied-list
               ))   
     )

    (define/public (throw country row col rank belong-to)

       (occupy (set-chess (list #f #f #f rank belong-to #f))
               (set-position (list country row col))
               'normal
       )

     )

     (define/private (same-country? country)
          (lambda (lst)
                  (eq? (first lst) country))
     )

     (define/public (same-rank? rank)
          (lambda (lst)
              (eq? (fourth lst) rank))
     )

     (define/private (same-belong-to? belong-to)
          (lambda (lst)
              (eq? (fifth lst) belong-to))
     )

     (define/private (same-country-row-col? position) ; private
          (lambda (lst)
               (and (eq? (first lst)  (send position get-country))
                    (eq? (second lst) (send position get-row))
                    (eq? (third lst)  (send position get-col))
               )
          )
     )

  
     (define/public (find-picked-up)
          (set-chess  
               (findf
                  (lambda (lst)
                      (eq? (sixth lst) 'picked-up)
                  ) occupied-list)
          )
     )

     (define/public (find-country country)
          (filter
             (same-country? country)
           occupied-list)
     )

     (define/public (find-whole-chess position)

          (if position
              (set-chess
                (findf (same-country-row-col? position) occupied-list)
              )
          ; else
              #f
          )
     )

    (define/public (change position new-list) ; use #f to denote values not to be changed
                                                     ; example: (change pos (list #f #f #f #f #f 'picked-up))

          (when position

            (define chosen (findf (same-country-row-col? position) occupied-list))
            (when chosen

              (define the-rest (filter-not (same-country-row-col? position) occupied-list))

              (set! chosen (map (lambda (a b) (or a b)) new-list chosen))
              (set! occupied-list

                    (append the-rest (list chosen))
               
              )
           )
            
          )
    )

    (define/public (change-state position new-state) ; call the above method

        (change position (list #f #f #f #f #f new-state))

    )
                             
    (define/public (delete-occupied position)
  
        (set! occupied-list
             (filter-not (same-country-row-col? position) occupied-list)
        )
       
     )


    (define/public (find-belong-to belong-to)
         (filter (same-belong-to? belong-to) occupied-list)
    )

    (define/public (find-rank rank)
         (filter (same-rank? rank) occupied-list)
    )  

    (define/public (delete-nation chess) ; delete everything of the nation that a chess belongs to
        (set! occupied-list
             (filter-not (same-belong-to? (send chess get-belong-to)) occupied-list))
    )
        
    (define/public (is-empty? belong-to)
        (null? (find-belong-to belong-to))
    )

  
))    

; ===================================================================
(define (set-position a-list) ; private
  
      (if a-list
          (new one-position%
                         [country (first a-list)]
                         [row (second a-list)]
                         [col (third a-list)]
         )
      ; else
         #f
      )
)

(define (set-chess a-list) ; private

      (if a-list
          (new one-chess%
                         [country (first a-list)]
                         [row (second a-list)]
                         [col (third a-list)]
                         [rank (fourth a-list)]
                         [belong-to (fifth a-list)]
                         [state (sixth a-list)]
         )
      ; else
         #f
      )
)

(define ((occupied? board) country row col)

       (if (send board find-whole-chess (set-position (list country row col))) #t #f)

)

(define (get-belong-to chess)
        (send chess get-belong-to)
)

(define (get-rank chess)
        (send chess get-rank)
)

(define (get-position chess)
        (send chess get-position)
)

(define (move-able? chess)
        (if chess
            (send chess move-able?)
            ; else
            #f
        )
)

(define (in-camp? chess)
        (if chess
            (send chess in-camp?)
            ; else
            #f
        )
)

(define (is-flag? chess)

  (send chess is-flag?)

)

(define (is-labor? chess)

  (send chess is-labor?)

)

(define (exist? chess)
        (if chess #t #f)
)

(define (not-exist? chess)
        (if chess #f #t)
)

(define (find-picked-up board)
        (send board find-picked-up)
)

(define (fight-able chess1 chess2)

  (and (enemy? (send chess1 get-belong-to)
               (send chess2 get-belong-to))
       (not (send chess2 in-camp?))
  )

)

(define (fight-result chess1 chess2)

  (beat-it? (send chess1 get-rank) (send chess2 get-rank))

)




     


