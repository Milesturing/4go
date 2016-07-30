#lang racket

(require "../4go-def.rkt"
         "../4go-route.rkt"
         "../4go-obj.rkt"
)

(provide strategy1)

; ====================================================

(define (under-attack board chess)

  (define result 0)

  (define (board-occupied? x y z) (send board occupied? x y z))

  (get-from (country row col rank belong-to) chess)
  
  (when (and chess (not (is-camp? country row col)))

    (define enemy-chesses

       (append (send board find-belong-to (right-country belong-to))
               (send board find-belong-to (left-country belong-to)))

    )
    
    (define quit #f)
    
    (for* [(e-chess enemy-chesses)]
       #:break quit

      (get-from (e-country e-row e-col e-rank) e-chess)

      (when (movable? e-rank)

         (define beat? (beat-it? e-rank rank))
        
         (when (and (= beat? 1) (not (= rank 100)))

           (define move-list (route-list board-occupied? e-country e-row e-col e-rank country row col))      
           (define accessible (> (length move-list) 1))

           (when accessible
              (set! result (- (score rank)))
              (set! quit #t) ; quit for loop
           )
         )

         (when (= beat? 0)


           
           (define move-list (route-list board-occupied? e-country e-row e-col e-rank country row col))      
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

(define (position-score rank)
  
      (match rank
        ([== 40] 30)
        ([== 39] 25)
        ([== 38] 20)
        ([== 37] 15)
        ([== 36] 10)
        ([== 35] 8)
        ([== 0]  22)
        (else    5)
       )
)

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

(define ratio 0.35) ; a constant

(define (calculate-value board belong-to)

  (define all-chess (send board find-belong-to belong-to)) ; all chesses belonging to this side
  (define sum 0)


  (when (not (send board is-empty? belong-to)) ; if the side is not empty
 

  (for* ([chess all-chess]) ; for every chess in this side

    (get-from (country row col rank) chess)

    (set! sum (+ sum (score rank)))

    (when (not (is-labor? rank))

          (set! sum (+ sum (* ratio (under-attack board chess))))

    )
     
   ) ; for

   )

  sum

)

;

(define (calculate-position-value board belong-to)

  (define all-chess (send board find-belong-to belong-to)) ; all chesses belonging to this side
  (define sum 0)

  (when (not (send board is-empty? belong-to)) ; if the side is not empty
 
   (define flag-list (filter (send board same-rank? 10) all-chess))

   (when (not (null? flag-list))

     (define flag (car flag-list))

     (get-from (_c _r flag-col) flag)

     (define my-all-chess (send board find-country belong-to))

     (for* ([my-chess my-all-chess])

        (get-from (_cc m-row m-col m-rank m-belong-to) my-chess)

       
        (when m-belong-to

          (define sign (if (enemy? m-belong-to belong-to) -1 1/2))

          (set! sum (+ sum (* sign 1/10 (position-score m-rank) (position-value m-row m-col flag-col))))

        )
      
     ) ; for
     
    )
    )

   sum

)

; ====================================================


(define (strategy1 board belong-to)

    (define whole-list (send board find-belong-to belong-to))
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

         (get-from (_c _r _l d-rank d-belong-to) (send board find-whole-chess d-country d-row d-col))

         (define accessible #f)

         (if (and (is-labor? s-rank) (not (send board occupied? d-country d-row d-col)) (not (>= d-row 4)))

             (set! accessible #f)

             ; else

             (begin
               (define (board-occupied? x y z) (send board occupied? x y z))
               
               (define move-list (route-list board-occupied? s-country s-row s-col s-rank d-country d-row d-col))
               (set! accessible (> (length move-list) 1))
             )
         )

         (define go-able   (or (not (send board occupied? d-country d-row d-col))
                          (and (send board occupied? d-country d-row d-col)
                               (enemy? s-belong-to d-belong-to)
                               (not (is-camp? d-country d-row d-col))
                               (not (and (is-base? d-country d-row d-col)
                                         (not (eq? d-rank 10))))
                              )))

         (when (and accessible go-able)

             (set! save-occupied (send board get-occupied-list))

             (send board delete-occupied s-country s-row s-col)

             (if (not (send board occupied? d-country d-row d-col))

                  (send board occupy d-country d-row d-col s-rank s-belong-to 'normal)
                 
                  ; else

                 (match (beat-it? s-rank d-rank)
                        ([== 1] (begin
                                  (send board delete-occupied d-country d-row d-col)
                                  (send board occupy d-country d-row d-col s-rank s-belong-to 'normal)
                                 ))
                        ([== 0] (send board delete-occupied d-country d-row d-col))
                        ([== -1] null)
                   )
                 )

           
              (set! value (- (+ (calculate-value board belong-to)
                                (calculate-position-value board belong-to)
                                (calculate-value board (right-country (right-country belong-to))))
                             (+ (calculate-value board (right-country belong-to))
                                (calculate-value board (left-country belong-to)))))
           

              (set! value (+ value (random 5)))
   
              (when (>= value max-value)

                    (set! max-value value)
                    (set! one-move (list s-country s-row s-col d-country d-row d-col))
              )
                  
              (send board set-occupied-list save-occupied)
            )
        )

    ))


    one-move

  
)