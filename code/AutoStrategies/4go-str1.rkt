#lang racket

(require "../4go-def.rkt"
         "../4go-route.rkt"
         "../4go-obj.rkt"
)

(provide strategy1)

; ====================================================

(define (under-attack board chess enemy-chesses)

    (define result 0)
  
    (define quit #f)

    (for* [(e-chess enemy-chesses)]
       #:break quit

      (when (move-able? e-chess)
  
        (define beat? (fight-result e-chess chess))
 
         (when (= beat? 1)

           (define accessible (access? board e-chess (get-position chess)))      
           
           
           (when accessible
              (set! result (- (score (get-rank chess))))
              (set! quit #t) ; quit for loop
           )
         )

         (when (= beat? 0)

           (define accessible (access? board e-chess (get-position chess)))      
           
           (when accessible
              (set! result (- (score (get-rank e-chess)) (score (get-rank chess))))
              (if (> result 0) (set! result 0))
              (set! quit #t) ; quit for loop
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
        ([== 0]  23)
        ([== 30] 7)
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

  (define all-chess (find-belong-to board belong-to)) ; all chesses belonging to this side
  (define enemies (find-all-enemies board belong-to))
  (define sum 0)

  (when (not (null? all-chess)) ; if the side is not empty
 

  (for ([chess all-chess]) ; for every chess in this side

    (set! sum (+ sum (score (get-rank chess))))

    (when (and (exist? chess) (not (in-camp? chess)))

      (set! sum (+ sum (* ratio (under-attack board chess enemies))))
     
    )

     
   ) ; for

   )

  sum

)



(define (calculate-position-value board belong-to)

  (define all-chess (find-belong-to board belong-to)) ; all chesses belonging to this side
  (define sum 0)

  (when (not (null? all-chess)) ; if the side is not empty
 
   (define flag-list (filter-with (list #f #f #f 10 #f #f) all-chess)) ; rank = 10

   (when (not (null? flag-list))

     (define flag (car flag-list))

     (define flag-col (get-col flag))

     (define my-all-chess (find-country board belong-to))

     (for ([my-chess my-all-chess])

        (define m-row (get-row my-chess))
        (define m-col (get-col my-chess))
        (define m-rank (get-rank my-chess))
        (define m-belong-to (get-belong-to my-chess))
       
        (when m-belong-to

          (define sign (if (enemy? m-belong-to belong-to) -1 1/2))

          (set! sum (+ sum (* sign 1/12 (position-score m-rank) (position-value m-row m-col flag-col))))

        )
      
     ) ; for
     
    )
    )

   sum

)

; ====================================================


(define (strategy1 board belong-to)

    (define whole-list (find-belong-to board belong-to))
    (define one-move null)
    (define value 0)
    (define max-value -10000)
    (define save-occupied null)

    (for* ([s-chess whole-list]) ; choose all possible chesses
      
      (when (move-able? s-chess)

      (define s-pos (get-position s-chess))
  
      (for* ([d-country (list middle up left down right)] ; d is destination
             [d-row (range (row-num d-country))]
             [d-col (range (col-num d-country))])

        (define d-pos (set-position (list d-country d-row d-col)))
        (define d-chess (find-whole-chess board d-pos))
        
        (define move-list (route-list board s-chess d-pos))
        (define accessible (> (length move-list) 1))

        (when accessible ; filter availability

          (define goable   (and (or (and (is-flag? d-chess) (exist? d-chess))
                                   (not (in-base? d-pos))
                               )
                              (or (not (exist? d-chess))
                                  (and (exist? d-chess) (fight-able s-chess d-chess))
                              )
                          )) ; conditions

          (when goable ; filter those positions that are available only

             (set! save-occupied (get-occupied-list board))

             (delete-occupied board s-pos)

             (if (not-exist? d-chess)
                    
                        (send board occupy s-chess d-pos 'normal)
             
                  ; else

                 (match (fight-result s-chess d-chess)
                        ([== 1] (begin
                                  (send board delete-occupied d-pos)
                                  (send board occupy s-chess d-pos 'normal)
                                 ))
                        ([== 0] (send board delete-occupied d-pos))
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
                    (set! one-move (list s-pos d-pos))
              )
                  
              (set-occupied-list board save-occupied)
            )
           )
        )

    ))

  
    one-move

  
)