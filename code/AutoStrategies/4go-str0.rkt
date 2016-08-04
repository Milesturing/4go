#lang racket

(require "../4go-def.rkt"
         "../4go-route.rkt"
         "../4go-obj.rkt"
)

(provide strategy0) ; strategy

; ====================================================

(define (under-attack board chess)

  (define result #f)
  (define quit #f)

  (when (and (exist? chess) (not (in-camp? chess)))

    (define enemy-chesses (find-all-enemies board (get-belong-to chess)))
                           
    (for* [(e-chess enemy-chesses)]
       #:break quit

      (when (and (move-able? e-chess) (or (= (fight-result e-chess chess) 1) (is-bomb? e-chess)))

         (define move-list (route-list board e-chess (get-position chess)))      

         (when (> (length move-list) 1)

              (set! result #t)
              (set! quit #t)
           
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


(define (calculate-value board belong-to)

  (define all-chess (find-belong-to board belong-to))
  (define sum 0)

  (unless (null? all-chess)
 

  (for ([chess all-chess])

    (set! sum (+ sum (score (get-rank chess) )))

    (if (in-camp? chess) (set! sum (add1 sum)))
     
   ) ; for

   (define flag-list (filter-with (list #f #f #f 10 #f #f) all-chess)) ; rank = 10

   (unless (null? flag-list)

   (define flag (car flag-list))

   (define flag-col (get-col flag))

   (define (extra-score e-row e-col e-score)

     (define e-pos (set-position (list belong-to e-row e-col)))

     (define e-chess (find-whole-chess board e-pos))  
                                
     (when (exist? e-chess)

       (define which-side (get-belong-to e-chess))

       (when (enemy? which-side belong-to)

           (set! sum (- sum e-score))
           
       ))
     
    )

   (extra-score 3 flag-col 65)
   (extra-score 4 flag-col 90)
   (extra-score 5 (add1 flag-col) 90)
   (extra-score 5 (sub1 flag-col) 90)
   (extra-score 2 2 40)
   (extra-score 3 (- 4 flag-col) 10)  
   (extra-score 4 (add1 flag-col) 10)
   (extra-score 4 (sub1 flag-col) 10)
   (extra-score 1 1 3)
   (extra-score 1 3 3)
     
    )
    )
  
   sum

)


(define (strategy0 board belong-to)

    (define whole-list (find-belong-to board belong-to))
    (define one-move null)
    (define value 0)
    (define max-value -10000)
    (define save-occupied null)
    (define ratio (/ (/ (+ (score 39) (score 38)) 2) (score 40)) )
  
    (for* ([s-chess whole-list])
      
      (when (move-able? s-chess)

      (define s-pos (get-position s-chess))
        
      (for* ([d-country (list middle up left down right)]
             [d-row (range (row-num d-country))]
             [d-col (range (col-num d-country))])

         (define d-pos (set-position (list d-country d-row d-col)))
         (define d-chess (find-whole-chess board d-pos))
  
         (define goable   (and (or (and (is-flag? d-chess) (exist? d-chess))
                                   (not (in-base? d-pos))
                               )
                              (or (not (exist? d-chess))
                                  (and (exist? d-chess) (fight-able s-chess d-chess))
                              )
                          )) ; conditions

         (when goable ; first filter those positions that are available only

           (define move-list (route-list board s-chess d-pos))
           (define accessible (> (length move-list) 1))

           (when accessible ; second filter availability

             (set! save-occupied (get-occupied-list board)) ; save it

             (delete-occupied board s-pos)

             (define case 0)

             (if (not-exist? d-chess)

                  (begin
                    
                        (send board occupy s-chess d-pos 'normal)
                        (set! case 1)

                  )
                 
                  ; else

                 (match (fight-result s-chess d-chess)
                        ([== 1] (begin
                                  (send board delete-occupied d-pos)
                                  (send board occupy s-chess d-pos 'normal)
                                  (set! case 1)
                                 ))
                        ([== 0] (send board delete-occupied d-pos))
                        ([== -1] null)
                   )
              )

              (set! value (- (+ (calculate-value board belong-to)
                                (calculate-value board (right-country (right-country belong-to))))
                             (+ (calculate-value board (right-country belong-to))
                                (calculate-value board (left-country belong-to)))))

           

              (when (and (= case 1) (under-attack board (find-whole-chess board d-pos)))

                  (set! value (- value (* ratio (score (get-rank s-chess)))))

              )


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