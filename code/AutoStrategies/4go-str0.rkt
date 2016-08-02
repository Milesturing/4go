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

;  (get-from (country row col rank belong-to) chess)
    
  (when (and (exist? chess) (not (in-camp? chess)))

    (define enemy-chesses

       (append (send board find-belong-to (right-country belong-to))
               (send board find-belong-to (left-country belong-to)))

    )

    (for* [(e-chess enemy-chesses)]
       #:break quit

      (get-from (e-country e-row e-col e-rank) e-chess)

      (when (and (movable? e-rank) (or (= (beat-it? e-rank rank) 1) (= e-rank 0)))

         (define move-list (route-list board occupied? e-chess c-pos))      

         (define accessible (> (length move-list) 1))

         (when accessible

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

  (define all-chess (send board find-belong-to belong-to))
  (define sum 0)


  (when (not (send board is-empty? belong-to)) 
 

  (for* ([chess all-chess])

    (get-from (country row col rank) chess)

    (set! sum (+ sum (score rank)))
   
    ;

    (if (is-camp? country row col) (set! sum (add1 sum)))
     
   ) ; for

   (define flag-list (filter (send board same-rank? 10) all-chess))

   (when (not (null? flag-list))

   (define flag (car flag-list))

   (get-from (_ _2 flag-col) flag)

   (define (extra-score e-row e-col e-score)

     (when ((occupied? board) belong-to e-row e-col)

       (get-from (_ _2 _3 _4 which-side) (send board find-whole-chess belong-to e-row e-col))

       (if (enemy? which-side belong-to)

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

    (define whole-list (send board find-belong-to belong-to))
    (define one-move null)
    (define value 0)
    (define max-value -10000)
    (define save-occupied null)
    (define ratio (/ (/ (+ (score 39) (score 38)) 2) (score 40)) )
  
    (for* ([some-chess whole-list])

      (get-from (s-country s-row s-col s-rank s-belong-to) some-chess)
      
      (if (and (movable? s-rank) (not (is-base? s-country s-row s-col)))
  
      (for* ([d-country (list middle up left down right)]
             [d-row (range (row-num d-country))]
             [d-col (range (col-num d-country))])

         (get-from (_ _2 _3 d-rank d-belong-to) (send board find-whole-chess d-country d-row d-col))

         (define accessible #f)

         (if (and (is-labor? s-rank) (not ((occupied? board) d-country d-row d-col))
                   (not (and (not (send board is-empty? d-belong-to)) (>= d-row 4)))
             )

             (set! accessible #f)

             (begin
               (define move-list (route-list board occupied? s-chess d-pos))
               (set! accessible (> (length move-list) 1))
              )
         )
        

         (define goable   (or (not ((occupied? board) d-country d-row d-col))
                          (and ((occupied? board) d-country d-row d-col)
                               (enemy? s-belong-to d-belong-to)
                               (not (is-camp? d-country d-row d-col))
                               (not (and (is-base? d-country d-row d-col)
                                         (not (eq? d-rank 10))))
                              )))

         (when (and accessible goable)

             (set! save-occupied (send board get-occupied-list))

             (send board delete-occupied s-pos)

             (if (not ((occupied? board) d-country d-row d-col))

                  (send board occupy s-chess d-pos 'normal)
                 
                  ; else

                 (match (beat-it? s-rank d-rank)
                        ([== 1] (begin
                                  (send board delete-occupied d-pos)
                                  (send board occupy s-chess d-pos 'normal)
                                 ))
                        ([== 0] (send board delete-occupied d-pos))
                        ([== -1] null)
                   )
                 )

              (set! value (- (+ (calculate-value board belong-to)
                                (calculate-value board (right-country (right-country belong-to))))
                             (+ (calculate-value board (right-country belong-to))
                                (calculate-value board (left-country belong-to)))))

           

              (when (and (not (is-labor? s-rank)) (under-attack board (send board find-whole-chess d-country d-row d-col)))

                  (set! value (- value (* ratio (score s-rank))))

              )


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