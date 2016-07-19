#lang racket

(require "4go-def.rkt")

(provide chess-board%)

; ===================================================================

(define chess-board% ; define a new class
  
  (class object%
    
    (super-new)
    (init-field [occupied-list null])

    (define/public (get-occupied-list)
         occupied-list
    )

    (define/public (set-occupied-list s)
         (set! occupied-list s)
    )

    (define/public (draw-all-chesses dc)      
         (for ([item occupied-list])
    
              (get-from (country row col rank belong-to state) item)    
              (draw-chess dc country row col (rank-code rank) belong-to state)
         )
    )

    (define/public (occupy country row col rank belong-to state)
         (set! occupied-list
               (cons (list country row col rank belong-to state)
                     occupied-list
               ))   
     )

     (define/private (same-country? country)
          (lambda (lst)
                  (eq? (first lst) country))
     )

     (define/private (same-country-row-col? country row col)
          (lambda (lst)
               (and (eq? (first lst) country)
                    (eq? (second lst) row)
                    (eq? (third lst) col)))
     )

     (define/public (same-rank? rank)
          (lambda (lst)
              (eq? (fourth lst) rank))
     )

     (define/private (same-belong-to? belong-to)
          (lambda (lst)
              (eq? (fifth lst) belong-to))
     )

     (define/public (delete-occupied country row col)
          (set! occupied-list
               (filter-not (same-country-row-col? country row col) occupied-list)
     ))
  
     (define/public (find-picked-up)
          (findf
             (lambda (lst)
                 (eq? (sixth lst) 'picked-up)
             ) occupied-list)
     )

     (define/public (find-country country)
          (filter
             (same-country? country)
           occupied-list)
     )

    (define/public (find-whole-chess country row col)
         (findf
             (same-country-row-col? country row col)
          occupied-list)
    )

    (define/public (find-its-rank country row col) ; find the rank based on the coordinates
         (define item (find-whole-chess country row col))
  
         (if item (fourth item) #f)
    )

    (define/public occupied?
         (lambda (country row col) 
                 (if (find-whole-chess country row col) #t #f)
         )
    )  

    (define/public (find-belong-to belong-to)
         (filter (same-belong-to? belong-to) occupied-list)
    )

    (define/public (find-rank rank)
         (filter (same-rank? rank) occupied-list)
    )  

    (define/public (delete-country belong-to) ; delete everything of a country
        (set! occupied-list
             (filter-not (same-belong-to? belong-to) occupied-list))
    )
        
    (define/public (is-empty? belong-to)
        (null? (find-belong-to belong-to))
    )

))    

; ===================================================================

