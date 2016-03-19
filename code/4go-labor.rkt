#lang racket

(require "4go-def.rkt")

(provide labor-fly)

; labor-fly modules
;=================================================================================================  

(define (neighbours-on-rail country row col) ; successful, do not touch it
    (define they null)
    (define neighbours 
         (list (list country (add1 row) col)
               (list country (sub1 row) col)
               (list country row (add1 col))
               (list country row (sub1 col))
         ))  
    (if (and (not-middle country) (= row 0) (= col 0))
       (set! neighbours (cons (list (left-country country) 0 4) neighbours))
     )  
    (if (and (not-middle country) (= row 0) (= col 4))
       (set! neighbours (cons (list (right-country country) 0 0) neighbours))
     )
  (if (and (not-middle country) (= row 0) (even? col))
      (set! neighbours (cons
           (cond [(eq? country down) (list middle 2 (/ col 2))]
                    [(eq? country up) (list middle 0 (- 2 (/ col 2)))]
                    [(eq? country left) (list middle (/ col 2) 0)]
                    [(eq? country right) (list middle (- 2 (/ col 2)) 2)]
                    ) neighbours))
   )
     (if (eq? country middle)
        (for* ([country2 (list down up left right)]
               [col2 (list 0 2 4)])
          (if (member (list country row col) (neighbours-on-rail country2 0 col2))
             (set! neighbours (cons (list country2 0 col2) neighbours))             
          ))
     )        
    (if (not (on-rail country row col)) null
       (set! they (filter (lambda (x) (and (apply on-rail x) (apply valid? x)))
                              neighbours))
    )
   they
)  
        
;=================================================================================================  
  
(define (labor-fly occupied? country row col country2 row2 col2) ; successful, do not touch it
; if the chess is a "labor", on the rail assumed
; a very hard to code module, requires lots of endeavor
  
  (define (search-next-step fly-route-list)
    
    (define chosen (list (list country row col)))
    (define new-route null)
    (define new-list null)
    (define final-pos null)
    (define quit #f)  
    
    (for* ([route fly-route-list])
     #:break quit
        (set! final-pos (last route))

        (if (equal? final-pos (list country2 row2 col2))
           (begin
               (set! chosen route)
               (set! quit #t)
           )
           ; else
           (for* ([next-pos (apply neighbours-on-rail final-pos)])
               (set! new-route (append route (list next-pos)))
            ; else if
              (if (or (and (apply occupied? next-pos) (not (equal? next-pos (list country2 row2 col2)))) 
                       (member next-pos route)) ; have to ensure the route does not pass the same place twice
               null
               (set! new-list (append new-list (list new-route)))
           )))
     )
    
  (if quit
       chosen 
       (if (null? new-list) (list (list country row col)) 
          (search-next-step new-list)
          )
  ))
  
  (if (and (on-rail country row col) (on-rail country2 row2 col2)) ; to check if on the rail
       (search-next-step (list (list (list country row col))))
   )    
) 

;=================================================================================================  
