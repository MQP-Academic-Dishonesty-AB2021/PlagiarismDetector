

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |CS 1102 Assignment #4 finaldraft|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 900) 
(define MTS (empty-scene SIZE SIZE))













(define-struct board-state (los difficulty players-turn?))

(define-struct spc (color char taken?))

(define spc1-1 (make-spc "white" "" false))
(define spc1-2 (make-spc "white" "" false))
(define spc1-3 (make-spc "white" "" false))
(define spc2-1 (make-spc "white" "" false))
(define spc2-2 (make-spc "white" "" false))
(define spc2-3 (make-spc "white" "" false))
(define spc3-1 (make-spc "white" "" false))
(define spc3-2 (make-spc "white" "" false))
(define spc3-3 (make-spc "white" "" false))

(define blank-board (list (list spc1-1 spc1-2 spc1-3) (list spc2-1 spc2-2 spc2-3) (list spc3-1 spc3-2 spc3-3)))






(define-struct abstract-board (board x y))



(define (spc-conversion spc)
  (cond
    [(string=? "X" (spc-char spc)) 1]
    [(string=? "O" (spc-char spc)) 2]
    [else 0]
    ))





(define (state-to-abstract bs)
  (local [(define (get-pos temp-bs col row) 
            (list-ref (list-ref (board-state-los temp-bs) col) row))]
    (list
     (list (spc-conversion (get-pos bs 0 0))
           (spc-conversion (get-pos bs 0 1))
           (spc-conversion (get-pos bs 0 2))) 
     (list (spc-conversion (get-pos bs 1 0))
           (spc-conversion (get-pos bs 1 1))
           (spc-conversion (get-pos bs 1 2)))
     (list (spc-conversion (get-pos bs 2 0))
           (spc-conversion (get-pos bs 2 1))
           (spc-conversion (get-pos bs 2 2))))))






(define (abstract-win lol team)
  (local [(define (col-win temp-lol counter)
            (if (not (> counter 2))
                (if (= team
                       (list-ref (list-ref temp-lol 0) counter)
                       (list-ref (list-ref temp-lol 1) counter)
                       (list-ref (list-ref temp-lol 2) counter))
                    true
                    (col-win temp-lol (+ counter 1))
                    )
                (row-win temp-lol 0)
                ))
           
          (define (row-win temp-lol counter)
            (if (not (> counter 2))
                (if 
                 (= team
                    (list-ref (list-ref temp-lol counter) 0)
                    (list-ref (list-ref temp-lol counter) 1)
                    (list-ref (list-ref temp-lol counter) 2))
                 true
                 (row-win temp-lol (+ counter 1)))
                (diagonal-win temp-lol)))
          
          (define (diagonal-win temp-lol)
            (cond
              [(= team (list-ref (list-ref temp-lol 0) 0) (list-ref (list-ref temp-lol 1) 1) (list-ref (list-ref temp-lol 2) 2)) true]
              [(= team (list-ref (list-ref temp-lol 2) 0) (list-ref (list-ref temp-lol 1) 1) (list-ref (list-ref temp-lol 0) 2)) true]
              [else false]))]
    (col-win lol 0)))




(define (abstract-try-place-opponent lol col row)
  (cond
    [(= col 0)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (abstract-build-row-opponent lol 0 0) (list-ref lol 1) (list-ref lol 2))] 
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (abstract-build-row-opponent lol 0 1) (list-ref lol 1) (list-ref lol 2))] 
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (abstract-build-row-opponent lol 0 2) (list-ref lol 1) (list-ref lol 2))] 
       [else lol]
       )]    
    [(= col 1)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (list-ref lol 0) (abstract-build-row-opponent lol 1 0) (list-ref lol 2))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (list-ref lol 0) (abstract-build-row-opponent lol 1 1) (list-ref lol 2))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (list-ref lol 0) (abstract-build-row-opponent lol 1 2) (list-ref lol 2))]
       [else lol]
       )]
    [(= col 2)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-opponent lol 2 0))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-opponent lol 2 1))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-opponent lol 2 2))]
       [else lol]
    
       )]
    [else lol]
    )
  )



(define (abstract-try-place-player lol col row)
  (cond
    [(= col 0)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (abstract-build-row-player lol 0 0) (list-ref lol 1) (list-ref lol 2))] 
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (abstract-build-row-player lol 0 1) (list-ref lol 1) (list-ref lol 2))] 
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (abstract-build-row-player lol 0 2) (list-ref lol 1) (list-ref lol 2))] 
       [else lol]
       )]    
    [(= col 1)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (list-ref lol 0) (abstract-build-row-player lol 1 0) (list-ref lol 2))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (list-ref lol 0) (abstract-build-row-player lol 1 1) (list-ref lol 2))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (list-ref lol 0) (abstract-build-row-player lol 1 2) (list-ref lol 2))]
       [else lol]
       )]
    [(= col 2)
     (cond
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 0)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-player lol 2 0))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 1)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-player lol 2 1))]
       [(and (= (list-ref (list-ref lol col) row) 0) (= row 2)) (list (list-ref lol 0) (list-ref lol 1) (abstract-build-row-player lol 2 2))]
       [else lol]
    
       )]
    [else lol]
    )
  )


   


(define (abstract-build-row-opponent los col row)
  (cond
    [(= row 0) (list 2 (list-ref (list-ref los col) 1) (list-ref (list-ref los col) 2))]
    [(= row 1) (list (list-ref (list-ref los col) 0) 2 (list-ref (list-ref los col) 2))]
    [(= row 2) (list (list-ref (list-ref los col) 0) (list-ref (list-ref los col) 1) 2)]
    [else empty]
    )
  )



(define (abstract-build-row-player los col row)
  (cond
    [(= row 0) (list 1 (list-ref (list-ref los col) 1) (list-ref (list-ref los col) 2))]
    [(= row 1) (list (list-ref (list-ref los col) 0) 1 (list-ref (list-ref los col) 2))]
    [(= row 2) (list (list-ref (list-ref los col) 0) (list-ref (list-ref los col) 1) 1)]
    [else empty]
    )
  )



(define (make-test-board ab col row)
  (if (> col 2)
      empty
      (if (not (> row 2))
          (cons (make-abstract-board (abstract-try-place-opponent (state-to-abstract ab) col row) col row) (make-test-board ab col (+ row 1)))
          (make-test-board ab (+ col 1) 0)))
  )



(define (make-test-board-player ab col row)
  (if (> col 2)
      empty
      (if (not (> row 2))
          (cons (make-abstract-board (abstract-try-place-player (abstract-board-board ab) col row) col row) (make-test-board-player ab col (+ row 1)))
          (make-test-board-player ab (+ col 1) 0)
          )
      )
  )

	


(define (determine-move ab depth)
  (cond
    [(= depth 1) (if (not (empty? (filter (lambda (x) (abstract-win (abstract-board-board x) 2)) (make-test-board ab 0 0))))
                     (first (filter (lambda (x) (abstract-win (abstract-board-board x) 2)) (make-test-board ab 0 0)))
                     empty
                     )]
    [(= depth 2)
     (if (not (empty? (filter (lambda (x) (abstract-win (abstract-board-board x) 2)) (make-test-board ab 0 0))))
         (filter (lambda (x) (abstract-win (abstract-board-board x) 2)) (make-test-board ab 0 0))
         (if (not (empty? (map (lambda (x)
                                 (filter (lambda (y) (abstract-win (abstract-board-board y) 1)) x))
                               (map (lambda (x) (make-test-board-player x -1 0)) (make-test-board ab -1 0)))))
             (first (map (lambda (x)
                           (filter (lambda (y) (abstract-win (abstract-board-board y) 1)) x))
                         (map (lambda (x) (make-test-board-player x -1 0)) (make-test-board ab -1 0))))
             empty
             )
         )]
    [else ab]
    )
  )



(define (draw-board bs)
  (place-image (rectangle (* (/ SIZE 6) 5) (/ SIZE 60) "solid" "Orange") (/ SIZE 2) (/ SIZE 3) 
               (place-image (rectangle (* (/ SIZE 6) 5) (/ SIZE 60) "solid" "Orange") (/ SIZE 2) (* (/ SIZE 3) 2) 
                            (place-image (rectangle (/ SIZE 60) (* (/ SIZE 6) 5) "solid" "Orange") (* (/ SIZE 3) 2) (/ SIZE 2) 
                                         (place-image (rectangle (/ SIZE 60) (* (/ SIZE 6) 5) "solid" "Orange") (/ SIZE 3) (/ SIZE 2) (draw-markers (board-state-los bs) 0 0)))))) 




(define blank-test-board (make-board-state blank-board 0 true))


(define test1-1 (make-spc "red" "O" true))
(define test1-2 (make-spc "white" "" false))
(define test1-3 (make-spc "white" "" false))
(define test2-1 (make-spc "blue" "X" true))
(define test2-2 (make-spc "white" "" false))
(define test2-3 (make-spc "white" "" false))
(define test3-1 (make-spc "white" "" false))
(define test3-2 (make-spc "white" "" false))
(define test3-3 (make-spc "white" "" false))

(define test-list (list (list test1-1 test1-2 test1-3)
                        (list test2-1 test2-2 test2-3)
                        (list test3-1 test3-2 test3-3)))
                              
(define test-board (make-board-state test-list 0 true))

(check-expect (handle-mouse blank-test-board 0 0 "button-up") blank-test-board)

(check-expect (handle-mouse test-board 1 1 "button-down") test-board)

(define (handle-mouse bs x y me)
  (cond [(mouse=? me "button-down")
         (if (board-state-players-turn? bs)
             (cond
               [(and (> y 0) (< y (/ SIZE 3))) (cond
                                                 [(and (and (> x 0) (< x (/ SIZE 3)))
                                                       (not (spc-taken? (list-ref (list-ref (board-state-los bs) 0) 0))))
                                                  (make-board-state (list (build-row (board-state-los bs) 0 0)
                                                                          (list-ref (board-state-los bs) 1) (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                 [(and (and (> x (/ SIZE 3)) (< x (*(/ SIZE 3) 2)))
                                                       (not (spc-taken? (list-ref (list-ref (board-state-los bs) 0) 1))))
                                                  (make-board-state (list (build-row (board-state-los bs) 0 1)
                                                                          (list-ref (board-state-los bs) 1) (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                 [(and (and (> x (*(/ SIZE 3) 2)) (< x SIZE))
                                                       (not (spc-taken? (list-ref (list-ref (board-state-los bs) 0) 2))))
                                                  (make-board-state (list (build-row (board-state-los bs) 0 2)
                                                                          (list-ref (board-state-los bs) 1) (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                 [else bs]
                                                 )]
               [(and (> y (/ SIZE 3)) (< y (*(/ SIZE 3) 2))) (cond
                                            
                                                               [(and (and (> x 0) (< x (/ SIZE 3)))
                                                                     (not (spc-taken? (list-ref (list-ref (board-state-los bs) 1) 0))))
                                                                (make-board-state (list (list-ref (board-state-los bs) 0) (build-row (board-state-los bs) 1 0)
                                                                                        (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                               [(and (and (> x (/ SIZE 3)) (< x (*(/ SIZE 3) 2)))
                                                                     (not (spc-taken? (list-ref (list-ref (board-state-los bs) 1) 1))))
                                                                (make-board-state (list (list-ref (board-state-los bs) 0) (build-row (board-state-los bs) 1 1)
                                                                                        (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                               [(and (and (> x (*(/ SIZE 3) 2)) (< x SIZE))
                                                                     (not (spc-taken? (list-ref (list-ref (board-state-los bs) 1) 2))))
                                                                (make-board-state (list (list-ref (board-state-los bs) 0) (build-row (board-state-los bs) 1 2)
                                                                                        (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) false)]
                                                               [else bs]
                                                               )]
               [(and (> y (*(/ SIZE 3) 2)) (< y SIZE)) (cond
                                                         [(and (and (> x 0) (< x (/ SIZE 3)))
                                                               (not (spc-taken? (list-ref (list-ref (board-state-los bs) 2) 0))))
                                                          (make-board-state (list (list-ref (board-state-los bs) 0)
                                                                                  (list-ref (board-state-los bs) 1) (build-row (board-state-los bs) 2 0)) (board-state-difficulty bs) false)]
                                                         [(and (and (> x (/ SIZE 3)) (< x (*(/ SIZE 3) 2)))
                                                               (not (spc-taken? (list-ref (list-ref (board-state-los bs) 2) 1))))
                                                          (make-board-state (list (list-ref (board-state-los bs) 0)
                                                                                  (list-ref (board-state-los bs) 1) (build-row (board-state-los bs) 2 1)) (board-state-difficulty bs) false)]
                                                         [(and (and (> x (*(/ SIZE 3) 2)) (< x SIZE))
                                                               (not (spc-taken? (list-ref (list-ref (board-state-los bs) 2) 2))))
                                                          (make-board-state (list (list-ref (board-state-los bs) 0)
                                                                                  (list-ref (board-state-los bs) 1) (build-row (board-state-los bs) 2 2)) (board-state-difficulty bs) false)]
                                                         [else bs]
                                                         )]
               [else bs]
               )
             bs
             )
         ]
        [else bs ]
        )
  )









(check-expect (handle-tick blank-test-board) blank-test-board)

(check-expect (handle-tick test-board) test-board)

(define (handle-tick bs)
  (if (not (board-state-players-turn? bs))
      (cond
        [(= (board-state-difficulty bs) 0) (opponents-turn bs (random 3) (random 3))]
        [(= (board-state-difficulty bs) 1)
         (if (not (empty? (determine-move bs 1)))
             (opponents-turn bs (abstract-board-x (determine-move bs 1)) (abstract-board-y (determine-move bs 1)))
             (opponents-turn bs (random 3) (random 3))
             )
         ]
        [(= (board-state-difficulty bs) 2)
         (if (not (empty? (determine-move bs 2)))
             (opponents-turn bs (abstract-board-x (first (determine-move bs 2))) (abstract-board-y (first(determine-move bs 2))))
             (opponents-turn bs (random 3) (random 3))
             )
         ]
        )
      bs
      )
  )




(define (opponents-turn bs col row)
  (if (not (spc-taken? (list-ref (list-ref (board-state-los bs) col) row)))
      (cond
        [(= col 0) (make-board-state (list (build-row-opponent (board-state-los bs) col row) (list-ref (board-state-los bs) 1)
                                           (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) true)]
        [(= col 1) (make-board-state (list (list-ref (board-state-los bs) 0) (build-row-opponent (board-state-los bs) col row)
                                           (list-ref (board-state-los bs) 2)) (board-state-difficulty bs) true)]
        [(= col 2) (make-board-state (list (list-ref (board-state-los bs) 0) (list-ref (board-state-los bs) 1)
                                           (build-row-opponent (board-state-los bs) col row)) (board-state-difficulty bs) true)]
        [else empty]
        )
      (opponents-turn bs (random 3) (random 3))
      )
  )



(define (build-row-opponent los col row)
  (cond
    [(= row 0) (list (make-spc "blue" "O" true) (list-ref (list-ref los col) 1) (list-ref (list-ref los col) 2))]
    [(= row 1) (list (list-ref (list-ref los col) 0) (make-spc "blue" "O" true) (list-ref (list-ref los col) 2))]
    [(= row 2) (list (list-ref (list-ref los col) 0) (list-ref (list-ref los col) 1) (make-spc "blue" "O" true))]
    [else empty]
    )
  )



(define (build-row los col row)
  (cond
    [(= row 0) (list (make-spc "red" "X" true) (list-ref (list-ref los col) 1) (list-ref (list-ref los col) 2))]
    [(= row 1) (list (list-ref (list-ref los col) 0) (make-spc "red" "X" true) (list-ref (list-ref los col) 2))]
    [(= row 2) (list (list-ref (list-ref los col) 0) (list-ref (list-ref los col) 1) (make-spc "red" "X" true))]
    [else empty]
    )
  )



(define (draw-markers los col row)
  (cond
    [(> row 2)
     (cond
       [(= col 2) MTS]
       [else (draw-markers los (+ col 1) 0)]
       )
     ]
    [else (place-image (text (spc-char (list-ref (list-ref los col) row))
                             (determine-text-size (/ SIZE 3)) (spc-color (list-ref (list-ref los col) row)))
                       (- (* (* (/ SIZE 24) 7) (+ 1 row)) (/ SIZE 12)) (- (* (* (/ SIZE 600) 179) (+ 1 col))
                                                                          (* (/ SIZE 100) 9)) (draw-markers los col (+ row 1)))]
    )
  )



(define (determine-text-size int)
  (if (> int 255)
      255
      (floor int)
      )
  )




(define (col-won? bs col team)
  (cond
    [(> col 2) false]
    [(and (col-filled? bs col)
          (and
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) col) 0)) team)
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) col) 1)) team)
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) col) 2)) team))) true]
    [else (col-won? bs (+ col 1) team)]))




(define (col-filled? bs num)
  (and (spc-taken? (list-ref (list-ref (board-state-los bs) num) 0))
       (spc-taken? (list-ref (list-ref (board-state-los bs) num) 1))
       (spc-taken? (list-ref (list-ref (board-state-los bs) num) 2))))





(define (rows-won? bs row team)
  (cond
    [(> row 2) false]
    [(and (row-filled? bs row)
          (and 
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) 0) row)) team)
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) 1) row)) team)
           (string=? (spc-color (list-ref (list-ref (board-state-los bs) 2) row)) team)))
     true]
       
    [else (rows-won? bs (+ row 1) team)]))





(define (row-filled? bs num)
  (and (spc-taken? (list-ref (list-ref (board-state-los bs) 0) num))
       (spc-taken? (list-ref (list-ref (board-state-los bs) 1) num))
       (spc-taken? (list-ref (list-ref (board-state-los bs) 2) num))))



(define (diag-won? bs team)
  (cond
    [(and
      (and (spc-taken? (list-ref (list-ref (board-state-los bs) 0) 0))
           (spc-taken? (list-ref (list-ref (board-state-los bs) 1) 1))
           (spc-taken? (list-ref (list-ref (board-state-los bs) 2) 2)))
      (and
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 0) 0)) team)
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 1) 1)) team)
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 2) 2)) team))) true]
    
    [(and
      (and (spc-taken? (list-ref (list-ref (board-state-los bs) 0) 2))
           (spc-taken? (list-ref (list-ref (board-state-los bs) 1) 1))
           (spc-taken? (list-ref (list-ref (board-state-los bs) 2) 0)))
      (and
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 0) 2)) team)
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 1) 1)) team)
       (string=? (spc-color (list-ref (list-ref (board-state-los bs) 2) 0)) team))) true]
          
    [else false]))






(define (los-full? bs)
  (cond [(and (row-filled? bs 0)
              (row-filled? bs 1)
              (row-filled? bs 2)) true]
        [else false]))



(define (end-state bs)

  (cond
    [(or (rows-won? bs 0 "red") (col-won? bs 0 "red") (diag-won? bs "red"))
     (place-image (text "X WINS" (/ SIZE 4) "green") (/ SIZE 2) (/ SIZE 2)
                  (draw-board bs))]
    [(or (rows-won? bs 0 "blue") (col-won? bs 0 "blue") (diag-won? bs "blue"))
     (place-image (text "O WINS" (/ SIZE 4) "green") (/ SIZE 2) (/ SIZE 2)
                  (draw-board bs))]
    [(los-full? bs)
     (place-image (text "DRAW" (/ SIZE 4) "green") (/ SIZE 2) (/ SIZE 2)
                  (draw-board bs))]))


(define s1-1 (make-spc "white" "" true))
(define s1-2 (make-spc "white" "" true))
(define s1-3 (make-spc "white" "" true))
(define s2-1 (make-spc "white" "" true))
(define s2-2 (make-spc "white" "" true))
(define s2-3 (make-spc "white" "" true))
(define s3-1 (make-spc "white" "" true))
(define s3-2 (make-spc "white" "" true))
(define s3-3 (make-spc "white" "" true))

(define full-list (list (list s1-1 s1-2 s1-3) (list s2-1 s2-2 s2-3) (list s3-1 s3-2 s3-3)))
(define full-board (make-board-state full-list 0 true))




(check-expect (done? blank-test-board) false)
(check-expect (done? test-board) false)
(check-expect (done? full-board) true)

(define (done? bs)
  (cond [(or (rows-won? bs 0 "red") (col-won? bs 0 "red") (diag-won? bs "red")) true]
        [(or (rows-won? bs 0 "blue") (col-won? bs 0 "blue") (diag-won? bs "blue")) true]
        [(los-full? bs) true] 
        [else false]))




(define (handle-key bs ke)
  (cond [(key=? ke "0") (make-board-state (board-state-los bs) 0 (board-state-players-turn? bs))]
        [(key=? ke "1") (make-board-state (board-state-los bs) 1 (board-state-players-turn? bs))]
        [(key=? ke "2") (make-board-state (board-state-los bs) 2 (board-state-players-turn? bs))]
        [else bs]))

(define (main ws)
  (big-bang ws                   
    (to-draw draw-board)  
    (stop-when done? end-state)    
    (on-mouse handle-mouse)       
    (on-key handle-key)
    (on-tick handle-tick)
    )
  )

(main (make-board-state blank-board 0 true))






