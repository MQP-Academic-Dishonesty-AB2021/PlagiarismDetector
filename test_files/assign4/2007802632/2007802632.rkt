

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment4_BlakeMcLeod_MicahVargas) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 






(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))

(define TEXT-SIZE (* .2 SIZE))
(define X (text "X" TEXT-SIZE "red"))
(define O  (text "O" TEXT-SIZE "blue"))
(define Ls (/ SIZE 30))
(define ls2 (/ Ls 2))
(define ticline (make-pen "orange" Ls "solid" "round" "round"))
(define v1 (* .4 SIZE))
(define v2 (* .6 SIZE))
(define h1 (* .4 SIZE))
(define h2 (* .6 SIZE))
(define sp (* SIZE .2))
(define ep (* SIZE .8))
(define px1 (* SIZE .3))
(define py1 (+ (* SIZE .3) ls2))
(define px2 (* SIZE .5))
(define py2 (+ (* SIZE .3) ls2))
(define px3 (* SIZE .7))
(define py3 (+ (* SIZE .3) ls2))
(define px4 (* SIZE .3))
(define py4 (+ (* SIZE .5) ls2))
(define px5 (* SIZE .5))
(define py5 (+ (* SIZE .5) ls2))
(define px6 (* SIZE .7))
(define py6 (+ (* SIZE .5) ls2))
(define px7 (* SIZE .3))
(define py7 (+ (* SIZE .7) ls2))
(define px8 (* SIZE .5))
(define py8 (+ (* SIZE .7) ls2))
(define px9 (* SIZE .7))
(define py9 (+ (* SIZE .7) ls2))









(define BW0 (list "" "" "" 
                  "" "" ""
                  "" "" ""))
(define BW1 (list "X" "X" "X" 
                  "" "" "" 
                  "" "" "")) 
(define BW2 (list "O" "O" "O" 
                  "" "" "" 
                  "" "" "")) 
                 
(define BW3 (list "X" "" ""
                  "X" "" ""
                  "X" "" "")) 
(define BW4 (list "O" "" ""
                  "O" "" ""
                  "O" "" "")) 

(define BW5 (list "X" "" ""
                  "" "X" ""
                  "" "" "X")) 
(define BW6 (list "O" "" ""
                  "" "O" ""
                  "" "" "O")) 
                 
(define BW7 (list "" "" "X"
                  "" "X" ""
                  "X" "" "")) 
(define BW8 (list "" "" "O"
                  "" "O" ""
                  "O" "" "")) 
                 

(define BT1 (list "X" "O" "X"
                  "X" "X" "O"
                  "O" "X" "O"))

(define T1 (list "X" "" "X"
                 "X" "" ""
                 "O" "" "O"))
(define T2 (list "O" "X" ""
                 "" "O" "X"
                 "X" "" ""))



(define-struct ttc (board turn level))



(define START (make-ttc BW0 "X" 0))
(define t1 (make-ttc T1 "O" 1))
(define t2 (make-ttc T1 "O" 2))
(define t3 (make-ttc T2 "O" 2))
(define t4 (make-ttc BW5 "O" 0))
(define r1 (make-ttc (list "X" "X" "X" "X" "X" "X" "X" "X" "X") "X" 0))
(define r2 (make-ttc (list "O" "O" "O" "O" "O" "O" "O" "O" "O") "O" 0))
(define r3 (make-ttc (list "X" "X" "X" "X" "X" "X" "X" "X" "X") "Xend" 0))
(define r4 (make-ttc (list "O" "O" "O" "O" "O" "O" "O" "O" "O") "Oend" 0))
(define r5 (make-ttc (list "O" "O" "O" "O" "O" "O" "O" "O" "O") "TIEend" 0))







(define (main ttc)
  (big-bang ttc
    (on-tick Oturn)     
    (on-mouse move)     
    (to-draw render)    
    (on-key level)))    



(check-expect (Oturn t3)
              (make-ttc (list "O" "X" "" "" "O" "X" "X" "" "O") "X" 1))
(check-expect (Oturn t4)
              (make-ttc (list "X" "" "" "" "X" "" "" "" "X") "Xend" 0))
(check-expect (Oturn
               (make-ttc (list "O" "" "" "" "O" "" "" "" "O") "Oend" 0))
              (make-ttc (list "O" "" "" "" "O" "" "" "" "O") "Oend" 0))
(check-expect (Oturn (make-ttc BT1 "TIEend" 0))
              (make-ttc BT1 "TIEend" 0))
(define (Oturn ttc)
  (cond [(string-contains? "end" (ttc-turn ttc)) ttc]
        [(win? ttc) (make-ttc
                     (ttc-board ttc)
                     (string-append (cond [(equal? (ttc-turn ttc) "X")"O"]
                                          [(equal? (ttc-turn ttc) "O")"X"])
                                    "end")
                     (ttc-level ttc))]
        [(full-board? (ttc-board ttc) 0) (make-ttc (ttc-board ttc) "TIEend" (ttc-level ttc))]
        [(equal? 1 (ttc-level ttc)) (nmove ttc 1)]
        [(equal? 2 (ttc-level ttc)) (nmove ttc 2)]
        [else (Omove ttc)]))



(check-expect (full-board?
               (list "" "" "" "" "" "" "" "" "") 0) false) 
(check-expect (full-board?
               (list "X" "X" "X" "X" "X" "X" "X" "X" "X") 0) true) 
(check-expect (full-board? BT1 0) true)


(define (full-board? board acc)
  (cond
    [(empty? board) false]
    [(= acc 8) true]
    [else
     (if (not (string=? (first board) ""))
         (full-board? (rest board) (+ acc 1))
         false)]))




(define (Omove ttc)
  (cond [(equal? "O" (ttc-turn ttc))
         (local [(define val (random 8))]
           (cond [(equal? (list-ref (ttc-board ttc) val) "")
                  (make-ttc (list-set (ttc-board ttc) val "O") "X"(ttc-level ttc))]
                 [else
                  (Oturn ttc)]))]
        [else ttc]))




(check-expect
 (nmove t3 2)
 (make-ttc (list "O" "X" "" "" "O" "X" "X" "" "O") "X" 1))
(check-expect
 (nmove t1 1)
 (make-ttc
  (list "X" "O" "X" "X" "" "" "O" "" "O") "X" 1))
(check-expect
 (nmove START 1)
 (make-ttc (list "" "" "" "" "" "" "" "" "") "X" 0))
(define (nmove ttc l )
  (cond [(equal? "O" (ttc-turn ttc))
         (cond [(equal? l 1)
                (local [(define lttc (genlist ttc "X" 0 empty))]
                  (cond [(empty? (rec lttc)) (Omove ttc)]
                        [else
                         (rec lttc)]))]
               [(equal? l 2)
                (local [(define rec1 (rec (genlist ttc "X" 0 empty)))
                        (define rec2 (rec (genlist ttc "O" 0 empty)))]
                  (cond [(empty? rec1)
                         (cond [(empty? rec2) (Omove ttc)]
                               [else
                                rec2])]
                        [else
                         rec1]))])]
        [else
         ttc]))
  



(define (genlist ttc move n lob)
  (cond [(equal? n 9) lob]
        [else
         (genlist ttc move (+ n 1) (cons (pmove ttc move n) lob))]))





(define (pmove ttc move n)
  (cond [(string=? (list-ref (ttc-board ttc) n) "")
         (make-ttc (list-set (ttc-board ttc) n move) "O" 1)]
        [else
         ttc]))






(define (rec lottc)
  (local [ (define (rec2  lottc place)
             (cond
               [(empty? lottc) empty]
               [else
                (cond[(win? (first lottc))
                      (make-ttc
                       (list-set (ttc-board (first lottc)) place "O")
                       "X" (ttc-level (first lottc)))]
                     [else
                      (rec2 (rest lottc) (- place 1))])]))]
    (rec2 lottc 8)))





(check-expect
 (move
  (make-ttc (list "" "" "" "X" "" "" "X" "" "") "X" 0) 0 0 "button-down")
              (make-ttc (list "X" "" "" "X" "" "" "X" "" "") "O" 0))
(check-expect
 (move
  (make-ttc (list "X" "" "" "X" "" "" "X" "O" "") "O" 0) 0 0 "button-down")
              (make-ttc (list "X" "" "" "X" "" "" "X" "O" "") "Xend" 0))
(check-expect
 (move
  (make-ttc (list "X" "" "" "X" "" "" "X" "O" "") "O" 0) 0 0 "button-up")
              (make-ttc (list "X" "" "" "X" "" "" "X" "O" "") "O" 0))
(check-expect
 (move
  (make-ttc (list "O" "" "" "O" "" "" "O" "X" "") "X" 0) 0 0 "button-down")
              (make-ttc (list "O" "" "" "O" "" "" "O" "X" "") "Oend" 0))
(check-expect
 (move
  (make-ttc (list "O" "" "" "O" "" "" "O" "X" "") "Oend" 0) 0 0 "button-down")
              (make-ttc (list "O" "" "" "O" "" "" "O" "X" "") "Oend" 0))
(check-expect
 (move
  (make-ttc (list "" "" "" "X" "" "" "X" "O" "") "O" 0) 0 0 "button-down")
              (make-ttc (list "" "" "" "X" "" "" "X" "O" "") "O" 0))
(check-expect
 (move
  (make-ttc (list "" "" "" "" "" "" "" "" "") "Oend" 0) 0 0 "button-down")
              (make-ttc (list "" "" "" "" "" "" "" "" "") "Oend" 0))

(define (move ttc x y mouse-click)
  (cond [(string-contains?  (ttc-turn ttc) "end") ttc]
        [(equal? mouse-click "button-down")
         (cond [(string-contains? "end" (ttc-turn ttc))ttc]
               [(win? ttc) (make-ttc
                            (ttc-board ttc)
                            (string-append
                             (cond [(equal? (ttc-turn ttc) "X")"O"]
                                   [(equal? (ttc-turn ttc) "O")"X"])
                             "end")
                            (ttc-level ttc))]
               [else
                (cond [(equal? (ttc-turn ttc) "X")
                       (square-select ttc x y)]
                      [else
                       ttc])])]
        [else ttc]))





(check-expect
 (square-select START px1 py1)
 (cond [(equal? (list-ref (ttc-board START) 0) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 0 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px2 py2)
 (cond [(equal? (list-ref (ttc-board START) 1) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 1 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px3 py3)
 (cond [(equal? (list-ref (ttc-board START) 2) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 2 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px4 py4)
 (cond [(equal? (list-ref (ttc-board START) 3) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 3 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px5 py5)
 (cond [(equal? (list-ref (ttc-board START) 4) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 4 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px6 py6)
 (cond [(equal? (list-ref (ttc-board START) 5) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 5 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px7 py7)
 (cond [(equal? (list-ref (ttc-board START) 6) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 6 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px8 py8)
 (cond [(equal? (list-ref (ttc-board START) 7) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 7 "X")
                                                    "O"(ttc-level START))]))
(check-expect
 (square-select START px9 py9)
 (cond [(equal? (list-ref (ttc-board START) 8) "") (make-ttc
                                                    (list-set
                                                     (ttc-board START) 8 "X")
                                                    "O"(ttc-level START))]))
                                                             

(define (square-select ttc x y)
  (local [(define (create ttc n)
            (cond [(equal? (list-ref (ttc-board ttc) n) "")
                   (make-ttc (list-set (ttc-board ttc) n "X")"O"
                                                             (ttc-level ttc))]
                  [else ttc]))]
    (cond [(and (< x h1) (< y v1))
           (create ttc 0)]
          [(and (and (> x h1) (< x h2)) (< y v1))
           (create ttc 1)]
          [(and (> x h2) (< y v1))
           (create ttc 2)]
          [(and (< x h1) (and (> y v1) (< y v2)))
           (create ttc 3)]
          [(and (and (> x h1) (< x h2)) (and (> y v1) (< y v2)))
           (create ttc 4)]
          [(and (> x h2) (and (> y v1) (< y v2)))
           (create ttc 5)]
          [(and (< x h1) (> y v1))
           (create ttc 6)]
          [(and (and (> x h1) (< x h2)) (> y v1))
           (create ttc 7)]
          [else
           (create ttc 8)])))



(check-expect(render r2) (overlay render-board-img
                                  (place-image (place-image
                                   O px1 py1 
                                   (place-image
                                    O px2 py2 
                                    (place-image
                                     O px3 py3
                                     (place-image
                                      O px4 py4 
                                      (place-image
                                       O px5 py5 
                                       (place-image
                                        O px6 py6 
                                        (place-image
                                         O px7 py7 
                                         (place-image
                                          O px8 py8 
                                          (place-image
                                           O px9 py9 (square SIZE "solid" "white"))
                                          ))))))))
                                               (/ SIZE 2) (/ SIZE 2)
                                               (square SIZE "solid" "white"))))
(check-expect(render r3) (overlay (text " X WINS" TEXT-SIZE "green") (overlay render-board-img
                                  (place-image (place-image
                                   X px1 py1 
                                   (place-image
                                    X px2 py2 
                                    (place-image
                                     X px3 py3
                                     (place-image
                                      X px4 py4 
                                      (place-image
                                       X px5 py5 
                                       (place-image
                                        X px6 py6 
                                        (place-image
                                         X px7 py7 
                                         (place-image
                                          X px8 py8 
                                          (place-image
                                           X px9 py9 (square SIZE "solid" "white"))
                                          ))))))))
                                               (/ SIZE 2) (/ SIZE 2)
                                               (square SIZE "solid" "white")))))
(check-expect(render r4) (overlay (text " O WINS" TEXT-SIZE "green") (overlay render-board-img
                                  (place-image (place-image
                                   O px1 py1 
                                   (place-image
                                    O px2 py2 
                                    (place-image
                                     O px3 py3
                                     (place-image
                                      O px4 py4 
                                      (place-image
                                       O px5 py5 
                                       (place-image
                                        O px6 py6 
                                        (place-image
                                         O px7 py7 
                                         (place-image
                                          O px8 py8 
                                          (place-image
                                           O px9 py9 (square SIZE "solid" "white"))
                                          ))))))))
                                               (/ SIZE 2) (/ SIZE 2)
                                                   (square SIZE "solid" "white")))))
(check-expect(render r5) (overlay (text " TIE" TEXT-SIZE "green") (overlay render-board-img
                                  (place-image (place-image
                                   O px1 py1 
                                   (place-image
                                    O px2 py2 
                                    (place-image
                                     O px3 py3
                                     (place-image
                                      O px4 py4 
                                      (place-image
                                       O px5 py5 
                                       (place-image
                                        O px6 py6 
                                        (place-image
                                         O px7 py7 
                                         (place-image
                                          O px8 py8 
                                          (place-image
                                           O px9 py9 (square SIZE "solid" "white"))
                                          ))))))))
                                               (/ SIZE 2) (/ SIZE 2)
                                               (square SIZE "solid" "white")))))
(define (render ttc)
  (overlay
   (cond [(equal? (ttc-turn ttc) "TIEend") (text " TIE" TEXT-SIZE "green")]
         [(equal? (ttc-turn ttc) "Xend") (text " X WINS" TEXT-SIZE "green")]
         [(equal? (ttc-turn ttc) "Oend") (text " O WINS" TEXT-SIZE "green")]
         [else (square 0 "outline" "white")])
   (overlay render-board-img 
            (render-move ttc))))
 


(define (render-move ttc)
  (local [(define (render-move1 board recurs totalimg)
            (cond [(= recurs 9)
                   (place-image totalimg
                                (/ SIZE 2)
                                (/ SIZE 2)
                                (square SIZE "solid" "white"))]
                  [else
                   (render-move1 (rest board)
                                 (+ recurs 1)
                                 (place-move (first board) recurs totalimg))]))]
    (render-move1 (ttc-board ttc) 0 (square SIZE "solid" "white"))))
  


(check-expect (place-move "g" 0 (square SIZE "solid" "white"))(square SIZE "solid" "white"))
(define (place-move place spot totalimg)
  (cond [(string=? "X" place) (cond [(= 0 spot) (place-image X px1 py1 totalimg)]
                                    [(= 1 spot) (place-image X px2 py2 totalimg)]
                                    [(= 2 spot) (place-image X px3 py3 totalimg)]
                                    [(= 3 spot) (place-image X px4 py4 totalimg)]
                                    [(= 4 spot) (place-image X px5 py5 totalimg)]
                                    [(= 5 spot) (place-image X px6 py6 totalimg)]
                                    [(= 6 spot) (place-image X px7 py7 totalimg)]
                                    [(= 7 spot) (place-image X px8 py8 totalimg)]
                                    [(= 8 spot) (place-image X px9 py9 totalimg)])]
        
        [(string=? "O" place) (cond [(= 0 spot) (place-image O px1 py1 totalimg)]
                                    [(= 1 spot) (place-image O px2 py2 totalimg)] 
                                    [(= 2 spot) (place-image O px3 py3 totalimg)]
                                    [(= 3 spot) (place-image O px4 py4 totalimg)]
                                    [(= 4 spot) (place-image O px5 py5 totalimg)]
                                    [(= 5 spot) (place-image O px6 py6 totalimg)]
                                    [(= 6 spot) (place-image O px7 py7 totalimg)]
                                    [(= 7 spot) (place-image O px8 py8 totalimg)]
                                    [(= 8 spot) (place-image O px9 py9 totalimg)])]
        [else totalimg]))
  

    


(define (writel img x1 y1 x2 y2 )
  (add-line img x1 y1  x2 y2 ticline))
(define render-board-img 
  (writel
   (writel
    (writel
     (writel
      (square SIZE "outline" "white")
      h2 sp h2 ep)
     h1 sp h1 ep)
    sp h2 ep h2)
   sp h1 ep h1))
    




(check-expect (win? (make-ttc BW0 "X" 0)) false)
(check-expect (win? (make-ttc BW1 "X" 0)) true)
(check-expect (win? (make-ttc BW2 "X" 0)) true)
(check-expect (win? (make-ttc BW3 "X" 0)) true)
(check-expect (win? (make-ttc BW4 "X" 0)) true)
(check-expect (win? (make-ttc BW5 "X" 0)) true)
(check-expect (win? (make-ttc BW6 "X" 0)) true)
(check-expect (win? (make-ttc BW7 "X" 0)) true)
(check-expect (win? (make-ttc (list "" "" "" "X" "X" "X" "" "" "") "X" 0)) true)
(check-expect (win? (make-ttc (list "" "" "" "" "" "" "X" "X" "X") "X" 0)) true)
(check-expect (win? (make-ttc (list "" "X" "" "" "X" "" "" "X" "") "X" 0)) true)
(check-expect (win? (make-ttc (list "" "" "X" "" "" "X" "" "" "X") "X" 0)) true)

       
(define (win? ttc)
  (local [(define (wins? p1 p2 p3)
            (and (string=? (list-ref (ttc-board ttc) p1)
                           (list-ref (ttc-board ttc) p2)
                           (list-ref (ttc-board ttc) p3))
                 (not (equal? (list-ref (ttc-board ttc) p1) ""))))]   
    (cond [(wins? 0 1 2) true]
          [(wins? 3 4 5) true]
          [(wins? 6 7 8) true]
          [(wins? 0 3 6) true]
          [(wins? 1 4 7) true]
          [(wins? 2 5 8) true]
          [(wins? 0 4 8) true]
          [(wins? 2 4 6) true]
          [else false])))







(check-expect (level START "r") START)
(check-expect (level START "1") (make-ttc (ttc-board START) (ttc-turn START) 1))
(check-expect (level START "2") (make-ttc (ttc-board START) (ttc-turn START) 2))
(check-expect (level START "j") START)

(define (level ttc key)
  (cond [(equal? "r" key) START]
        [(equal? "1" key) (make-ttc (ttc-board ttc) (ttc-turn ttc) 1)]
        [(equal? "2" key) (make-ttc (ttc-board ttc) (ttc-turn ttc) 2)]
        [else ttc]))