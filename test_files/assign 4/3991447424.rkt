

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 600) 
(define MTS (empty-scene SIZE SIZE))


(define VLINE (rectangle (/ SIZE 30) (* SIZE .9) "solid" "orange"))
(define HLINE (rotate 90 VLINE))

(define BOARD (place-image HLINE (/ SIZE 2) (* SIZE 2/3)
              (place-image HLINE (/ SIZE 2) (/ SIZE 3)
              (place-image VLINE (* SIZE 2/3) (/ SIZE 2)
              (place-image VLINE (/ SIZE 3) (/ SIZE 2) MTS)))))

(define TEXT-SIZE (/ SIZE 4))
(define X-COLOR "red")
(define O-COLOR "blue")
(define WIN-COLOR "green")
(define POS1 (/ SIZE 6))
(define POS2 (* 3 POS1))
(define POS3 (* 5 POS1))

(define X-TEXT (text "X" TEXT-SIZE X-COLOR))
(define O-TEXT (text "O" TEXT-SIZE O-COLOR))









(define B0 (list "" "" ""
                 "" "" ""
                 "" "" ""))

(define B1 (list ""  ""  ""
                 ""  "O" ""
                 "X" ""  ""))
(define B1-1 (list ""  ""  ""
                   ""  "O" ""
                   "X" "O" "X"))
(define B1-2 (list "X" "O" ""
                   ""  "O" ""
                   "X" "O" "X"))

(define B2 (list "O" "" "O"
                 "X" "" "X"
                 ""  "" ""))

(define B3 (list "O" "" "X"
                 "X" "" "X"
                 ""  "" "O"))

(define B4 (list "X" "O" "X"
                 ""  "O" ""
                 "O" "X" "X"))

(define TIE (list "X" "O" "X"
                  "X" "O" "O"
                  "O" "X" "X"))

(define OWIN (list "O" "X" "X"
                   "O" "O" "X"
                   "X" ""  "O"))

(define XWIN (list "X" "O" "X"
                   ""  "O" "X"
                   "O" ""  "X"))



(define ROW1 (list 0 1 2))
(define ROW2 (list 3 4 5))
(define ROW3 (list 6 7 8))
(define COL1 (list 0 3 6))
(define COL2 (list 1 4 7))
(define COL3 (list 2 5 8))
(define DG1 (list 0 4 8))
(define DG2 (list 2 4 6))

(define units (list ROW1 ROW2 ROW3 COL1 COL2 COL3 DG1 DG2))










(define-struct world-state (cur-board difficulty winner))

 

(define START (make-world-state B0 0 ""))
(define TIE-GAME (make-world-state TIE 0 "T"))





(define (main ws)
  (big-bang ws
    (on-tick update)     
    (to-draw render)     
    (on-key change-dif)  
    (on-mouse make-move))) 
    






(define (update ws)
  (local ((define (unit-full los)
            (local ((define (same los str)
                      (and (string=? str (first los))
                           (string=? str (list-ref los 1))
                           (string=? str (list-ref los 2)))))
              (cond [(same los "X") "X"]
                    [(same los "O") "O"]
                    [else ""])))

          (define (make-unit aboard aunit)
            (list (list-ref aboard (first aunit))
                  (list-ref aboard (list-ref aunit 1))
                  (list-ref aboard (list-ref aunit 2))))

          (define (get-winner aboard lou)
            (cond [(empty? lou) ""]
                  [(not (string=? (unit-full (make-unit aboard (first lou))) ""))
                   (unit-full (make-unit aboard (first lou)))]
                  [else (get-winner aboard (rest lou))]))

          (define (board-full? aboard)
            (not (member? "" aboard)))

          (define WIN (get-winner (world-state-cur-board ws) units)))         
    (cond [(not (string=? WIN ""))
           (make-world-state (world-state-cur-board ws)
                             (world-state-difficulty ws) WIN)]
          [(board-full? (world-state-cur-board ws))
           (make-world-state (world-state-cur-board ws)
                             (world-state-difficulty ws) "T")]
          [else ws])))


(check-expect (update START) START)                 
(check-expect (update (make-world-state TIE 0 ""))
              (make-world-state TIE 0 "T"))         
(check-expect (update (make-world-state OWIN 1 ""))
              (make-world-state OWIN 1 "O"))        
(check-expect (update (make-world-state XWIN 2 ""))
              (make-world-state XWIN 2 "X"))        







(define (render ws)
  (local ((define (winning-text winner)
            (local ((define (make-text str) (text str TEXT-SIZE WIN-COLOR)))
              (cond [(string=? winner "T") (make-text "TIE")]
                    [(string=? winner "X") (make-text "X WINS")]
                    [(string=? winner "O") (make-text "O WINS")]
                    [else (square 0 "solid" "white")])))

          (define (render-board aboard)
            (local
              ((define (render-board-in aboard position)
                 (cond
                   [(empty? aboard) BOARD]
                   [else
                    (local
                      ((define (X-POS index)
                         (cond [(= 0 (modulo index 3)) POS1]
                               [(= 1 (modulo index 3)) POS2]
                               [(= 2 (modulo index 3)) POS3]))

                       (define (Y-POS index)
                         (cond [(= 0 (floor (/ index 3))) POS1]
                               [(= 1 (floor (/ index 3))) POS2]
                               [(= 2 (floor (/ index 3))) POS3]))

                       (define (make-text letter)
                         (cond [(string=? "X" letter) X-TEXT]
                               [(string=? "O" letter) O-TEXT]
                               [else (square 0 "solid" "white")])))
                      (place-image (make-text (first aboard))
                                   (X-POS position) (Y-POS position)
                                   (render-board-in (rest aboard) (add1 position))))])))                                   
              (render-board-in aboard 0))))
    (place-image (winning-text (world-state-winner ws)) (/ SIZE 2) (/ SIZE 2)
                 (render-board (world-state-cur-board ws)))))

(check-expect (render START) BOARD) 
(check-expect (render (make-world-state B1 0 "")) 
              (place-image O-TEXT POS2 POS2
                           (place-image X-TEXT POS1 POS3 BOARD)))
(check-expect (render (make-world-state B0 0 "X")) 
              (place-image (text "X WINS" TEXT-SIZE WIN-COLOR)
                           (/ SIZE 2) (/ SIZE 2) BOARD))
(check-expect (render (make-world-state B0 0 "T")) 
              (place-image (text "TIE" TEXT-SIZE WIN-COLOR)
                           (/ SIZE 2) (/ SIZE 2) BOARD))











(define (make-move ws x-cor y-cor me)
  (local((define (get-pos mouse-x mouse-y)
           (local ((define (mouse->num mouse-pos)
                     (cond [(< mouse-pos (/ SIZE 3)) 0]
                           [(> mouse-pos (* SIZE 2/3)) 2]
                           [else 1])))
             (+ (* 3 (mouse->num mouse-y)) (mouse->num mouse-x))))

         (define (AI-move aboard diff)
           (if (string=? "" (world-state-winner
                             (update (make-world-state aboard 0 ""))))
               
               (local ((define (random-move aboard)
                         (local ((define index (random 9)))
                           (if (string=? "" (list-ref aboard index))
                               (insert-into-list "O" index aboard)
                               (random-move aboard))))

                       (define (winning-board lob letter)
                         (local((define (board-won? aboard)
                                  (string=?
                                   letter
                                   (world-state-winner
                                    (update (make-world-state aboard 0 "")))))
                                (define winning-board-list
                                  (filter board-won? lob)))
                           (if (empty? winning-board-list)
                               false
                               (first winning-board-list))))
              
                       (define (make-boards aboard letter)
                         (local
                           ((define (make-boards-in lobs context)
                              (cond [(empty? lobs) empty]
                                    [(string=? "" (first lobs))
                                     (cons
                                      (insert-into-list letter context aboard)
                                      (make-boards-in (rest lobs)
                                                      (add1 context)))]
                                    [else (make-boards-in (rest lobs)
                                                          (add1 context))])))
                           (make-boards-in aboard 0)))

                       (define (winning-move aboard letter)
                         (winning-board (make-boards aboard letter) letter))

                       (define (replace-x aboard x-board)
                         (local
                           ((define (replace-x-in aboard x-board context)
                              (cond
                                [(and (string=? "" (list-ref aboard context))
                                      (string=? "X" (list-ref x-board context)))
                                 (insert-into-list "O" context aboard)]
                                [else (replace-x-in aboard x-board
                                                    (add1 context))])))
                           (replace-x-in aboard x-board 0)))

                       (define (diff1 aboard)
                         (if (false? (winning-move aboard "O"))
                             (random-move aboard)
                             (winning-move aboard "O")))

                       (define (diff2 aboard)
                         (if (false? (winning-move aboard "O"))
                             (if (false? (winning-move aboard "X"))
                                 (random-move aboard)
                                 (replace-x aboard (winning-move aboard "X")))
                             (winning-move aboard "O")))
                       )  
                 (cond [(= 0 diff) (random-move aboard)]
                       [(= 1 diff) (diff1 aboard)]
                       [(= 2 diff) (diff2 aboard)]
                       ))
               aboard)))
    (cond [(and (mouse=? me "button-down")
                (string=? "" (world-state-winner ws))
                (string=? "" (list-ref (world-state-cur-board ws)
                                       (get-pos x-cor y-cor))))
           (make-world-state (AI-move
                              (insert-into-list "X" (get-pos x-cor y-cor)
                                                (world-state-cur-board ws))
                              (world-state-difficulty ws))
                             (world-state-difficulty ws)
                             (world-state-winner ws))]
          [else ws])))

(check-expect (make-move TIE-GAME SIZE SIZE "button-down") TIE-GAME)
(check-expect (make-move (make-world-state B1 2 "") SIZE SIZE "button-down")
              (make-world-state B1-1 2 ""))
(check-expect (make-move (make-world-state B1-1 1 "") 0 0 "button-down")
              (make-world-state B1-2 1 ""))








(define (insert-into-list item index lox)
  (local((define (beginning lox index)
           (local((define (beginning-in lox context)
                    (cond [(>= context index) empty]
                          [else (cons (first lox)
                                      (beginning-in (rest lox)
                                                    (add1 context)))])))
             (beginning-in lox 0)))
         
         (define (end lox index)
           (local((define (end-in lox context)
                    (cond [(> context index) lox]
                          [else (end-in (rest lox) (add1 context))])))
             (end-in lox 0))))
    (append (beginning lox index) (list item) (end lox index))))


(check-expect (insert-into-list "X" 2 B0) (list "" "" "X"
                                                "" "" ""
                                                "" "" ""))
(check-expect (insert-into-list "I" 7 TIE) (list "X" "O" "X"
                                                 "X" "O" "O"
                                                 "O" "I" "X"))
(check-expect (insert-into-list 2 0 (list 1)) (list 2))







(define (change-dif ws ke)
  (local((define (replace-dif dif)
           (make-world-state (world-state-cur-board ws)
                             dif (world-state-winner ws))))
    (cond [(key=? ke "0") (replace-dif 0)]
          [(key=? ke "1") (replace-dif 1)]
          [(key=? ke "2") (replace-dif 2)]
          [else ws])))

(check-expect (change-dif START "0") START)
(check-expect (change-dif START "1") (make-world-state B0 1 ""))
(check-expect (change-dif START "2") (make-world-state B0 2 ""))
(check-expect (change-dif START "6") START)














 









 



 

















 
                 













 











 




 
 
  
















 
  












 










 









 






