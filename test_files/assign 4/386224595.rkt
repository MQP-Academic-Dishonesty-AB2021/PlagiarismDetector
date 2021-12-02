

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4 Pt 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 







(define SIZE 700) 
(define MTS (empty-scene SIZE SIZE))





(define CUSHION (/ SIZE 8))
(define UNIT-SIZE (/ SIZE 4))
(define FONT-SIZE (round (/ SIZE 8)))
(define BOARD-IMG
  (local [(define HORIZONTAL (line (* 3 UNIT-SIZE) 0 "black"))
          (define VERTICAL (line 0 (* 3 UNIT-SIZE) "black"))]
    (place-image VERTICAL
                 (+ CUSHION UNIT-SIZE UNIT-SIZE) (/ SIZE 2)
                 (place-image VERTICAL
                              (+ CUSHION UNIT-SIZE) (/ SIZE 2) 
                              (place-image HORIZONTAL
                                           (/ SIZE 2) (+ CUSHION UNIT-SIZE UNIT-SIZE) 
                                           (place-image HORIZONTAL
                                                        (/ SIZE 2) (+ CUSHION UNIT-SIZE) 
                                                        MTS))))))



(define-struct unit (x-pos y-pos space))





 

(define EMPTY-UNIT (make-unit 0 0 ""))
(define X-UNIT (make-unit 30 50 "X"))
(define O-UNIT (make-unit 20 60 "O"))

(define-struct turn (player board))




 
 


(define U11 (make-unit CUSHION 
                       CUSHION ""))
(define U12 (make-unit (+ CUSHION UNIT-SIZE) 
                       CUSHION ""))
(define U13 (make-unit (+ CUSHION UNIT-SIZE UNIT-SIZE) 
                       CUSHION ""))
(define U21 (make-unit CUSHION 
                       (+ CUSHION UNIT-SIZE) ""))
(define U22 (make-unit (+ CUSHION UNIT-SIZE) 
                       (+ CUSHION UNIT-SIZE) ""))
(define U23 (make-unit (+ CUSHION UNIT-SIZE UNIT-SIZE) 
                       (+ CUSHION UNIT-SIZE) ""))
(define U31 (make-unit CUSHION 
                       (+ CUSHION UNIT-SIZE UNIT-SIZE) ""))
(define U32 (make-unit (+ CUSHION UNIT-SIZE) 
                       (+ CUSHION UNIT-SIZE UNIT-SIZE) ""))
(define U33 (make-unit (+ CUSHION UNIT-SIZE UNIT-SIZE) 
                       (+ CUSHION UNIT-SIZE UNIT-SIZE) ""))

(define start-board (list U11 U12 U13
                          U21 U22 U23
                          U31 U32 U33))

(define START (make-turn "X" start-board))





(define (main turn)
  (big-bang turn
    (on-tick o-turn)
    (on-mouse click)
    (to-draw render)))




(check-expect (o-turn START) START)
(check-satisfied (o-turn (make-turn "O" start-board)) (λ(o) (not (equal? START o))))

(define (o-turn turn)
  (if (string=? (turn-player turn) "O")
      (update-turn turn
                   (add1 (random 3))
                   (add1 (random 3)))
      turn))




(check-expect (click START 0 0 "move") START)
(check-expect (click START (/ SIZE 2) (/ SIZE 2) "button-down")
              (check-cords START (/ SIZE 2) (/ SIZE 2)))

(define (click turn x y mouse)
  (cond [(mouse=? mouse "button-down")
         (check-cords turn x y)]
        [else turn]))




(check-expect (check-cords START 0 0) START)
(check-expect (check-cords START 0 SIZE) START)
(check-expect (check-cords START (/ SIZE 2) (/ SIZE 2)) (update-turn START 2 2))
(check-expect (check-cords START (/ SIZE 3) (/ SIZE 3)) (update-turn START 1 1))

(define (check-cords turn x y)
  (local [(define x-col (cond [(< x (unit-x-pos (list-ref (turn-board turn) 1))) 1]
                              [(< x (unit-x-pos (list-ref (turn-board turn) 2))) 2]
                              [else 3]))
          (define y-row (cond [(< y (unit-y-pos (list-ref (turn-board turn) 3))) 1]
                              [(< y (unit-y-pos (list-ref (turn-board turn) 6))) 2]
                              [else 3]))]
    (cond [(or (< x CUSHION) (> x (- SIZE CUSHION))) turn]
          [(or (< y CUSHION) (> y (- SIZE CUSHION))) turn]
          [else (update-turn turn x-col y-row)])))




(check-expect (update-turn START 1 2) (make-turn "O" (update-board START 3)))
(check-expect (update-turn (make-turn "O" (list U11 U12 U13 U21
                                                (make-unit (unit-x-pos U22)
                                                           (unit-y-pos U22) "X")
                                                U23 U31 U32 U33)) 2 2)
              (make-turn "O" (list U11 U12 U13 U21
                                   (make-unit (unit-x-pos U22)
                                              (unit-y-pos U22) "X")
                                   U23 U31 U32 U33)))

(define (update-turn turn x-col y-row)
  (local [(define unit-num (sub1 (cond [(and (= 1 x-col) (= 1 y-row)) 1]
                                       [(and (= 2 x-col) (= 1 y-row)) 2]
                                       [(and (= 3 x-col) (= 1 y-row)) 3]
                                       [(and (= 1 x-col) (= 2 y-row)) 4]
                                       [(and (= 2 x-col) (= 2 y-row)) 5]
                                       [(and (= 3 x-col) (= 2 y-row)) 6]
                                       [(and (= 1 x-col) (= 3 y-row)) 7]
                                       [(and (= 2 x-col) (= 3 y-row)) 8]
                                       [(and (= 3 x-col) (= 3 y-row)) 9])))]
    (if (or (win? (turn-board turn) (turn-player turn))
            (win? (turn-board turn) (next-turn turn))
            (not (string=? "" (unit-space (list-ref (turn-board turn)
                                                    unit-num)))))
        turn
        (make-turn (next-turn turn)
                   (update-board turn unit-num)))))




(check-expect (next-turn (make-turn "X" empty)) "O")
(check-expect (next-turn (make-turn "O" empty)) "X")

(define (next-turn turn) (if (string=? (turn-player turn) "X") "O" "X"))




(check-expect (update-board START 0) (list (make-unit (unit-x-pos U11)
                                                      (unit-y-pos U11)
                                                      "X")
                                           U12 U13
                                           U21 U22 U23
                                           U31 U32 U33))
(check-expect (update-board START 2) (list U11 U12 (make-unit (unit-x-pos U13)
                                                              (unit-y-pos U13)
                                                              "X")
                                           U21 U22 U23
                                           U31 U32 U33))
(check-expect (update-board START 8) (list U11 U12 U13
                                           U21 U22 U23
                                           U31 U32 (make-unit (unit-x-pos U33)
                                                              (unit-y-pos U33)
                                                              "X")))

(define (update-board turn unit-num)
  (local [(define (create count)
            (if (= count 9) empty
                (local [(define current-unit (list-ref (turn-board turn) count))]
                  (cond [(= count unit-num)
                         (cons (make-unit (unit-x-pos current-unit)
                                          (unit-y-pos current-unit)
                                          (turn-player turn))
                               (create (add1 count)))]
                        [else
                         (cons current-unit (create (add1 count)))]))))]
    (create 0)))




(check-expect (render START) BOARD-IMG)
(check-expect (render (make-turn "O" (list U11 U12 U13 U21
                                           (make-unit (unit-x-pos U22)
                                                      (unit-y-pos U22) "X")
                                           U23 U31 U32 U33)))
              (place-image (text "X" FONT-SIZE "red")
                           (+ (/ UNIT-SIZE 2) (unit-x-pos U22))
                           (+ (/ UNIT-SIZE 2) (unit-y-pos U22))
                           BOARD-IMG))
(check-expect (render (make-turn "X" (list U11 U12 U13 U21
                                           (make-unit (unit-x-pos U22)
                                                      (unit-y-pos U22) "O")
                                           U23 U31 U32 U33)))
              (place-image (text "O" FONT-SIZE "blue")
                           (+ (/ UNIT-SIZE 2) (unit-x-pos U22))
                           (+ (/ UNIT-SIZE 2) (unit-y-pos U22))
                           BOARD-IMG))

(define (render turn)
  (local [(define board (turn-board turn))
          (define (curr num) (list-ref board num))
          (define (choose-color unit)
            (if (string=? (unit-space unit) "X") "red" "blue"))
          (define (create count)
            (cond [(= count 9) BOARD-IMG]
                  [else
                   (place-image (text (unit-space (curr count)) FONT-SIZE
                                      (choose-color (curr count)))
                                (+ (/ UNIT-SIZE 2) (unit-x-pos (curr count)))
                                (+ (/ UNIT-SIZE 2) (unit-y-pos (curr count)))
                                (create (add1 count)))]))]
    (cond [(win? (turn-board turn) (next-turn turn))
           (place-image (text (string-append (next-turn turn)
                                             " Wins!")
                              (* 2 FONT-SIZE) "black")
                        (/ SIZE 2) (/ SIZE 2)
                        (create 0))]
          [(tied? (turn-board turn))
           (place-image (text "TIE"
                              (* 2 FONT-SIZE) "black")
                        (/ SIZE 2) (/ SIZE 2)
                        (create 0))]
          [else (create 0)])))




(check-expect (win? (turn-board START) "X") false)
(check-expect (win? (list (make-unit (unit-x-pos U11)
                                     (unit-y-pos U11)
                                     "X")
                          (make-unit (unit-x-pos U11)
                                     (unit-y-pos U11)
                                     "X")
                          (make-unit (unit-x-pos U11)
                                     (unit-y-pos U11)
                                     "X")
                          U21 U22 U23 U31 U32 U33) "X") true)

(define (win? lou player)
  (local [(define (value num) (unit-space (list-ref lou num)))]
    (or (string=? player (value 0) (value 1) (value 2))
        (string=? player (value 3) (value 4) (value 5))
        (string=? player (value 6) (value 7) (value 8))
        (string=? player (value 0) (value 3) (value 6))
        (string=? player (value 1) (value 4) (value 7))
        (string=? player (value 2) (value 5) (value 8))
        (string=? player (value 0) (value 4) (value 8))
        (string=? player (value 6) (value 4) (value 2)))))




(check-expect (tied? (turn-board START)) false)
(check-expect (tied? (list
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X")
                      (make-unit (unit-x-pos U11)
                                 (unit-y-pos U11)
                                 "X"))) true)

(define (tied? lou)
  (local [(define (value num) (unit-space (list-ref lou num)))
          (define (not-empty str) (not (string=? str "")))]
    (and (not-empty (value 0))(not-empty (value 1))(not-empty (value 2))
         (not-empty (value 3))(not-empty (value 4))(not-empty (value 5))
         (not-empty (value 6))(not-empty (value 7))(not-empty (value 8)))))
