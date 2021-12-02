

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |tic tac toe|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))

(define HORI-LINE (rectangle (* SIZE 4/5) (* SIZE 1/25) "solid" "orange"))
(define VERT-LINE (rectangle (* SIZE 1/25) (* SIZE 4/5) "solid" "orange"))

(define GRID
  (overlay
   (overlay/offset HORI-LINE
                   0 (/ SIZE 3.5) 
                   HORI-LINE)
   (overlay/offset VERT-LINE
                   (/ SIZE 3.5) 0
                   VERT-LINE)))

(define X-IMG
  (rotate 45
          (overlay
           (rectangle (/ SIZE 6) (/ SIZE 26) "solid" "red")
           (rectangle (/ SIZE 26) (/ SIZE 6) "solid" "red"))))

(define O-IMG
  (overlay
   (circle (/ SIZE 20) "solid" "white")
   (circle (/ SIZE 12) "solid" "blue")))

(define END-FONT-SIZE (/ SIZE 5))
(define END-COLOR "green")

















(define B1 (list " " " " " "
                 " " " " " "
                 " " " " " "))

(define B2 (list "X" "O" " "
                 "O" "X" " "
                 " " " " " "))

(define B3 (list "O" " " "X"
                 "O" "X" " "
                 "O" " " " "))


(define-struct game (board winner depth))





(define START (make-game B1 " " 0))
(define G2 (make-game B2 " " 1))
(define G3 (make-game B3 "O" 2))

               








(define (main g)
  (big-bang g               
    (to-draw render)        
    (on-mouse handle-mouse) 
    (on-key handle-key)))   




(check-expect (render START)
              (place-image GRID (/ SIZE 2) (/ SIZE 2) MTS))

(check-expect (render G2)
              (place-images
               (list GRID X-IMG X-IMG O-IMG O-IMG)              
               (list
                (make-posn (/ SIZE 2) (/ SIZE 2))
                (make-posn (+ (* (quotient 0 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 0 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 4 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 4 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 1 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 1 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 3 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 3 3) (* SIZE 7/25))(* SIZE 11/50))))
               MTS))

(check-expect (render G3)
              (place-images
               (list
                (overlay
                 (text "O WINS" END-FONT-SIZE END-COLOR)
                 GRID)
                X-IMG X-IMG O-IMG O-IMG O-IMG)              
               (list
                (make-posn (/ SIZE 2) (/ SIZE 2))
                (make-posn (+ (* (quotient 2 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 2 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 4 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 4 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 0 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 0 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 3 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 3 3) (* SIZE 7/25))(* SIZE 11/50)))
                (make-posn (+ (* (quotient 6 3) (* SIZE 7/25))(* SIZE 11/50))
                           (+ (* (remainder 6 3) (* SIZE 7/25))(* SIZE 11/50))))
               MTS))



(define (render g)
  (local [(define board (game-board g))
          (define (render-tiles b)
            (local [(define (render-t b index)
                      (cond [(empty? b) MTS]
                            [else
                             (place-image (cond [(string=? (first b) "X")
                                                 X-IMG]
                                                [(string=? (first b) "O")
                                                 O-IMG]
                                                [else empty-image])
                                          (+ (* (quotient index 3) (* SIZE 7/25))
                                             (* SIZE 11/50))
                                          (+ (* (remainder index 3) (* SIZE 7/25))
                                             (* SIZE 11/50))
                                          (render-t (rest b) (add1 index)))]))]
              (render-t b 0)))
          (define (render-winner g)
            (cond [(string=? (game-winner g) " ")
                   empty-image]
                  [(string=? (game-winner g) "DRAW")
                   (text "DRAW" END-FONT-SIZE END-COLOR)]
                  [else
                   (text (string-append (game-winner g) " WINS")
                         END-FONT-SIZE END-COLOR)]))]
    (place-image (render-winner g) (/ SIZE 2) (/ SIZE 2)
                 (place-image GRID (/ SIZE 2) (/ SIZE 2)
                              (render-tiles board)))))





(check-expect (contains? START 0 0) false) 
(check-expect (contains? (make-game B2 " " 0) 4 2) true) 
(check-expect (contains? (make-game B3 "O" 0) 1 4) false) 



(define (contains? g index num)
  (and (string=? (list-ref (game-board g) index) "X")
       (= (length (filter (λ (t) (string=? t "O")) (game-board g))) num)
       (ormap (λ (t) (string=? t "O")) (game-board g))
       (string=? (game-winner g) " ")
       (= (game-depth g) 0)))




(check-satisfied (handle-mouse START 0 0
                               "button-down") 
                 (λ (g) (contains? g 0 1)))
                 
(check-satisfied (handle-mouse START (/ SIZE 2) (/ SIZE 2)
                               "button-down") 
                 (λ (g) (contains? g 4 1)))

(check-satisfied (handle-mouse START (* SIZE 1/3) (* SIZE 2/3)
                               "button-down") 
                 (λ (g) (contains? g 1 1)))

(check-expect (handle-mouse (make-game B1 " " 1) 0 0 
                            "button-down")
              (make-game (list "X" " " " "
                               " " " " " "
                               " " " " "O")
                         " " 1))

(check-expect (handle-mouse G2 SIZE SIZE 
                            "button-down")
              (make-game (list "X" "O" " "
                               "O" "X" " "
                               " " " " "X")
                         "X" 1))

(check-expect (handle-mouse (make-game B3 "O" 1) (/ SIZE 3) SIZE 
                            "button-down")
              (make-game B3 "O" 1))

(check-expect (handle-mouse (make-game B1 " " 2) 0 0 
                            "button-down")
              (make-game (list "X" " " " "
                               " " " " " "
                               " " " " "O")
                         " " 2))

(check-expect (handle-mouse (make-game B2 " " 2) (/ SIZE 3) SIZE 
                            "button-down")
              (make-game (list "X" "O" "X"
                               "O" "X" " "
                               "O" " " " ")
                         " " 2))

(check-expect (handle-mouse G3 SIZE SIZE 
                            "button-down")
              G3)



(define (handle-mouse g x y me)
  (if (and (string=? me "button-down")
           (string=? (game-winner g) " "))
      (local [(define pos (+ (* (quotient (sub1 x) (quotient SIZE 3)) 3)
                             (quotient (sub1 y) (quotient SIZE 3))))]
        (if (string=? (list-ref (game-board g) pos) " ")
            (local [(define new-board (list-set (game-board g) pos "X"))
                    (define game-over-new-board (game-over new-board))]
              (cond [(or (string=? game-over-new-board "X")
                         (string=? game-over-new-board "DRAW"))
                     (make-game new-board game-over-new-board (game-depth g))]
                    [(or (= (game-depth g) 0)
                         (= (game-depth g) 1)
                         (= (game-depth g) 2))
                     (local [(define new-board-comp
                               (list-set new-board
                                         (comp-move new-board
                                                    (game-depth g))
                                         "O"))]
                       (make-game new-board-comp
                                  (game-over new-board-comp)
                                  (game-depth g)))]
                    [else
                     (error "How did we get here?")]))
            g))
      g))




(check-expect (game-over B1) " ") 
(check-expect (game-over B2) " ") 
(check-expect (game-over B3) "O") 
(check-expect (game-over (list " " " " "X" 
                               "O" "X" " "
                               "X" "O" " "))
              "X")
(check-expect (game-over (list "X" "O" "X" 
                               "X" "X" "O"
                               "O" "X" "O"))
              "DRAW")



(define (game-over b)
  (local [(define S1 (list-ref b 0))
          (define S2 (list-ref b 1))
          (define S3 (list-ref b 2))
          (define S4 (list-ref b 3))
          (define S5 (list-ref b 4))
          (define S6 (list-ref b 5))
          (define S7 (list-ref b 6))
          (define S8 (list-ref b 7))
          (define S9 (list-ref b 8))]
    (cond [(and (not (string=? S1 " "))
                (or (string=? S1 S2 S3)
                    (string=? S1 S4 S7)))
           S1]
          [(and (not (string=? S5 " "))
                (or (string=? S4 S5 S6)
                    (string=? S2 S5 S8)
                    (string=? S1 S5 S9)
                    (string=? S3 S5 S7)))
           S5]
          [(and (not (string=? S9 " "))
                (or (string=? S7 S8 S9)
                    (string=? S3 S6 S9)))
           S9]
          [(not (ormap (λ (t) (string=? t " ")) (list S1 S2 S3
                                                      S4 S5 S6
                                                      S7 S8 S9)))
           "DRAW"]
          [else " "])))





(check-satisfied (comp-move B1 0) 
                 (λ (pos) (string=? (list-ref B1 pos) " ")))
(check-satisfied (comp-move B2 0) 
                 (λ (pos) (string=? (list-ref B2 pos) " ")))
(check-satisfied (comp-move B3 0) 
                 (λ (pos) (string=? (list-ref B3 pos) " ")))

(check-expect (comp-move B1 1) 8) 
(check-expect (comp-move B2 1) 8) 
(check-expect (comp-move B3 1) 1) 

(check-expect (comp-move B1 2) 8) 
(check-expect (comp-move B2 2) 8) 
(check-expect (comp-move B3 2) 1) 



(define (comp-move b depth)
  (local [
          
          
          (define (make-poss-pos b)
            (local [(define (make-pos index)
                      (cond [(> index 8) empty]
                            [(string=? (list-ref b index) " ")
                             (cons index
                                   (make-pos (add1 index)))]
                            [else
                             (make-pos (add1 index))]))]
              (make-pos 0)))
          
          
          
          (define (comp-move-1 lon state)
            (cond [(= (length lon) 1) (first lon)]
                  [(string=? (game-over (list-set b (first lon) state))
                             state)
                   (first lon)]
                  [else
                   (comp-move-1 (rest lon) state)]))]
    (cond [(= depth 0)
           
           
           (local [(define (computer-move-0 r)
                     (if (string=? (list-ref b r) " ")
                         r
                         (computer-move-0 (random 9))))]
             (computer-move-0 (random 9)))]
          [(= depth 1)
           (comp-move-1 (make-poss-pos b) "O")]
          [(= depth 2)
           (local [(define pos1 (comp-move b 1))]
             (if (string=? (game-over (list-set b pos1 "O"))
                           "O")
                 pos1
                 (comp-move-1 (make-poss-pos b) "X")))])))





(check-expect (handle-key START "") 
              (make-game B1 " " 0))
(check-expect (handle-key START "0") 
              (make-game B1 " " 0))
(check-expect (handle-key G2 "2") 
              (make-game B2 " " 2))
(check-expect (handle-key G3 "1") 
              (make-game B3 "O" 1))



(define (handle-key g ke)
  (make-game (game-board g) (game-winner g)
             (cond [(or (string=? ke "0")
                        (string=? ke "1")
                        (string=? ke "2"))
                    (string->number ke)]
                   [else 0])))