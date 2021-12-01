

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4 Nathaniel and Jake|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 





(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define blank (square 0 "solid" "white"))
(define Xcolor "red")
(define Ocolor "blue")
(define XOfont (/ SIZE 6))
(define Lines (/ SIZE 20))
(define Wcolor "green")
(define Wfont (/ SIZE 5))
(define Xtext (text "X" XOfont Xcolor))
(define Otext (text "O" XOfont Ocolor))
(define Rrow (rectangle (* .9 SIZE) Lines "solid" "orange"))
(define Rcolumn (rectangle Lines (* .9 SIZE) "solid" "orange"))

(define row1 (list 0 1 2))
(define row2 (list 3 4 5))
(define row3 (list 6 7 8))
(define rows (list row1 row2 row3))

(define col1 (list 0 3 6))
(define col2 (list 1 4 7))
(define col3 (list 2 5 8))
(define cols (list col1 col2 col3))

(define dia1 (list 0 4 8))
(define dia2 (list 2 4 6))
(define diags (list dia1 dia2))

(define X 1)
(define O 0)
(define B false)


(define BD0
  (list B B B
        B B B
        B B B))
(define BD1
  (list X B O
        B B B
        B B X))




(define-struct unit (pos1 pos2 pos3))






(define UNIT1 (make-unit 0 1 2))
(define UNIT2 (make-unit 3 4 5))

 


(define UNITS
  (list (make-unit 0 1 2)
        (make-unit 3 4 5)
        (make-unit 6 7 8)
        (make-unit 0 3 6)
        (make-unit 1 4 7)
        (make-unit 2 5 8)
        (make-unit 0 4 8)
        (make-unit 2 4 6)))


(define-struct board (LoC difficulty move))





 

(define START (make-board BD0 0 1))





(define (main board)
  (big-bang board               
    (on-tick   tock)            
    (to-draw   render)          
    (on-mouse  placex)          
    (on-key    handle-key)))    







(define (tock board)
  (if (even? (board-move board)) 
      (Ofunction board) 
      board))







(check-expect (render (make-board (list B B B B O B B B X) 0 2))
              (place-images (append (list
                                     (won? (make-board (list B B B B O B B B X) 0 2) X)
                                     (won? (make-board (list B B B B O B B B X) 0 2) O)
                                     (draw? (make-board (list B B B B O B B B X) 0 2))
                                     Rrow Rrow Rcolumn Rcolumn)
                                    (render-board (list B B B B O B B B X)))
                            (append (list (make-posn (/ SIZE 2) (/ SIZE 2))
                                          (make-posn (/ SIZE 2) (/ SIZE 2))
                                          (make-posn (/ SIZE 2) (/ SIZE 2))
                                          (make-posn (/ SIZE 2) (/ SIZE 3))
                                          (make-posn (/ SIZE 2) (* 2 (/ SIZE 3)))
                                          (make-posn  (/ SIZE 3) (/ SIZE 2))
                                          (make-posn  (* 2 (/ SIZE 3)) (/ SIZE 2)))
                                    (list (make-posn (/ SIZE 6) (/ SIZE 6))
                                          (make-posn (* 3 (/ SIZE 6)) (/ SIZE 6))
                                          (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                                          (make-posn (/ SIZE 6) (* 3(/ SIZE 6)))
                                          (make-posn (* 3 (/ SIZE 6)) (* 3 (/ SIZE 6)))
                                          (make-posn (* 5 (/ SIZE 6)) (* 3 (/ SIZE 6)))
                                          (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                                          (make-posn (* 3 (/ SIZE 6)) (* 5 (/ SIZE 6)))
                                          (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6)))))
                              
                            MTS))

(define (render board)
  (place-images (append (list (won? board X) (won? board O) (draw? board)
                              Rrow Rrow Rcolumn Rcolumn) (render-board (board-LoC board)))
                (append (list (make-posn (/ SIZE 2) (/ SIZE 2))
                              (make-posn (/ SIZE 2) (/ SIZE 2))
                              (make-posn (/ SIZE 2) (/ SIZE 2))
                              (make-posn (/ SIZE 2) (/ SIZE 3))
                              (make-posn (/ SIZE 2) (* 2 (/ SIZE 3)))
                              (make-posn  (/ SIZE 3) (/ SIZE 2))
                              (make-posn  (* 2 (/ SIZE 3)) (/ SIZE 2)))
                        (list (make-posn (/ SIZE 6) (/ SIZE 6))
                              (make-posn (* 3 (/ SIZE 6)) (/ SIZE 6))
                              (make-posn (* 5 (/ SIZE 6)) (/ SIZE 6))
                              (make-posn (/ SIZE 6) (* 3(/ SIZE 6)))
                              (make-posn (* 3 (/ SIZE 6)) (* 3 (/ SIZE 6)))
                              (make-posn (* 5 (/ SIZE 6)) (* 3 (/ SIZE 6)))
                              (make-posn (/ SIZE 6) (* 5 (/ SIZE 6)))
                              (make-posn (* 3 (/ SIZE 6)) (* 5 (/ SIZE 6)))
                              (make-posn (* 5 (/ SIZE 6)) (* 5 (/ SIZE 6)))))
                              
                MTS))











(check-expect (render-board BD0) (list blank blank blank
                                       blank blank blank
                                       blank blank blank))
(check-expect (render-board BD1) (list Xtext blank Otext
                                       blank blank blank
                                       blank blank Xtext))
(define (render-board loc)
  (cond [(empty? loc) empty]
        [(false? (first loc)) (cons blank (render-board (rest loc)))]
        [(= (first loc) X) (cons Xtext (render-board (rest loc)))]
        [(= (first loc) O) (cons Otext (render-board (rest loc)))]))

(check-expect (render-board BD0) (list blank blank blank
                                       blank blank blank
                                       blank blank blank))
(check-expect (render-board BD1) (list Xtext blank Otext
                                       blank blank blank
                                       blank blank Xtext))









(check-expect (placex (make-board BD0 0 1) 1 1 "button-down")
              (make-board (list X B B B B B B B B) 0 2
                          ))
(check-expect (placex (make-board BD0 0 1) 105 1 "button-down")
              (make-board (list B X B B B B B B B) 0 2
                          ))

(define (placex board x-cor y-cor MouseEvent)
  (if (mouse=? MouseEvent "button-down")
      (cond
        [(or (checkwon? board X) (checkwon? board O)
             (image=? (draw? board) (text "DRAW" Wfont Wcolor))) board]
        [(and (< x-cor (/ SIZE 3)) (< y-cor (/ SIZE 3)) (false? (list-ref (board-LoC board) 0)))
         (make-board (fill-square  board 0 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (> x-cor (/ SIZE 3)) (< x-cor (* 2 (/ SIZE 3)))
              (< y-cor (/ SIZE 3)) (false? (list-ref (board-LoC board) 1)))
         (make-board (fill-square  board 1 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (< x-cor SIZE) (> x-cor (* 2 (/ SIZE 3))) (< y-cor (/ SIZE 3))
              (false? (list-ref (board-LoC board) 2)))
         (make-board (fill-square  board 2 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (< x-cor (/ SIZE 3)) (< y-cor (* 2 (/ SIZE 3))) (> y-cor (/ SIZE 3))
              (false? (list-ref (board-LoC board) 3)))
         (make-board (fill-square  board 3 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (> x-cor (/ SIZE 3)) (< x-cor (* 2 (/ SIZE 3))) (< y-cor (* 2 (/ SIZE 3)))
              (> y-cor (/ SIZE 3)) (false? (list-ref (board-LoC board) 4)))
         (make-board (fill-square  board 4 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (< x-cor SIZE) (> x-cor (* 2 (/ SIZE 3))) (< y-cor (* 2 (/ SIZE 3)))
              (> y-cor (/ SIZE 3)) (false? (list-ref (board-LoC board) 5)))
         (make-board (fill-square  board 5 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (< x-cor (/ SIZE 3)) (> y-cor (* 2 (/ SIZE 3))) (< y-cor SIZE)
              (false? (list-ref (board-LoC board) 6)))
         (make-board (fill-square  board 6 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (> x-cor (/ SIZE 3)) (< x-cor (* 2 (/ SIZE 3)))
              (> y-cor (* 2 (/ SIZE 3))) (< y-cor SIZE) (false? (list-ref (board-LoC board) 7)))
         (make-board (fill-square  board 7 X) (board-difficulty board) (+ 1 (board-move board)))]
        [(and (> x-cor (* 2 (/ SIZE 3))) (< x-cor SIZE)
              (> y-cor (* 2 (/ SIZE 3))) (< y-cor SIZE) (false? (list-ref (board-LoC board) 8)))
         (make-board (fill-square  board 8 X) (board-difficulty board) (+ 1 (board-move board)))]
        [else
         (make-board (board-LoC board) (board-difficulty board) (board-move board))])
      (make-board (board-LoC board) (board-difficulty board) (board-move board))))











(check-expect (fill-square (make-board BD1 1 1) 0 X)
              (cons X (rest (board-LoC (make-board BD1 1 1)))))

(check-expect (fill-square (make-board BD1 1 1) 0 O)
              (cons O (rest (board-LoC (make-board BD1 1 1)))))


(check-expect (fill-square (make-board BD1 1 1) 1 X)
              (append (list (first (board-LoC (make-board BD1 1 1))))
                      (list X)
                      (rest (rest (board-LoC (make-board BD1 1 1))))))

(define (fill-square bd p nv)
  (append (take (board-LoC bd) p)
          (list nv)
          (drop (board-LoC bd) (add1 p))))






(define (Ofunction board)
  (local [(define (nextboard board)
  (local [(define (forwin board)
            (list? (takewin board)))
          (define (takewin board)
            (local [(define (checkwin L)
            (cond [(empty? L) false]
                  [else
                   (if (unitwin board (first L))
                       (steal-position board (first L))
                       (checkwin (rest L)))]))]
    (checkwin UNITS)))
          (define (block? board)
(list? (next-block board)))
          (define (win? board)
  (list? (next-win board)))
          (define (next-win board)
  (local [(define (check L)
            (cond [(empty? L) false]
                  [else
                   (if (next-win? board (first L))
                       (cond [(not (eqv? (list-ref (board-LoC board) (unit-pos1 (first L))) O))
                              (fill-board (board-LoC board) (unit-pos1 (first L)) O)]
                             [(not (eqv? (list-ref (board-LoC board) (unit-pos2 (first L))) O))
                              (fill-board (board-LoC board) (unit-pos2 (first L)) O)]
                             [else
                              (fill-board (board-LoC board) (unit-pos3 (first L)) O)])
                       (check (rest L)))]))]
    (check UNITS)))
          (define (next-move board)
  (local [(define r-pos (random (length (board-LoC board))))]
    (if (false? (list-ref (board-LoC board) r-pos))
        (fill-board (board-LoC board) r-pos O)
        (next-move board))))
          (define (next-block board)
  (local [(define (checkb L)
            (cond [(empty? L) false]
                  [else
                   (if (unit-block? board (first L))
                       (steal-position board (first L))
                       (checkb (rest L)))]))]
    (checkb UNITS)))
          (define (next-win? board unit)
  (and (not (eqv? (list-ref (board-LoC board) (unit-pos1 unit)) X))
       (not (eqv? (list-ref (board-LoC board) (unit-pos3 unit)) X))
       (not (eqv? (list-ref (board-LoC board) (unit-pos3 unit)) X))))
          (define (unitwin board unit)
  (= (count-O (board-LoC board) unit) 2))
          (define (unit-block? board unit)
  (= (count-X (board-LoC board) unit) 2))
          (define (count-X boardlist unit)
  (local [(define L (list (list-ref boardlist (unit-pos1 unit))
                          (list-ref boardlist (unit-pos2 unit))
                          (list-ref boardlist (unit-pos3 unit))))]
    (- (length (filter (lambda (x) (eqv? X x)) L)) (length (filter (lambda (x) (eqv? O x)) L)))))
          (define (count-O boardlist unit)
  (local [(define L (list (list-ref boardlist (unit-pos1 unit))
                          (list-ref boardlist (unit-pos2 unit))
                          (list-ref boardlist (unit-pos3 unit))))]
    (- (length (filter (lambda (x) (eqv? O x)) L)) (length (filter (lambda (x) (eqv? X x)) L)))))
          (define (steal-position board unit)
  (cond [(false? (list-ref (board-LoC board) (unit-pos1 unit)))
         (fill-board (board-LoC board) (unit-pos1 unit) O)]
        [(false? (list-ref (board-LoC board) (unit-pos2 unit)))
         (fill-board (board-LoC board) (unit-pos2 unit) O)]
        [(false? (list-ref (board-LoC board) (unit-pos3 unit)))
         (fill-board (board-LoC board) (unit-pos3 unit) O)]))
(define (fill-board list p nv)
  (cond [(zero? p) (cons nv (rest list))]
        [else
         (cons (first list) (fill-board (rest list) (sub1 p) nv))]))
          ]
  (cond [(= (board-difficulty board) 2)
         (cond [(forwin board)
                (takewin board)]
               [(block? board)
                (next-block board)]
               [(win? board) 
                (next-win board)]
               [else
                (next-move board)])]
        [(= (board-difficulty board) 1)
         (if (forwin board)
             (takewin board)
             (next-move board))]
        [else
         (next-move board)])))]
  (cond [(or (checkwon? board X) (checkwon? board O)
             (image=? (draw? board) (text "DRAW" Wfont Wcolor))) board]
        [(= (board-difficulty board) 0)
         (local [(define r (random 9))]
           (if (false? (list-ref (board-LoC board) r))
               (make-board (fill-square board r O)
                           (board-difficulty board)
                           (+ 1 (board-move board))
                           )
               (Ofunction board)))]
        [(= (board-difficulty board) 1)
         (make-board (nextboard board)
                     (board-difficulty board)
                     (+ 1 (board-move board))
                     )]
        [(= (board-difficulty board) 2)
         (make-board (nextboard board)
                     (board-difficulty board)
                     (+ 1 (board-move board))
                     )])))








(check-expect (handle-key START "2") (make-board BD0 2 1 ))
(check-expect (handle-key START "8") (make-board BD0 8 1 ))
(check-expect (handle-key START "1") (make-board BD0 1 1 ))

(define (handle-key board ke)
  (cond [(key=? ke "0")
         (make-board (board-LoC board) 0 (board-move board))]
        [(key=? ke "1")
         (make-board (board-LoC board) 1 (board-move board))]
        [(key=? ke "2")
         (make-board (board-LoC board) 2 (board-move board))]
        [(key=? ke "3")
         (make-board (board-LoC board) 3 (board-move board))]
        [(key=? ke "4")
         (make-board (board-LoC board) 4 (board-move board))]
        [(key=? ke "5")
         (make-board (board-LoC board) 5 (board-move board))]
        [(key=? ke "6")
         (make-board (board-LoC board) 6 (board-move board))] 
        [(key=? ke "7")
         (make-board (board-LoC board) 7 (board-move board))]
        [(key=? ke "8")
         (make-board (board-LoC board) 8 (board-move board))]
        [(key=? ke "9")
         (make-board (board-LoC board) 9 (board-move board))]
        [else (make-board (board-LoC board) (board-difficulty board))]))







(check-expect (draw? START) blank)
(check-expect (draw? (make-board (list X O O
                                       O X X
                                       X X O)
                                 0 1 ))
              (text "DRAW" Wfont Wcolor))

(define (draw? board)
  (if (and (or (and (false? (checkwon? board X)) (andmap number? (board-LoC board)))
               (and (andmap number? (board-LoC board)) (false? (checkwon? board O))))
           (not (checkwon? board X)) (not (checkwon? board O)))
      (text "DRAW" Wfont Wcolor)
      blank))
 





(check-expect (won? (make-board (list X X X X X X X X X) 0 9 ) X)
              (text "X wins!!!" Wfont Wcolor))
(check-expect (won? (make-board (list O O O O O O O O O) 0 9 ) O)
              (text "O wins!!!" Wfont Wcolor))
(check-expect (won? START X) blank)


(define (won? board char)
  (local [(define (charwon board char)
  (cond [(= char 1)
         (text "X wins!!!" Wfont Wcolor)]
        [(= char 0)
         (text "O wins!!!" Wfont Wcolor)]
        [else
         blank]))]
  (cond [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 1)
                                                   (list-ref (board-LoC board) 2)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 3)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 5)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 6)
                                                   (list-ref (board-LoC board) 7)
                                                   (list-ref (board-LoC board) 8)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 3)
                                                   (list-ref (board-LoC board) 6)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 1)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 7)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 2)
                                                   (list-ref (board-LoC board) 5)
                                                   (list-ref (board-LoC board) 8)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 8)))
          (charwon board char)]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 2)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 6)))
          (charwon board char)]
        [else blank])))
   





(check-expect (checkwon? (make-board (list X X X X X X X X X) 0 9 ) X) true)
(check-expect (checkwon? (make-board (list O O O O O O O O O) 0 9 ) O) true)
(check-expect (checkwon? START X) false)

(define (checkwon? board char)
  (cond [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 1)
                                                   (list-ref (board-LoC board) 2)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 3)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 5)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 6)
                                                   (list-ref (board-LoC board) 7)
                                                   (list-ref (board-LoC board) 8)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 3)
                                                   (list-ref (board-LoC board) 6)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 1)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 7)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 2)
                                                   (list-ref (board-LoC board) 5)
                                                   (list-ref (board-LoC board) 8)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 0)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 8)))
          true]
        [ (andmap (lambda (p) (eqv? p char)) (list (list-ref (board-LoC board) 2)
                                                   (list-ref (board-LoC board) 4)
                                                   (list-ref (board-LoC board) 6)))
          true]
        [else false]))