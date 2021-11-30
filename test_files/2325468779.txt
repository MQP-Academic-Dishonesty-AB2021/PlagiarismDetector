

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname assignment4_nathan_dynko_ian_poulsen) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 900) 
(define COLOR "salmon")
(define LINE (make-pen COLOR 15 "solid" "round" "round"))
(define MTS (empty-scene SIZE SIZE))


(define BD1
  (list "a" "b" "c"
        "d" "e" "f" 
        "g" "h" "i"))


(define BD2
  (list "a" "X" "c"
        "d" "e" "X" 
        "g" "O" "i"))

(define BD3
  (list "X" "O" "X"
        "O" "O" "X" 
        "X" "X" "O"))
(define BD4
  (list "X" "X" "X"
        "O" "X" "O" 
        "O" "X" "O"))


(define left-x (/ SIZE 6))
(define middle-x (/ SIZE 2))
(define right-x (* 5 (/ SIZE 6)))

(define top-y (/ SIZE 6))
(define center-y (/ SIZE 2))
(define bottom-y (* 5 (/ SIZE 6)))


(define BG
  (add-line
   (add-line
    (add-line
     (add-line
      MTS 20 (* 2 (/ SIZE 3)) (- SIZE 20) (* 2 (/ SIZE 3)) LINE)
     20 (/ SIZE 3) (- SIZE 20) (/ SIZE 3) LINE)
    (* 2(/ SIZE 3)) 20 (* 2(/ SIZE 3)) (- SIZE 20) LINE)
   (/ SIZE 3) 20 (/ SIZE 3) (- SIZE 20) LINE))








(define-struct player (symbol image))








(define-struct bs (img player logic-bd turn))





(define X (make-player "X" (text "X" (/ SIZE 4) "firebrick")))  
(define O (make-player "O" (text "O" (/ SIZE 4) "cornflowerblue")))



(check-expect (read-sqr BD1 2) "c")

(define (read-sqr bd pos) 
  (list-ref bd pos))




(check-expect (fill-square BD1 2 "X")
              (list "a" "b" "X"
                    "d" "e" "f" 
                    "g" "h" "i"))

(define (fill-square bd pos new-val)
  (append (take bd pos)
          (list new-val)
          (drop bd (add1 pos))))
          
          




(define START (make-bs BG X BD1 0))


(define TEST1 (make-bs BG X BD2 0))
(define TEST2 (make-bs BG X BD3 0))
(define TEST3 (make-bs BG X BD4 0))

(define (main bs)
  (big-bang bs
    (on-tick turn?)                     
    (to-draw render-board)              
    (on-mouse handle-mouse)             
    
    (stop-when game-over? display-end)  
    )) 




(check-expect (render-board START) BG)

(define (render-board bs)
  (bs-img bs))







(define (turn? bs)
  (if (even? (bs-turn bs))
      (make-bs (bs-img bs) X (bs-logic-bd bs) (bs-turn bs))
      (handle-mouse
       (make-bs
        (bs-img bs) O (bs-logic-bd bs)
        (bs-turn bs)) (random (- SIZE 5))
                      (random (- SIZE 5)) "button-down")))







(define (handle-mouse bs x y me)

  (local
    [
     (define (check-left-x x)   
       (if (and (> x 0) (< x (/ SIZE 3)))
           true
           false))
     (define (check-middle-x x) 
       (if (and (> x (/ SIZE 3)) (< x (* 2 (/ SIZE 3))))
           true
           false))
     (define (check-right-x x)  
       (if (and (> x (* 2 (/ SIZE 3))) (< x SIZE))
           true
           false)) 

     
     (define (check-top-y y)    
       (if (and (> y 0) (< y (/ SIZE 3)))
           true
           false))
     (define (check-middle-y y) 
       (if (and (> y (/ SIZE 3)) (< y (* 2 (/ SIZE 3))))
           true
           false))
     (define (check-bottom-y y) 
       (if (and (> y (* 2 (/ SIZE 3))) (< y SIZE)) 
           true
           false))
     
     (define (valid? index)
       (not (or (string=?
                 (list-ref (bs-logic-bd bs) index) "X")
                (string=? (list-ref (bs-logic-bd bs) index) "O"))))]

    
    
    
    (cond
      [(mouse=? me "button-down")
       (cond
         
         [(and (check-left-x x)(check-top-y y))
          (if (valid? 0)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                left-x top-y (bs-img bs))
               (bs-player bs)
               (fill-square
                (bs-logic-bd bs) 0
                (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-middle-x x)(check-top-y y))
          (if (valid? 1)
              (make-bs
               (place-image (player-image (bs-player bs))
                            middle-x top-y (bs-img bs))
               (bs-player bs)
               (fill-square
                (bs-logic-bd bs) 1
                (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-right-x x)(check-top-y y))
          (if (valid? 2)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                right-x top-y (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 2
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]

         
         [(and (check-left-x x)(check-middle-y y))
          (if (valid? 3)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                left-x center-y(bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 3
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-middle-x x)(check-middle-y y))
          (if (valid? 4)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                middle-x center-y (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 4
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-right-x x)(check-middle-y y))
          (if (valid? 5)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                right-x center-y (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 5
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]

         
         [(and (check-left-x x)(check-bottom-y y))
          (if (valid? 6)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                left-x bottom-y (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 6
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-middle-x x)(check-bottom-y y))
          (if (valid? 7)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                middle-x bottom-y (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 7
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         
         [(and (check-right-x x)(check-bottom-y y))
          (if (valid? 8)
              (make-bs
               (place-image
                (player-image (bs-player bs))
                right-x bottom-y
                (bs-img bs))
               (bs-player bs)
               (fill-square (bs-logic-bd bs) 8
                            (player-symbol (bs-player bs)))
               (add1 (bs-turn bs)))
              bs)]
         [else bs])]
      
      [else bs])))  



(check-expect (winning? START) false) 
(check-expect (winning? TEST3) true)  

(define (winning? bs)  
  (local
    [
     (define a (list-ref (bs-logic-bd bs) 0))
     (define b (list-ref (bs-logic-bd bs) 1))
     (define c (list-ref (bs-logic-bd bs) 2))
     (define d (list-ref (bs-logic-bd bs) 3))
     (define e (list-ref (bs-logic-bd bs) 4))
     (define f (list-ref (bs-logic-bd bs) 5))
     (define g (list-ref (bs-logic-bd bs) 6))
     (define h (list-ref (bs-logic-bd bs) 7))
     (define i (list-ref (bs-logic-bd bs) 8))]
      
    (cond
      [(and (string=? a b) (string=? b c)) true]
      [(and (string=? d e) (string=? e f)) true]
      [(and (string=? g h) (string=? h i)) true]
      [(and (string=? a d) (string=? d g)) true]
      [(and (string=? b e) (string=? e h)) true]
      [(and (string=? c f) (string=? f i)) true]
      [(and (string=? a e) (string=? e i)) true]
      [(and (string=? c e) (string=? e g)) true]
      [else false])))





(check-expect (board-full? START) false) 
(check-expect (board-full? TEST1) false) 
(check-expect (board-full? TEST3) true)  

(define (board-full? bs)
  (andmap (λ (n) (or (string=? n "X") (string=? n "O"))) (bs-logic-bd bs)))




(check-expect (tied? START) false) 
(check-expect (tied? TEST3) false) 
(check-expect (tied? TEST2) (place-image (text "tie" (/ SIZE 4) "black")
                   (/ SIZE 2) (/ SIZE 2) BG)) 

(define (tied? bs)
  (if (and (board-full? bs) (not (winning? bs)))
      (place-image (text "tie" (/ SIZE 4) "black")
                   (/ SIZE 2) (/ SIZE 2) (bs-img bs))
      false))





(check-expect (game-over? START) false) 

(define (game-over? bs)
  (cond [(winning? bs) true]
        [(not (false? (tied? bs))) true]
        [else false]))



(define (display-end bs)
  (cond [(winning? bs)
         (place-image
          (text (string-append (player-symbol (bs-player bs)) " wins!")
                (/ SIZE 4) "black") (/ SIZE 2) (/ SIZE 2) (bs-img bs))]
        
        [(not (false? (tied? bs))) 
         (place-image
          (text "tie" (/ SIZE 4) "black") (/ SIZE 2) (/ SIZE 2) (bs-img bs))]
        
        [else empty-image])) 



(check-expect (possible-moves START)
              (list "a" "b" "c" "d" "e" "f" "g" "h" "i")) 
(check-expect (possible-moves TEST3) empty)               
(check-expect (possible-moves TEST1)                      
              (list "a" "c" "d" "e" "g" "i"))

(define (possible-moves bs)
  (filter (λ (n) (not (or (string=? n "X") (string=? n "O"))))
            (bs-logic-bd bs)))

 

