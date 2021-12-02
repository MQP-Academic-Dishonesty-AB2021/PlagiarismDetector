

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Assignment 4, Kevin & Jamie|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 900) 
(define MTS (empty-scene SIZE SIZE))



(define BOARD
  (place-image (rectangle (/ SIZE 30) (* SIZE .9) "solid"
                          "orange") (/ SIZE 3) (/ SIZE 2)
     (place-image (rectangle (/ SIZE 30) (* SIZE .9) "solid"
                             "orange") (* (/ SIZE 3) 2) (/ SIZE 2)
        (place-image (rectangle (* SIZE .9) (/ SIZE 30) "solid"
                                "orange") (/ SIZE 2) (/ SIZE 3)
           (place-image (rectangle (* SIZE .9) (/ SIZE 30) "solid"
                                  "orange") (/ SIZE 2) (* (/ SIZE 3) 2) MTS)))))

(define-struct letter (x y image))






(define X-IMAGE (scale (/ SIZE 4) (text "X" 1 "red")))
(define O-IMAGE (scale (/ SIZE 4) (text "O" 1 "blue")))

(define-struct ws (game-state cpu-depth player-turn? game-over?))







(define START (make-ws (list (list 0 0 0)
                             (list 0 0 0)
                             (list 0 0 0))
                       0 true false))

(define TEST1 (make-ws (list (list 0 (make-letter 1 0 O-IMAGE)
                                   (make-letter 2 0 O-IMAGE))
              (list (make-letter 0 1 X-IMAGE)
                    (make-letter 1 1 X-IMAGE) (make-letter 2 1 X-IMAGE))
              (list (make-letter 0 2 X-IMAGE)
                    (make-letter 1 2 O-IMAGE) (make-letter 2 2 O-IMAGE)))
               0 false false))

(define TEST2 (make-ws (list (list (make-letter 0 0 O-IMAGE)
                                   (make-letter 1 0 O-IMAGE)
                                   (make-letter 2 0 X-IMAGE))
              (list (make-letter 0 1 X-IMAGE)
                    (make-letter 1 1 X-IMAGE) (make-letter 2 1 O-IMAGE))
              (list (make-letter 0 2 O-IMAGE)
                    (make-letter 1 2 X-IMAGE) (make-letter 2 2 O-IMAGE)))
              0 false true))

(define TEST3 (make-ws (list (list 0 (make-letter 1 0 O-IMAGE) 0)
              (list (make-letter 0 1 X-IMAGE) 0 0)
              (list (make-letter 0 2 X-IMAGE)
                    (make-letter 1 2 O-IMAGE) (make-letter 2 2 X-IMAGE)))
              1 false false))

(define TEST4 (make-ws (list (list 0 0 (make-letter 2 0 O-IMAGE))
              (list (make-letter 0 1 X-IMAGE) 0 (make-letter 2 1 O-IMAGE))
              (list (make-letter 0 2 X-IMAGE) 0 (make-letter 2 2 O-IMAGE)))
              2 false false))

(define TEST5 (make-ws (list (list (make-letter 0 0 O-IMAGE)
                                   (make-letter 1 0 O-IMAGE)
                                   (make-letter 2 0 X-IMAGE))
              (list (make-letter 0 1 X-IMAGE)
                    (make-letter 1 1 X-IMAGE) (make-letter 2 1 O-IMAGE))
              (list (make-letter 0 2 X-IMAGE) 0 (make-letter 2 2 O-IMAGE)))
              0 true true))

(define CPU-START (make-ws (ws-game-state START)
                           (ws-cpu-depth START) false false))





(define (insert x y list letter)
  (local [ 
          (define (list-processes acc list target)
            (if (= acc 0) (cons target (rest list))
                (cons (first list)
                      (list-processes (sub1 acc) (rest list) target))))]
    (if (empty? list) empty
        (list-processes y list (list-processes x (list-ref list y) letter))))) 
  
(check-expect (insert 0 0 empty
                      (make-letter 0 0 O-IMAGE)) empty) 
(check-expect (insert 1 1 (ws-game-state START) 2)
              (list (list 0 0 0)
                    (list 0 2 0)
                    (list 0 0 0))) 
(check-expect (insert 0 2 (ws-game-state TEST2) (make-letter 0 2 O-IMAGE))
              (list (list (make-letter 0 0 O-IMAGE) (make-letter 1 0 O-IMAGE)
                          (make-letter 2 0 X-IMAGE))
                    (list (make-letter 0 1 X-IMAGE) (make-letter 1 1 X-IMAGE)
                          (make-letter 2 1 O-IMAGE))
                    (list (make-letter 0 2 O-IMAGE) (make-letter 1 2 X-IMAGE)
                          (make-letter 2 2 O-IMAGE)))) 



(define (render-board ws)
  (local [(define (render-end ws scene)
       (local [(define winner (check-over ws))]
         (place-image (text (cond [(= winner 1) "Player Wins!"]
                                  [(= winner 2) "Computer\n    Wins!"]
                                  [(= winner 3) "DRAW"]) (/ SIZE 6) "black")
                                     (/ SIZE 2) (/ SIZE 2) scene)))
          (define (render-letter letter scene)
            (if (not (letter? letter)) scene
                (place-image (letter-image letter)
                             (+(* (/ SIZE 3) (letter-x letter))
                             (/ SIZE 6)) (+(* (/ SIZE 3) (letter-y letter))
                                           (/ SIZE 6)) scene)))]
    (if (ws-game-over? ws) 
        (render-end ws (foldl render-letter BOARD
                              (foldl append empty (ws-game-state ws))))
        (foldl render-letter BOARD (foldl append empty (ws-game-state ws))))))

(check-expect (render-board START) BOARD) 



(define (on-click ws x y mouse)
  (if (string=? mouse "button-down")
      (if (ws-player-turn? ws)
          (local [(define column (floor (/ x (/ SIZE 3))))
                  (define row (floor (/ y (/ SIZE 3))))]
            (if (not (letter? (list-ref (list-ref
                                         (ws-game-state ws) row) column)))
                (make-ws (insert column row (ws-game-state ws)
                                 (make-letter column row X-IMAGE))
                       (ws-cpu-depth ws) false (ws-game-over? ws)) ws)) ws) ws))
 
(check-expect (on-click START (* SIZE .25) (* SIZE .5) "button-down") 
              (make-ws (insert 0 1 (ws-game-state START) 
                               (make-letter 0 1 X-IMAGE)) 0 false false))
(check-expect (on-click TEST5 (* SIZE .5) (* SIZE .75) "button-down") 
              (make-ws (insert 1 2 (ws-game-state TEST5) 
                               (make-letter 1 2 X-IMAGE)) 0 false true))
(check-expect (on-click TEST4 (* SIZE .5) (* SIZE .25) "button-down") TEST4)
      
(check-expect (on-click TEST1 (* SIZE .75) (* SIZE .75) "button-down") TEST1)
      
(check-expect (on-click TEST3 (* SIZE .25) (* SIZE .5) "drag") TEST3)
      
              



(define (cpu-turn ws)
  (local [
          
          
          (define (insert-helper letter ws)
            (if (not (letter? letter)) ws
                (make-ws (insert (letter-x letter) (letter-y letter)
                                 (ws-game-state ws) letter) (ws-cpu-depth ws)
                         (not(ws-player-turn? ws)) (ws-game-over? ws))))
          
          
          (define (is-legal? letter ws)
            (not (letter? (list-ref (list-ref (ws-game-state ws)
                                              (letter-y letter))
                                              (letter-x letter)))))
          
          
          (define (possible-moves ws)
            (local [(define current-player-image
                      (if (ws-player-turn? ws) X-IMAGE O-IMAGE))
                    (define (append-board ws) (flatten (ws-game-state ws)))
                    (define (valid-move-list list index)
                      (cond [(empty? list) empty]
                            [(number? (first list)) (cons (make-letter
                                                           (remainder index 3)
                                                           (floor (/ index 3))
                                                           current-player-image)
                                    (valid-move-list (rest list) (add1 index)))]
                            [else (valid-move-list (rest list) (add1 index))]))]
              (filter (λ (letter) (is-legal? letter ws))
                      (valid-move-list (append-board ws) 0))))
          
          
          (define (random-move ws)
            (local [(define x (random 3))
                    (define y (random 3))]
              (if (is-legal? (make-letter x y O-IMAGE) ws)
                  (make-letter x y O-IMAGE)
                  (random-move ws))))
          
          
          (define (change-turns ws) (make-ws (ws-game-state ws)
                               (ws-cpu-depth ws) (not (ws-player-turn? ws))
                               (ws-game-over? ws)))
          
          
          (define (choose-move lol ws)
            (cond [(empty? lol) (random-move ws)]
                  [(= (check-over (insert-helper (first lol) ws))
                      (if (ws-player-turn? ws) 1 2)) (first lol)]
                  [else (choose-move (rest lol) ws)]))]
            
    (cond [(= (ws-cpu-depth ws) 1)
           (insert-helper (choose-move (possible-moves ws) ws) ws)]
          [(= (ws-cpu-depth ws) 2)
           (if (not (letter? (choose-move (possible-moves (change-turns ws))
                                          (change-turns ws)))) ws
               (local [(define their-move (choose-move
                         (possible-moves (change-turns ws)) (change-turns ws)))]
                 (insert-helper (make-letter (letter-x their-move)
                                           (letter-y their-move) O-IMAGE) ws)))]
          [else (insert-helper (random-move ws) ws)])))

(check-expect (cpu-turn TEST1) 
           (make-ws (insert 0 0 (ws-game-state TEST1) (make-letter 0 0 O-IMAGE))
                       0 true false))
(check-expect (cpu-turn TEST3) 
           (make-ws (insert 1 1 (ws-game-state TEST3) (make-letter 1 1 O-IMAGE))
                       1 true false))
(check-expect (cpu-turn TEST4) 
           (make-ws (insert 0 0 (ws-game-state TEST4) (make-letter 0 0 O-IMAGE))
                       2 true false))




(define (tock ws)
  (cond [(not (= (check-over ws) 0)) (make-ws (ws-game-state ws)
                                              (ws-cpu-depth ws) false true)]
        [(ws-player-turn? ws) ws]
        [else (cpu-turn ws)]))

(check-expect (tock START) START) 
(check-expect (tock TEST4) 
              (make-ws (ws-game-state TEST4) (ws-cpu-depth TEST4) false true))
(check-expect (tock TEST3) (cpu-turn TEST3)) 
(check-random (tock CPU-START) (cpu-turn CPU-START)) 







(define (check-over ws)
  (local [(define (is-win? list)
            (cond [(member 0 list) false]
                  [(andmap (λ (a) (image=? (letter-image a) X-IMAGE)) list) 1]
                  [(andmap (λ (a) (image=? (letter-image a) O-IMAGE)) list) 2]
                  [else false]))]
    (cond [(not (false? (is-win? (list-ref (ws-game-state ws) 0))))
           (is-win? (list-ref (ws-game-state ws) 0))]
          [(not (false? (is-win? (list-ref (ws-game-state ws) 1))))
           (is-win? (list-ref (ws-game-state ws) 1))]
          [(not (false? (is-win? (list-ref (ws-game-state ws) 2))))
           (is-win? (list-ref (ws-game-state ws) 2))]
          [(not (false? (is-win? (list
                                (list-ref (list-ref (ws-game-state ws) 0) 0)
                                (list-ref (list-ref (ws-game-state ws) 1) 1)
                                (list-ref (list-ref (ws-game-state ws) 2) 2)))))
           (is-win? (list
                     (list-ref (list-ref (ws-game-state ws) 0) 0)
                     (list-ref (list-ref (ws-game-state ws) 1) 1)
                     (list-ref (list-ref (ws-game-state ws) 2) 2)))]
          [(not (false? (is-win? (list
                                (list-ref (list-ref (ws-game-state ws) 0) 2)
                                (list-ref (list-ref (ws-game-state ws) 1) 1)
                                (list-ref (list-ref (ws-game-state ws) 2) 0)))))
           (is-win? (list
                     (list-ref (list-ref (ws-game-state ws) 0) 2)
                     (list-ref (list-ref (ws-game-state ws) 1) 1)
                     (list-ref (list-ref (ws-game-state ws) 2) 0)))]
          [(not (false? (is-win? (list
                                (list-ref (list-ref (ws-game-state ws) 0) 0)
                                (list-ref (list-ref (ws-game-state ws) 1) 0)
                                (list-ref (list-ref (ws-game-state ws) 2) 0)))))
           (is-win? (list
                     (list-ref (list-ref (ws-game-state ws) 0) 0)
                     (list-ref (list-ref (ws-game-state ws) 1) 0)
                     (list-ref (list-ref (ws-game-state ws) 2) 0)))]
          [(not (false? (is-win? (list
                                (list-ref (list-ref (ws-game-state ws) 0) 1)
                                (list-ref (list-ref (ws-game-state ws) 1) 1)
                                (list-ref (list-ref (ws-game-state ws) 2) 1)))))
           (is-win? (list
                     (list-ref (list-ref (ws-game-state ws) 0) 1)
                     (list-ref (list-ref (ws-game-state ws) 1) 1)
                     (list-ref (list-ref (ws-game-state ws) 2) 1)))]
          [(not (false? (is-win? (list
                                (list-ref (list-ref (ws-game-state ws) 0) 2)
                                (list-ref (list-ref (ws-game-state ws) 1) 2)
                                (list-ref (list-ref (ws-game-state ws) 2) 2)))))
           (is-win? (list
                     (list-ref (list-ref (ws-game-state ws) 0) 2)
                     (list-ref (list-ref (ws-game-state ws) 1) 2)
                     (list-ref (list-ref (ws-game-state ws) 2) 2)))]
          [(not (ormap (λ (list) (member 0 list)) (ws-game-state ws))) 3]
          [else 0])))

(check-expect (check-over START) 0) 
(check-expect (check-over TEST1) 1) 
(check-expect (check-over TEST2) 3) 
(check-expect (check-over TEST4) 2) 
(check-expect (check-over TEST5) 1) 





(define (depth-change ws key)
  (if (string-numeric? key)
      (make-ws (ws-game-state ws) (string->number key)
               (ws-player-turn? ws) (ws-game-over? ws))
      (if (string=? key " ") START ws)))

(check-expect (depth-change TEST2 "m") TEST2) 
(check-expect (ws-cpu-depth (depth-change START "8")) 8) 
(check-expect (ws-cpu-depth (depth-change TEST3 "0")) 0) 
(check-expect (depth-change TEST1 " ") START) 



(define (main ws)
  (big-bang ws
    (on-tick tock)
    (on-mouse on-click)
    (on-key depth-change)
    (to-draw render-board)))