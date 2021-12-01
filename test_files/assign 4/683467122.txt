

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment4ColeSarah) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))






(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 




(define SIZE 600) 
(define MTS (empty-scene SIZE SIZE))

(define BAR (rectangle
             (* SIZE 1/50)
             (- SIZE (* 1/10 SIZE))
             "solid"
             "royal blue")) 
(define CRISSCROSS2 (place-image
                     BAR
                     (* SIZE 1/3)
                     (* SIZE 1/2)
                     (place-image BAR (* SIZE 2/3) (* SIZE 1/2) MTS)))
(define CRISSCROSS (place-image
                    (rotate 90 BAR)
                    (* SIZE 1/2)
                    (* SIZE 1/3)
                    (place-image
                     (rotate 90 BAR)
                     (* SIZE 1/2)
                     (* SIZE 2/3)
                     CRISSCROSS2))) 
(define EMOJI1 (scale (* SIZE (/ 1 300)) .))   
(define EMOJI2 (scale (* SIZE (/ 1 300)) .))   


(define-struct TICTACTOE (LON move win smart))










(define EMPTYBD                                   
  (make-TICTACTOE (list 0 0 0 0 0 0 0 0 0) 0 0 0))

(define EMPTYBD1                                   
  (make-TICTACTOE (list 0 0 0 0 0 0 0 0 0) 0 1 0))

(define EMPTYBD2                                   
  (make-TICTACTOE (list 0 0 0 0 0 0 0 0 0) 0 2 0))

(define EMPTYBD3                                   
  (make-TICTACTOE (list 0 0 0 0 0 0 0 0 0) 0 3 0))

(define EMPTYBD4                                   
  (make-TICTACTOE (list 0 0 0 0 0 0 0 0 0) 1 0 0))

(define WINBD1                                    
  (make-TICTACTOE (list 1 1 1 0 0 0 0 0 0) 0 0 0))

(define WINBD12                                    
  (make-TICTACTOE (list 2 2 2 0 0 0 0 0 0) 0 0 0))

(define WINBD2                                    
  (make-TICTACTOE (list 0 0 0 1 1 1 0 0 0) 0 0 0))

(define WINBD3                                    
  (make-TICTACTOE (list 0 0 0 0 0 0 1 1 1) 0 0 0))

(define WINBD4                                    
  (make-TICTACTOE (list 1 0 0 1 0 0 1 0 0) 0 0 0))

(define WINBD5                                    
  (make-TICTACTOE (list 0 1 0 0 1 0 0 1 0) 0 0 0))

(define WINBD6                                    
  (make-TICTACTOE (list 0 0 1 0 0 1 0 0 1) 0 0 0))

(define WINBD7                                    
  (make-TICTACTOE (list 1 0 0 0 1 0 0 0 1) 0 0 0))

(define WINBD8                                    
  (make-TICTACTOE (list 0 0 1 0 1 0 1 0 0) 0 0 0))

(define TIE                                       
  (make-TICTACTOE (list 2 1 2 2 1 2 1 2 1) 0 0 0))

(define Quad1 (* 1/6 SIZE))
(define Quad2 (* 3/6 SIZE))
(define Quad3 (* 5/6 SIZE))
(define COLOR "Medium Green")






(check-expect (draw EMPTYBD) CRISSCROSS)                          
(check-expect (draw EMPTYBD1) (overlay
                               (text "YOU WON!" (* 1/6 SIZE) "Medium Green")
                               CRISSCROSS))   
(check-expect (draw EMPTYBD2) (overlay
                               (text "YOU LOST!" (* 1/6 SIZE) "Medium Green")
                               CRISSCROSS))  
(check-expect (draw EMPTYBD3) (overlay
                               (text "TIE!" (* 1/6 SIZE) "Medium Green")
                               CRISSCROSS))       





        
 

 

 

 
         
(define (draw TTT)                                   
  (local
    [
     (define (drawer lon)                                          
       (local
         [(define (helper position)       
            (local                        
              [(define x-coordinate (modulo position 3))
               (define y-coordinate (quotient position 3))
               (define xposition (+ 1/6 (* x-coordinate 2/6)))
               (define yposition (+ 1/6 (* y-coordinate 2/6)))                  
               (define (pickimage x)
                 (cond [(= x 1) EMOJI1]
                       [(= x 2) EMOJI2]
                       [else empty-image]))]
              (cond
                [(= position 9) CRISSCROSS]
                [else (place-image (list-ref (map pickimage lon)
                                             position)
                                   (* xposition SIZE)
                                   (* yposition SIZE)
                                   (helper (add1 position)))]))

            )]
         (helper 0)))]
    (cond
      [ (= (TICTACTOE-win TTT) 1)
        (overlay
         (text "YOU WON!" (* 1/6 SIZE) "Medium Green")
         (drawer (TICTACTOE-LON TTT)))]                      
      [ (= (TICTACTOE-win TTT) 2)
        (overlay
         (text "YOU LOST!" (* 1/6 SIZE) "Medium Green")
         (drawer (TICTACTOE-LON TTT)))]                     
      [ (= (TICTACTOE-win TTT) 3)
        (overlay
         (text "TIE!" (* 1/6 SIZE) "Medium Green")
         (drawer (TICTACTOE-LON TTT)))]                              
      [else (drawer (TICTACTOE-LON TTT))])))                               






(check-expect (determine-box Quad1 Quad3) 6)                 
(check-expect (determine-box Quad2 Quad1) 1)                 
(check-expect (determine-box Quad3 Quad3) 8)                 
(check-expect (determine-box (* 1/3 SIZE) Quad3) -1)                
(check-expect (determine-box Quad3 (* 1/3 SIZE)) -1)                
(check-expect (determine-box (* 2 SIZE) (* 2 SIZE)) -1)                    

(define (determine-box x y)
  (cond
    [(= (determine-col x) -1) -1]
    [(= (determine-col y) -1) -1]
    [else (+ (* (determine-col y) 3) (determine-col x))]))
    




(check-expect (determine-col Quad1) 0)               
(check-expect (determine-col Quad2) 1)               
(check-expect (determine-col Quad3) 2)               
(check-expect (determine-col (* 1/3 SIZE)) -1)              
                                                            
(check-expect (determine-col (* 2 SIZE)) -1)                
(define (determine-col x)
  (cond
    [(and (> x 0)                                     
          (< x (* 1/3 SIZE))) 0]
    [(and (> x (* 1/3 SIZE))                          
          (< x (* 2/3 SIZE))) 1]
    [(and (> x (* 2/3 SIZE))                          
          (< x (* 3/3 SIZE))) 2]
    [else -1]))









(check-expect (is-filled? empty 5) false)                                 
(check-expect (is-filled? (TICTACTOE-LON EMPTYBD) 5) false)               
(check-expect (is-filled? (TICTACTOE-LON WINBD3) 8) true)                 
                                                                          
(check-expect (is-filled? (TICTACTOE-LON WINBD12) 2) true)                
                                                                          
(check-expect (is-filled? (TICTACTOE-LON WINBD2) 1) false)                

(define (is-filled? LON box-number)
  (cond [(empty? LON) false]
        [(= (list-ref LON box-number) 0) false]
        [else true]))





(check-expect (valid-move EMPTYBD1 Quad3 Quad3) true)               
(check-expect (valid-move WINBD4 Quad2 Quad3) true)                 
(check-expect (valid-move WINBD1 Quad1 Quad1) false)                
(check-expect (valid-move WINBD2 Quad2 Quad2) false)                
(check-expect (valid-move empty 0 0) false)                                       

(define (valid-move TTT mouse-x mouse-y)
  (cond
    [(>= (determine-box mouse-x mouse-y) 0)
     (if (not (is-filled? (TICTACTOE-LON TTT) (determine-box mouse-x mouse-y)))
         true
         false)]
    [else
     false]
    ))




(check-expect (next-player 1) 0)
(check-expect (next-player 0) 1)

(define (next-player player)
  (if (>= 0 player)   
      1
      0))

  




(check-expect (edit-board (list 0 1 2) 1 3) (list 0 3 2))           
(check-expect (edit-board (list 0) 0 1) (list 1))                   

(define (edit-board LON box-number player)
  (append
   (take LON box-number)
   (list player)
   (drop LON (+ box-number 1))))



(check-expect (TTTMod EMPTYBD 1)
              (make-TICTACTOE
               (TICTACTOE-LON EMPTYBD)
               (TICTACTOE-move EMPTYBD)
               1
               (TICTACTOE-smart EMPTYBD)))  

(define (TTTMod TTT num)
  (make-TICTACTOE
   (TICTACTOE-LON TTT)
   (TICTACTOE-move TTT)
   num
   (TICTACTOE-smart TTT)))                                                      




(check-expect (lin EMPTYBD) (TICTACTOE-LON EMPTYBD))

(define (lin TTT)
  (TICTACTOE-LON TTT))





(check-expect (check-win WINBD1) (TTTMod WINBD1 1))   
(check-expect (check-win WINBD12) (TTTMod WINBD12 2)) 
(check-expect (check-win EMPTYBD) EMPTYBD)            
(check-expect (check-win WINBD7) (TTTMod WINBD7 1))   

(define (check-win TTT)
  (local
    [
     (define (helper col nu num)
       (append (take (drop (lin TTT) col) 1)
               (take (drop (lin TTT) (+ col nu)) 1)
               (take (drop (lin TTT) (+ col num)) 1)))
          
     (define (win-horizontal TTT row)              
       (cond [(empty? (lin TTT)) TTT]              
                                                   
             [(= row 9) TTT]     
             [(equal? (list 1 1 1) (drop (take (lin TTT) (+ 3 row)) row))
              (TTTMod TTT 1)]
             [(equal? (list 2 2 2) (drop (take (lin TTT) (+ 3 row)) row))
              (TTTMod TTT 2)]
             [else
              (win-horizontal TTT (+ 3 row))]))
     (define (win-vertical TTT col)               
       (cond [(empty? (lin TTT)) TTT]             
             [(= col 3) TTT]    
             [(equal? (list 1 1 1)
                      (helper col 3 6)) 
              (TTTMod TTT 1)]
             [(equal? (list 2 2 2)
                      (helper col 3 6))           
              (TTTMod TTT 2)]
             [else
              (win-vertical TTT (+ 1 col))]))
     (define (helper2 TTT n nu num)
       (and (=  (list-ref (lin TTT) n)
                (list-ref (lin TTT) nu)
                (list-ref (lin TTT) num))
            (not (= (list-ref (lin TTT) n) 0))))                                
     (define (win-diagonal TTT vert-start)       
       (cond                                     
         [(helper2 TTT 0 4 8)
          (if (= (list-ref (lin TTT) 0) 1)
              (TTTMod TTT 1)
              (TTTMod TTT 2))]
         [(helper2 TTT 2 4 6)
          (if (= (list-ref (lin TTT) 2) 1)
              (TTTMod TTT 1)
              (TTTMod TTT 2))]
         [else TTT]
         ))
     (define (all3? TTT numb) (not(= (TICTACTOE-win
                                      (win-horizontal
                                       (win-diagonal
                                        (win-vertical TTT numb)
                                        numb) numb)) numb)))
     (define (all3 TTT numb) (win-horizontal
                              (win-diagonal
                               (win-vertical TTT numb) numb) numb))
     ]
    (cond [(all3? TTT 0)
           (all3 TTT 0)]
          [(empty? (filter (λ (c) (= c 0)) (TICTACTOE-LON TTT)))
           (make-TICTACTOE (TICTACTOE-LON TTT)
                           (TICTACTOE-move TTT)
                           3
                           (TICTACTOE-smart TTT))]
          [else
           (make-TICTACTOE (TICTACTOE-LON TTT)
                           (TICTACTOE-move TTT)
                           0
                           (TICTACTOE-smart TTT))])))







(check-expect (fill EMPTYBD Quad1 Quad1 "button-down")                               
              (make-TICTACTOE
               (edit-board
                (TICTACTOE-LON EMPTYBD) (determine-box Quad1 Quad1) 1)
               (next-player (TICTACTOE-move EMPTYBD))
               (TICTACTOE-win EMPTYBD)
               (TICTACTOE-smart EMPTYBD)))
(check-expect (fill WINBD12 Quad3 Quad3 "button-down")                               
              (make-TICTACTOE
               (edit-board
                (TICTACTOE-LON WINBD12)
                (determine-box Quad3 Quad3)
                (TICTACTOE-move WINBD12))
               0
               2 (TICTACTOE-smart WINBD12)))
(check-expect (fill WINBD2 Quad2 Quad1 "button-down")
              (make-TICTACTOE                              
               (TICTACTOE-LON WINBD2)
               0
               1
               (TICTACTOE-smart WINBD2)))
(check-expect (fill TIE 0 0 "button-down") (TTTMod TIE 3))                               


(define (fill TTT x y m)
  (cond
    [(not (= (TICTACTOE-win (check-win TTT)) 0)) (check-win TTT)]
    [(and (valid-move TTT x y) (= (TICTACTOE-move TTT) 0))
     (if (equal? m "button-down")
         (make-TICTACTOE
          (edit-board (TICTACTOE-LON TTT) (determine-box x y) 1)
          (next-player (TICTACTOE-move TTT))
          (TICTACTOE-win TTT)
          (TICTACTOE-smart TTT))
         TTT)]
    [(= (TICTACTOE-move TTT) 0)
     TTT]
    [(= (TICTACTOE-smart TTT) 1) (smart1 TTT)]
    [(= (TICTACTOE-smart TTT) 2) (smart2 TTT)]
    [else (cbb TTT (random SIZE) (random SIZE))]))








(define CompBD1                                    
  (make-TICTACTOE (list 2 2 0 0 0 0 0 0 0) 1 0 1))

(define CompBD2                                    
  (make-TICTACTOE (list 2 0 0 2 0 0 0 0 0) 1 0 1))

(define CompBD3 
  (make-TICTACTOE (list 2 0 0 0 2 0 0 0 0) 1 0 1)) 

(check-expect (smart1 CompBD1) (make-TICTACTOE (list 2 2 2 0 0 0 0 0 0) 0 0 1))
(check-expect (smart1 CompBD2) (make-TICTACTOE (list 2 0 0 2 0 0 2 0 0) 0 0 1))
(check-expect (smart1 CompBD3) (make-TICTACTOE (list 2 0 0 0 2 0 0 0 2) 0 0 1))

(define (smart1 TTT)
  (local
    [
     (define (recursive counter)
       (cond
         [(= counter 9) (cbb TTT (random SIZE) (random SIZE))]
         [else (if (= (list-ref (lin TTT) counter) 0)
                   (if (not (= (TICTACTOE-win
                                (check-win
                                 (make-TICTACTOE
                                  (edit-board (lin TTT) counter 2)
                                  (next-player (TICTACTOE-move TTT))            
                                  (TICTACTOE-win TTT)
                                  (TICTACTOE-smart TTT)))) 0)) 
                       (make-TICTACTOE (edit-board (lin TTT) counter 2)
                                       (next-player (TICTACTOE-move TTT))
                                       (TICTACTOE-win TTT)
                                       (TICTACTOE-smart TTT))
                       (recursive (+ 1 counter)))
                   (recursive (+ 1 counter)))]))]
               
    (recursive 0)))

(define CompBD4                                    
  (make-TICTACTOE (list 1 1 0 0 0 0 0 0 0) 1 0 2))

(define CompBD5                                    
  (make-TICTACTOE (list 1 0 0 1 0 0 0 0 0) 1 0 2))

(define CompBD6 
  (make-TICTACTOE (list 1 0 0 0 1 0 0 0 0) 1 0 2)) 

(define CompBD7                                    
  (make-TICTACTOE (list 2 2 0 0 0 0 0 0 0) 1 0 2))

(define CompBD8                                    
  (make-TICTACTOE (list 2 0 0 2 0 0 0 0 0) 1 0 2))

(define CompBD9 
  (make-TICTACTOE (list 2 0 0 0 2 0 0 0 0) 1 0 2)) 

(define CompBD10 
  (make-TICTACTOE (list 1 1 0 2 2 0 0 0 0) 1 0 2))

(check-expect (smart2 CompBD7) (make-TICTACTOE (list 2 2 2 0 0 0 0 0 0) 0 0 2)) 
                                                                                
(check-expect (smart2 CompBD8) (make-TICTACTOE (list 2 0 0 2 0 0 2 0 0) 0 0 2)) 
                                                                                
(check-expect (smart2 CompBD9) (make-TICTACTOE (list 2 0 0 0 2 0 0 0 2) 0 0 2)) 
                                                                                
(check-expect (smart2 CompBD10) (make-TICTACTOE (list 1 1 0 2 2 2 0 0 0) 0 0 2)) 
                                                                                 
(check-expect (smart2 CompBD4) (make-TICTACTOE (list 1 1 2 0 0 0 0 0 0) 0 0 2))  
                                                                                 
(check-expect (smart2 CompBD5) (make-TICTACTOE (list 1 0 0 1 0 0 2 0 0) 0 0 2))  
                                                                                 
(check-expect (smart2 CompBD6) (make-TICTACTOE (list 1 0 0 0 1 0 0 0 2) 0 0 2))  
                                                                                 

(define (smart2 TTT)
  (local [(define (recursive counter)
            (cond
              [(= counter 18) (cbb TTT (random SIZE) (random SIZE))]
              [(< counter 9) (if (= (list-ref (lin TTT) counter) 0)
                                 (if  (not
                                       (=
                                        (TICTACTOE-win
                                         (check-win
                                          (make-TICTACTOE
                                           (edit-board (lin TTT) counter 2)
                                           (next-player (TICTACTOE-move TTT))
                                           (TICTACTOE-win TTT)
                                           (TICTACTOE-smart TTT)))) 0))
                                      (make-TICTACTOE
                                       (edit-board (lin TTT) counter 2)
                                       (next-player (TICTACTOE-move TTT))
                                       (TICTACTOE-win TTT)
                                       (TICTACTOE-smart TTT))
                                      (recursive (+ 1 counter)))
                                 (recursive (+ 1 counter)))]
              [else (if (= (list-ref (lin TTT) (- counter 9)) 0)
                        (if
                         (not
                          (=
                           (TICTACTOE-win
                            (check-win (make-TICTACTOE
                                        (edit-board (lin TTT) (- counter 9) 1)  
                                        (next-player (TICTACTOE-move TTT))
                                        (TICTACTOE-win TTT)
                                        (TICTACTOE-smart TTT)))) 0))
                         (make-TICTACTOE (edit-board (lin TTT) (- counter 9) 2)
                                         (next-player (TICTACTOE-move TTT))
                                         (TICTACTOE-win TTT)
                                         (TICTACTOE-smart TTT)) 
                             
                             
                         (recursive (+ 1 counter)))
                        (recursive (+ 1 counter)))]))]
               
    (recursive 0)))





(define (cbb TTT x y)
  (cond
    [(valid-move TTT x y)
     (make-TICTACTOE
      (edit-board (TICTACTOE-LON TTT) (determine-box x y) 2)
      (next-player (TICTACTOE-move TTT))
      (TICTACTOE-win TTT)
      (TICTACTOE-smart TTT))]
    [else (cbb TTT (random SIZE) (random SIZE))]))




(check-expect (key EMPTYBD "2") (make-TICTACTOE (TICTACTOE-LON EMPTYBD)         
                                                (TICTACTOE-move EMPTYBD)
                                                (TICTACTOE-win EMPTYBD) 2))   
(check-expect (key EMPTYBD "1") (make-TICTACTOE (TICTACTOE-LON EMPTYBD)
                                                (TICTACTOE-move EMPTYBD)
                                                (TICTACTOE-win EMPTYBD) 1))   
(check-expect (key EMPTYBD "0") (make-TICTACTOE (TICTACTOE-LON EMPTYBD)
                                                (TICTACTOE-move EMPTYBD)
                                                (TICTACTOE-win EMPTYBD) 0))   
(check-expect (key EMPTYBD "a") EMPTYBD) 

(define (key TTT a-key)
  (cond
    [(key=? a-key "1") (make-TICTACTOE (TICTACTOE-LON TTT)
                                       (TICTACTOE-move TTT)
                                       (TICTACTOE-win TTT) 1)]
    [(key=? a-key "2") (make-TICTACTOE (TICTACTOE-LON TTT)
                                       (TICTACTOE-move TTT)
                                       (TICTACTOE-win TTT) 2)]
    [(key=? a-key "0") (make-TICTACTOE (TICTACTOE-LON TTT)
                                       (TICTACTOE-move TTT)
                                       (TICTACTOE-win TTT) 0)]
    [else (make-TICTACTOE (TICTACTOE-LON TTT)
                          (TICTACTOE-move TTT)
                          (TICTACTOE-win TTT)
                          (TICTACTOE-smart TTT))]))

(define START EMPTYBD)


 

(define (main TTT)
  (big-bang TTT
    (on-release key)
    (to-draw draw)
    (on-mouse fill)
    ))