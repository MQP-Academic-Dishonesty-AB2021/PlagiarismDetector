

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Assignment4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))





(require 2htdp/image)
(require 2htdp/universe)
(require racket/list)




(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))



(define FONT-SIZE (floor (/ SIZE 4))) 
(define MARGIN (/ SIZE 10)) 
(define SQUARE-SIZE (/ (- SIZE (* 2 MARGIN)) 3)) 
(define X "X") 
(define O "O") 
(define X-IMG (text "x" FONT-SIZE "red")) 
(define O-IMG (text "o" FONT-SIZE "blue")) 
(define B "") 
(define BLANK-IMG (square 0 "solid" "white")) 
(define FIRST-POS (/ (+ SIZE (* 4 MARGIN)) 6)) 
(define MID-POS (/ SIZE 2)) 
(define LAST-POS (/ (- (* 5 SIZE) (* 4 MARGIN)) 6)) 
(define COORDINATES (list (make-posn FIRST-POS FIRST-POS) 
                          (make-posn MID-POS FIRST-POS)
                          (make-posn LAST-POS FIRST-POS)
                          (make-posn FIRST-POS MID-POS)
                          (make-posn MID-POS MID-POS)
                          (make-posn LAST-POS MID-POS)
                          (make-posn FIRST-POS LAST-POS)
                          (make-posn MID-POS LAST-POS)
                          (make-posn LAST-POS LAST-POS)))
                            


(define PEN-SIZE (floor (/ SIZE 25))) 
(define PEN-COLOR "orange") 
(define PEN (make-pen PEN-COLOR PEN-SIZE "solid" "round" "bevel")) 


(define FIRST-LINE (/ (+ SIZE MARGIN) 3)) 
(define SECOND-LINE (/ (- (* 2 SIZE) MARGIN) 3)) 
(define EDGE1 MARGIN) 
(define EDGE2 (- SIZE MARGIN)) 


(define BOARD-IMG                                                                    
  (add-line (add-line (add-line (add-line MTS FIRST-LINE EDGE1 FIRST-LINE EDGE2 PEN) 
                                SECOND-LINE EDGE1 SECOND-LINE EDGE2 PEN) 
                      EDGE1 FIRST-LINE EDGE2 FIRST-LINE PEN) 
            EDGE1 SECOND-LINE EDGE2 SECOND-LINE PEN)) 










(define V1 "X") 
(define V2 "O") 
(define V3 B) 







(define B0 (list B B B       
                 B B B
                 B B B ))


(define B1 (list X B B       
                 B B B
                 B B B ))

(define B2 (list X O B       
                 B B B
                 B B B ))

(define B3 (list X O B       
                 B X B
                 B B X ))

(define B4 (list X X B       
                 B X B
                 O O O ))

(define B5 (list X O O       
                 O X X
                 X O O ))

(define B6 (list O B O       
                 B X B
                 X B B))

(define B7 (list B O B       
                 B X B
                 X B B))







(define ROWS (list
              (list 0 1 2)
              (list 3 4 5)
              (list 6 7 8)))


(define COLS (list
              (list 0 3 6)
              (list 1 4 7)
              (list 2 5 8)))


(define DIAGS (list
               (list 0 4 8)
               (list 2 4 6)))


(define UNITS (append ROWS COLS DIAGS))









(define-struct ws (turn board message in-progress? difficulty))













(define START (make-ws 1 B0 "" true 0)) 
(define WS1 (make-ws 2 B1 "" true 2))   
(define WS2 (make-ws 3 B2 "" true 1))   
(define WS3 (make-ws 10 B5 "" true 0))  
(define WS4 (make-ws 5 B3 "" true 2))   
(define WS5 (make-ws 5 B6 "" true 1))   
(define WS6 (make-ws 4 B7 "" true 2))   
(define WS7 (make-ws 7 B4 "" true 1))   





(define (main ws)
  (big-bang ws
    (on-tick tick)
    (to-draw render)
    (on-mouse handle-mouse)
    (on-key handle-key)))









(check-expect (is-win? X (ws-board START)) false) 
(check-expect (is-win? O (ws-board START)) false) 
(check-expect (is-win? X (ws-board WS2)) false) 
(check-expect (is-win? O (ws-board WS2)) false) 
(check-expect (is-win? X (ws-board WS4)) true) 
(check-expect (is-win? O (ws-board WS7)) true) 


(define (is-win? val board)
  (local [(define (check-unit val unit)
            (andmap (λ (pos)
                      (string=? val (list-ref board pos))) unit))]
    (ormap (λ (unit) (check-unit val unit)) UNITS)))









(check-expect (tick WS4) (make-ws (ws-turn WS4) (ws-board WS4) "X WINS" false (ws-difficulty WS4))) 
(check-expect (tick WS7) (make-ws (ws-turn WS7) (ws-board WS7) "O WINS" false (ws-difficulty WS7))) 
(check-expect (tick WS3) (make-ws (ws-turn WS3) (ws-board WS3) "DRAW" false (ws-difficulty WS3))) 
(check-expect (tick WS2) WS2) 


(define (tick ws)
  (local [(define (turn->player ws) 
            (if (even? (ws-turn ws))
                O
                X))
          (define (win val ws) 
            (make-ws (ws-turn ws) (ws-board ws) (string-append val " WINS") false (ws-difficulty ws)))
          (define (is-draw? ws) 
            (not (ormap (λ (v) (string=? B v)) (ws-board ws))))
          (define (draw ws) 
            (make-ws (ws-turn ws) (ws-board ws) "DRAW" false (ws-difficulty ws)))]         
    (cond
      [(not (ws-in-progress? ws)) ws]
      [(is-win? X (ws-board ws)) (win X ws)]
      [(is-win? O (ws-board ws)) (win O ws)]
      [(is-draw? ws) (draw ws)]
      [(string=? (turn->player ws) O) (play-o ws)] 
      [else ws])))











(check-expect (render START) BOARD-IMG) 
(check-expect (render WS5) (local [(define (val->img val) 
                                     (cond
                                       [(string=? X val) X-IMG]
                                       [(string=? O val) O-IMG]
                                       [else
                                        BLANK-IMG]))] (place-images (map val->img (ws-board WS5) ) COORDINATES BOARD-IMG)))
(check-expect (render WS3) (local [(define (val->img val) 
                                     (cond
                                       [(string=? X val) X-IMG]
                                       [(string=? O val) O-IMG]
                                       [else
                                        BLANK-IMG]))] (place-image (text (ws-message WS3) FONT-SIZE "green") MID-POS MID-POS
                                                                   (place-images (map val->img (ws-board WS3) ) COORDINATES BOARD-IMG))))


(define (render ws)
  (local [(define (val->img val) 
            (cond
              [(string=? X val) X-IMG]
              [(string=? O val) O-IMG]
              [else
               BLANK-IMG]))]
    (place-image (text (ws-message ws) FONT-SIZE "green") MID-POS MID-POS
                 (place-images (map val->img (ws-board ws)) COORDINATES BOARD-IMG))))










(check-expect (handle-mouse START FIRST-POS LAST-POS "button-down") (make-ws (add1 (ws-turn START)) (list B B B 
                                                                                                          B B B
                                                                                                          X B B)
                                                                             (ws-message START) (ws-in-progress? START) (ws-difficulty START)))

(check-expect (handle-mouse START (+ 1000 FIRST-POS) LAST-POS "button-down") START) 

(check-expect (handle-mouse WS1 FIRST-POS FIRST-POS "button-down") WS1) 

(check-expect (handle-mouse WS1 FIRST-POS LAST-POS "drag") WS1) 



(define (handle-mouse ws x y mouse-event)
  (local [(define (r-c->pos r c) 
            (+ (* r 3) c))
          
          (define (move-valid? pos) 
            (and x-y-valid? 
                 (odd? (ws-turn ws)) 
                 (string=? B (list-ref (ws-board ws) pos)) 
                 (not (false? (ws-in-progress? ws))))) 
          
          (define x-y-valid? 
            (and (>= x EDGE1)
                 (<= x EDGE2)
                 (>= y EDGE1)
                 (<= y EDGE2)))
          
          (define pos              
            (r-c->pos (floor (/ (- y MARGIN) SQUARE-SIZE))
                      (floor (/ (- x MARGIN) SQUARE-SIZE))))]
    
    (cond [(mouse=? mouse-event "button-down")
           (if (move-valid? pos)
               (place-val pos X ws) 
               ws)]
          [else ws])))









(check-expect (handle-key START "0") START) 
(check-expect (handle-key START "1") (make-ws (ws-turn START) (ws-board START) (ws-message START) (ws-in-progress? START) 1)) 
(check-expect (handle-key START "2") (make-ws (ws-turn START) (ws-board START) (ws-message START) (ws-in-progress? START) 2)) 
(check-expect (handle-key START "a") START) 


(define (handle-key ws key-event)
  (cond 
    [(key=? key-event "0") (make-ws (ws-turn ws) (ws-board ws) (ws-message ws) (ws-in-progress? ws) 0)]
    [(key=? key-event "1") (make-ws (ws-turn ws) (ws-board ws) (ws-message ws) (ws-in-progress? ws) 1)]
    [(key=? key-event "2") (make-ws (ws-turn ws) (ws-board ws) (ws-message ws) (ws-in-progress? ws) 2)]
    [else ws] 
    ))








(check-expect (place-val 3 X START)  
              (make-ws (add1 (ws-turn START)) (list B B B X B B B B B) (ws-message START) (ws-in-progress? START) (ws-difficulty START)))

(check-expect (place-val 7 O WS2)    
              (make-ws (add1 (ws-turn WS2)) (list X O B B B B B O B) (ws-message WS2) (ws-in-progress? WS2) (ws-difficulty WS2)))



(define (place-val pos val ws)
  (local [
          
          
          (define (update-board board ind)
            (cond
              [(empty? board) empty]
              [else
               (if (= ind pos)
                   (cons val
                         (update-board (rest board) (add1 ind))) 
                   (cons (first board)
                         (update-board (rest board) (add1 ind))))]))] 
              
    (make-ws (add1 (ws-turn ws)) 
             (update-board (ws-board ws) 0) 
             (ws-message ws) (ws-in-progress? ws) (ws-difficulty ws)))) 























(check-expect (play-o WS5) (make-ws 6 (list O O O
                                            B X B
                                            X B B) "" true 1)) 

(check-expect (play-o WS6) (make-ws 5 (list B O O 
                                            B X B
                                            X B B) "" true 2)) 



                                      

(define (play-o ws)
  (local [(define (next-boards val) 
            
            
            (local [(define (next-boards val pos) 
                      (cond
                        [(> pos 8) empty]
                        [(string=? B (list-ref (ws-board ws) pos)) 
                         (cons (ws-board (place-val pos val ws))
                               (next-boards val (add1 pos)))] 
                        [else
                         (next-boards val (add1 pos))]))]
              (next-boards val 0))) 
          
          (define (play-random-o pos)             
            (local [(define (pos-valid? pos)
                      (string=? B (list-ref (ws-board ws) pos)))] 
              (if (pos-valid? pos)
                  (place-val pos O ws) 
                  ws))) 

          
          (define (play-o--bd depth) 
            (cond
              [(= (ws-difficulty ws) depth) (play-random-o (random 9))] 
              [(even? depth) (play-o--lobd (next-boards O) O depth)] 
              [else
               (play-o--lobd (next-boards X) X depth)])) 

          (define (play-o--lobd lobd val depth)
            (cond
              [(empty? lobd) (play-o--bd (add1 depth))] 
              [(is-win? val (first lobd)) (replace-val (first lobd) O)] 
              [else
               (play-o--lobd (rest lobd) val depth)]))

          
          
          
          (define (replace-val board val)
            
            
            (local [(define (replace-val board val pos)
                      (cond
                        [(not (string=? (list-ref board pos)
                                        (list-ref (ws-board ws) pos))) 
                         (place-val pos val ws)] 
                        [else
                         (replace-val board val (add1 pos))]))] 
              (replace-val board val 0)))] 
    
    (play-o--bd 0))) 
   

