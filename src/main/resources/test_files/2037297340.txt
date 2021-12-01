

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |part 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 





(define SIZE 300) 
(define MTS (empty-scene SIZE SIZE))
(define TEXT-SIZE (round (/ SIZE 4)))
(define OFFSET (/ SIZE 10))

(define rec (rectangle 5 (- SIZE OFFSET) "solid" "orange"))

(define empty-board (overlay
                     (overlay/offset rec
                                     (/ SIZE 3) 0
                                     rec)
                     (overlay/offset (rotate 90 rec)
                                     0 (/ SIZE 3)
                                     (rotate 90 rec))))

(define-struct board (values player-turn?))

(define START (make-board (list false false false
                                false false false
                                false false false) true))




(define x-img (text " X " TEXT-SIZE "blue"))
(define o-img (text " O " TEXT-SIZE "red"))



(define B1 (make-board (list                             
                        false false false
                        false false false
                        false false false) true))

(define B2 (make-board (list                             
                        x-img x-img o-img
                        x-img o-img x-img
                        o-img o-img x-img) false))

(define B3 (make-board (list                             
                        o-img false x-img
                        false o-img o-img
                        x-img false x-img) true))

(define B4 (make-board (list                             
                        x-img x-img x-img
                        o-img o-img x-img
                        o-img x-img o-img) false))

(define B5 (make-board (list                             
                        o-img x-img x-img
                        false o-img x-img
                        o-img false o-img) false))

(define B6 (make-board (list                             
                        o-img x-img o-img
                        x-img o-img x-img
                        x-img o-img x-img) true))


























(define (main START)
  (big-bang START
    (on-tick update)
    (to-draw render)
    (on-mouse handle-mouse)))









(define (get-value board spot)
  (list-ref (board-values board) spot))

(check-expect (get-value B1 3) false)
(check-expect (get-value B2 5) x-img)
(check-expect (get-value B3 7) false)
(check-expect (get-value B4 8) o-img)
(check-expect (get-value B5 1) x-img)








(define (render board)
  
  (local [
          
          
          (define (get-image b n)                                                                                                  
            (if (false? (get-value b n))
                empty-image                                                                                                       
                (get-value b n)))

          
          (define location-list (list 
                                 (make-posn (/ SIZE 5.5) (/ SIZE 5.5))                    
                                 (make-posn (/ SIZE 2) (/ SIZE 5.5))                      
                                 (make-posn (- SIZE (/ SIZE 5.5)) (/ SIZE 5.5))           
                                 (make-posn (/ SIZE 5.5) (/ SIZE 2))                      
                                 (make-posn (/ SIZE 2) (/ SIZE 2))                        
                                 (make-posn (- SIZE (/ SIZE 5.5)) (/ SIZE 2))             
                                 (make-posn (/ SIZE 5.5) (- SIZE (/ SIZE 5.5)))           
                                 (make-posn (/ SIZE 2) (- SIZE (/ SIZE 5.5)))             
                                 (make-posn (- SIZE (/ SIZE 5.5)) (- SIZE (/ SIZE 5.5))))) 

          
          (define images-list (list   
                               (get-image board 0) (get-image board 1) (get-image board 2)
                               (get-image board 3) (get-image board 4) (get-image board 5)
                               (get-image board 6) (get-image board 7) (get-image board 8)))

          
          (define normal-board (place-images images-list location-list (place-image empty-board (/ SIZE 2) (/ SIZE 2) MTS))) 

          
          (define (draw-winning-board txt clr)                                                                              
            (overlay/align "middle" "middle"
                           (text txt TEXT-SIZE clr)
                           (place-images images-list location-list (place-image empty-board (/ SIZE 2) (/ SIZE 2) MTS))))]
    
    (if (string=? (get-winner board) "")
        normal-board
        (draw-winning-board (get-winner board) "green"))))









(define (get-winner board)
            
  
  (local [(define (check-win a b c)
            (cond [(false? (get-value board a)) false]
                  [(and (equal? (get-value board a) (get-value board b))
                        (equal? (get-value board b) (get-value board c))) true]
                  [else false]))
                            
                    
          
          
          (define (check-winner i)                                                                                    
            (local [(define winner (list-ref (board-values board) i))]                                                
              (if (equal? winner x-img)
                  "X-WINS"
                  "O-WINS")))

          
          
          
          (define (empty-squares? b)                                                                                  
            (cond [(empty? b) false]                                                                                    
                  [(false? (first b)) true]                                                                             
                  [else                                                                                             
                   (empty-squares? (rest b))]))]

    
    (cond                                                                                                              
      [(check-win 0 1 2) (check-winner 0)] 
      [(check-win 3 4 5) (check-winner 3)] 
      [(check-win 6 7 8) (check-winner 6)] 
      [(check-win 0 4 8) (check-winner 0)] 
      [(check-win 2 4 6) (check-winner 2)] 
      [(check-win 0 3 6) (check-winner 0)] 
      [(check-win 1 4 7) (check-winner 1)] 
      [(check-win 2 5 8) (check-winner 2)] 
      [else                                                                                                          
       (if (empty-squares? (board-values board))                                                                    
           ""
           "DRAW")])))

(check-expect (get-winner B1) "")
(check-expect (get-winner B2) "O-WINS")
(check-expect (get-winner B3) "")
(check-expect (get-winner B4) "X-WINS")
(check-expect (get-winner B6) "DRAW")







(define (game-over? board)
  (not (string=? (get-winner board) "")))

(check-expect (game-over? B1) #f) 
(check-expect (game-over? B2) #t) 
(check-expect (game-over? B6) #t) 









(define (update board)
  (local [(define (computer-move board)
            (local
              [(define (get-open-indexes list i)
                 (cond [(empty? list) empty]
                       [(false? (first list))
                        (cons i (get-open-indexes (rest list) (add1 i)))]
                       [else
                        (get-open-indexes (rest list) (add1 i))]))
               (define open-indexes
                 (get-open-indexes (board-values board) 0))
               (define spot
                 (list-ref open-indexes (random (length open-indexes))))]
              (fill-spot board spot false)))]
  (if (and
       (not (board-player-turn? board))
       (not (game-over? board)))
      (computer-move board)
      board)))









(define (handle-mouse board x y event)
  (if (string=? event "button-down")
      (local
        [(define spot (get-spot-xy x y))]
        (if (and
             (not (false? spot))
             (board-player-turn? board)
             (not (game-over? board)))
            (local [(define fill (fill-spot board spot true))]
              (if (not (false? fill))
                  fill
                  board))
            board))
      board))
























(define (get-spot-xy x y)
  (local
    [(define WIDTH (- SIZE OFFSET))
     (define LEFT-EDGE (/ OFFSET 2))

     (define (get-section pos)       
       (cond [(and (> pos LEFT-EDGE) (< pos (+ LEFT-EDGE (/ WIDTH 3)))) 0]
             [(and (> pos (+ LEFT-EDGE (/ WIDTH 3))) (< pos (+ LEFT-EDGE (* (/ WIDTH 3) 2)))) 1]
             [(and (> pos (+ LEFT-EDGE (* (/ WIDTH 3) 2))) (< pos (+ LEFT-EDGE WIDTH))) 2]
             [else false]))

     (define c (get-section x))
     (define r (get-section y))]

    (if (or (false? r) (false? c))
        false
        (get-spot-rc r c))))



(check-expect (get-spot-xy 230 215) 8)
(check-expect (get-spot-xy 25  50)  0)
(check-expect (get-spot-xy 150 60)  1)
(check-expect (get-spot-xy 230 160) 5)
(check-expect (get-spot-xy 35 210) 6)
(check-expect (get-spot-xy 5 10) false)
(check-expect (get-spot-xy 500 500) false)










(define (get-spot-rc row col)
  (+ (* 3 row) col))

(check-expect (get-spot-rc 2 2) 8)
(check-expect (get-spot-rc 0 0) 0)
(check-expect (get-spot-rc 0 1) 1)
(check-expect (get-spot-rc 1 2) 5)
(check-expect (get-spot-rc 2 0) 6)


 







(define (fill-spot board spot player?)
  (if (not (false? (get-value board spot)))
      false
      (make-board
       (list-set
        (board-values board)
        spot
        (if player?
            x-img
            o-img))
       (not player?))))



(check-expect (fill-spot B1 4 true)
              (make-board (list                            
                           false false false
                           false x-img false
                           false false false) false))

(check-expect (fill-spot B1 2 false)
              (make-board (list                            
                           false false o-img
                           false false false
                           false false false) true))

(check-expect (fill-spot B2 7 true)
              false)

(check-expect (fill-spot B2 7 false)
              false)