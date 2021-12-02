

#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |traffic signal Final Stryder Crouse & Jack Weinstein|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 800)
(define HEIGHT 600)
(define X-POS (/ WIDTH 2)) 
(define Y-POS (/ HEIGHT 4)) 
(define MTS (empty-scene WIDTH HEIGHT))

(define TICKS-SECOND 28) 




(define LIGHT-RADIUS 40) 
(define GREEN-LENGTH 5) 
(define YELLOW-LENGTH 2) 
(define RED-LENGTH 4) 





(define (ticks->seconds ticks)
  (floor (/ ticks TICKS-SECOND)))

(check-expect (ticks->seconds 0) 0)
(check-expect (ticks->seconds 28) 1)
(check-expect (ticks->seconds 280) 10)
(check-expect (ticks->seconds 279) 9)
(check-expect (ticks->seconds 281) 10)



(define AUTO-IMAGE1 . )
(define AUTO-IMAGE2 .)
(define AUTO-IMAGE3 .)





(define (pick-image val)
  (cond
    [(= val 0) AUTO-IMAGE1]
    [(= val 1) AUTO-IMAGE2]
    [else
     AUTO-IMAGE3]))
(check-expect (pick-image 0) AUTO-IMAGE1)
(check-expect (pick-image 1) AUTO-IMAGE2)
(check-expect (pick-image 2) AUTO-IMAGE3)








 


[define-struct WS [time ListOfAuto billBoards]]






 

[define START [make-WS 0 empty (list "")]]




(define (main ws)
  (big-bang ws                   
    (on-tick   tock)     
    (to-draw   render) 
    (on-key handle-key) 
    ))







(define (tock ws)
  [make-WS [+ 1 [WS-time ws]]
           [append [genAuto [random 20] ws]
            [updateAutos [WS-ListOfAuto ws] ws]]
           (WS-billBoards ws)])





(define (render ws)
  [place-images
   [append [list [whatLight? ws]
                 (text (number->string [ticks->seconds [WS-time ws]])
                       20 "black")
                 (text (checkBillboard (WS-billBoards ws)) 20 "blue")
                 ]
           [returnAutoImages [WS-ListOfAuto ws]]]
   
   [append [list [make-posn X-POS Y-POS]
                 [make-posn [+ X-POS 70] Y-POS]
                 [make-posn [- X-POS 150] Y-POS]
                 ]
           [returnAutoPos [WS-ListOfAuto ws]]] MTS])




(check-expect (checkBillboard empty)"")
(check-expect (checkBillboard (list "pop"))"pop")


(define (checkBillboard los)
  (if (empty? los)
      
      ""
      
      (first los)
      )
  )


[check-expect [returnAutoImages empty] empty]
[check-expect [returnAutoImages [list auto1 auto2]]
              [list AUTO-IMAGE1 AUTO-IMAGE2]]
[check-expect [returnAutoImages [list auto1]] [list AUTO-IMAGE1]]


(define (returnAutoImages loa)
  (cond [(empty? loa) empty]
        [else
         (cons (auto-image (first loa))
               (returnAutoImages (rest loa)))]))









[check-expect [returnAutoPos empty] empty]
[check-expect [returnAutoPos [list
                              [make-auto 0 400 5 1 AUTO-IMAGE1 5]
                              [make-auto 200 500 5 1 AUTO-IMAGE1 5]]]
              [list [make-posn 0 400] [make-posn 200 500]]]



(define (returnAutoPos loa)
  (cond [(empty? loa) empty]
        [else
         (cons [make-posn (auto-x (first loa)) (auto-y (first loa))]
               (returnAutoPos (rest loa)))]))




(check-expect (handle-key (make-WS 88 empty (list "hello")) "\r")
              (make-WS 88 empty (list "hello\n"))
              )
(check-expect (handle-key (make-WS 88 empty (list "hello")) "\t")
              (make-WS 88 empty (list ""))
              )
(check-expect (handle-key (make-WS 88 empty (list "hello")) "prior")
              (make-WS 88 empty empty)
              )
(check-expect (handle-key (make-WS 88 empty (list "hello")) "next")
              (make-WS 88 empty (list "" "hello"))
              )
(check-expect (handle-key (make-WS 88 empty (list "hello")) "\b")
              (make-WS 88 empty (list "hell"))
              )


(define (handle-key ws key)
  (if (empty? (WS-billBoards ws))
      
      (cond
        [(key=? key "next") (make-WS (WS-time ws) (WS-ListOfAuto ws)
                                     (append (list "")
                                             (WS-billBoards ws))
                                     )]
        [else ws]
       )
      
      (cond
        [(key=? key "\r")
         (make-WS (WS-time ws) (WS-ListOfAuto ws) 
                  (append (list (string-append (first(WS-billBoards ws)) "\n"))
                          (rest(WS-billBoards ws)))
                  )]
        [(key=? key "\t") (make-WS (WS-time ws) (WS-ListOfAuto ws)
                                   (append (list "")
                                           (rest(WS-billBoards ws)))
                                   )]
        [ (key=? key "prior") (make-WS (WS-time ws) (WS-ListOfAuto ws)
                                       (rest(WS-billBoards ws))
                                       )]
        [(key=? key "next") (make-WS (WS-time ws) (WS-ListOfAuto ws)
                                     (append (list "")
                                             (WS-billBoards ws))
                                     )]
        [(key=? key "\b") (make-WS (WS-time ws) (WS-ListOfAuto ws)
                                   (append
                                    (list (removeLast (first(WS-billBoards ws))))
                                    (rest(WS-billBoards ws)))
                                   )]
        [(key=? key "shift") ws]
        [else
         (make-WS (WS-time ws) (WS-ListOfAuto ws)
                  (append (list (string-append (first(WS-billBoards ws)) key))
                          (rest(WS-billBoards ws)))
                  )]
        )
      )
  
  
  )





(check-expect (removeLast "")"")
(check-expect (removeLast "hi")"h")


(define (removeLast string)
  (if (> 0 (- (string-length string) 1))
      
      string
      
      (substring string 0 (- (string-length string) 1))
      )
  )









[check-expect [whatLight? [make-WS 0 empty empty]]
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green"))]


[check-expect [whatLight? [make-WS 139 empty empty]]
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green"))]


[check-expect [whatLight? [make-WS 168 empty empty]]
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "solid" "yellow")
               (circle LIGHT-RADIUS "outline" "green"))]


[check-expect [whatLight? [make-WS 224 empty empty]]
              (above
               (circle LIGHT-RADIUS "solid" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "outline" "green"))]


[check-expect [whatLight? [make-WS 392 empty empty]]
              (above
               (circle LIGHT-RADIUS "outline" "red")
               (circle LIGHT-RADIUS "outline" "yellow")
               (circle LIGHT-RADIUS "solid" "green"))]


(define [whatLight? ws]
  (cond [(< (remainder (ticks->seconds [WS-time ws])
                       (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
            GREEN-LENGTH)
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "solid" "green"))
         ]
        [(< [- (remainder (ticks->seconds [WS-time ws])
                          (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
               GREEN-LENGTH] YELLOW-LENGTH)
         (above
          (circle LIGHT-RADIUS "outline" "red")
          (circle LIGHT-RADIUS "solid" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))
         ]
        [(< [- (remainder (ticks->seconds [WS-time ws])
                          (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
               [+ GREEN-LENGTH YELLOW-LENGTH]] RED-LENGTH)
         (above
          (circle LIGHT-RADIUS "solid" "red")
          (circle LIGHT-RADIUS "outline" "yellow")
          (circle LIGHT-RADIUS "outline" "green"))]))






[check-expect [whatLight2 [make-WS 0 empty empty]] "green"]
[check-expect [whatLight2 [make-WS 139 empty empty]] "green"]
[check-expect [whatLight2 [make-WS 168 empty empty]] "yellow"]
[check-expect [whatLight2 [make-WS 224 empty empty]] "red"]
[check-expect [whatLight2 [make-WS 392 empty empty]] "green"]



(define (whatLight2 ws)
  (cond [(< (remainder (ticks->seconds [WS-time ws])
                       (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
            GREEN-LENGTH)
         "green"
         ]
        [(< [- (remainder (ticks->seconds [WS-time ws])
                          (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
               GREEN-LENGTH] YELLOW-LENGTH)
         "yellow"
         ]
        [(< [- (remainder (ticks->seconds [WS-time ws])
                          (+ GREEN-LENGTH YELLOW-LENGTH RED-LENGTH))
               [+ GREEN-LENGTH YELLOW-LENGTH]] RED-LENGTH)
         "red"]))




(define-struct auto (x y dx speedMod image realSpeed))










 






 





[define tauto1 [make-auto 100 400 1 1 AUTO-IMAGE1 1]]


(check-expect (genAuto 15 (make-WS 32 (list tauto1) empty)) empty)



[define [genAuto num ws]
  [cond [[= 1 num]
         (list[make-auto 0
                         [+ Y-POS [random (- HEIGHT Y-POS
                                             (* 6 LIGHT-RADIUS))] (* 4 LIGHT-RADIUS)]
                         [+ 4 [random 10]] 1 [pick-image [random 3]]
                         [+ 4 [random 10]] ])]
        [else empty]]]

(define testGen (genAuto 1 (make-WS 32 (list tauto1) empty)))
(check-expect (length testGen) 1)






[define tauto2 [make-auto 900 400 5 1 AUTO-IMAGE2 1]]

(check-expect (updateAutos (list tauto1)
                           (make-WS (* 3 28) (list tauto1) empty))
              (list (make-auto 101 400 1 1 AUTO-IMAGE1 1 ))
              )
(check-expect (updateAutos empty (make-WS (* 3 28) empty empty))empty)

(check-expect (updateAutos (list tauto2)
                           (make-WS (* 3 28) (list tauto2) empty))
              empty
              )


(define (updateAutos loa ws)
  (cond [(empty? loa) empty]
        [[> [auto-x [first loa]] WIDTH] [updateAutos [rest loa] ws]]
        [else
         (cons (make-auto
                (moveAuto (first loa) ws)
                (auto-y (first loa))
                (updateAutoSpeed (first loa))
                (updateAutoSpeedMod (first loa) ws)
                (auto-image (first loa))
                (auto-realSpeed (first loa))
                )
               (updateAutos (rest loa) ws))]))




[define auto1 [make-auto 100 400 1 1 AUTO-IMAGE1 1]]
[define auto2 [make-auto X-POS 400 5 1 AUTO-IMAGE2 5]]
[define auto3 [make-auto (+ X-POS 1) 400 5 0.75 AUTO-IMAGE2 5]]
[define auto4 [make-auto (+ X-POS 100) 400 5 1 AUTO-IMAGE2 5]]

[check-expect [moveAuto auto1 (make-WS (* 3 28) (list auto1 empty) empty)]
              (+ (auto-dx auto1) (auto-x auto1))]
[check-expect [moveAuto auto2 (make-WS (* 3 28) (list auto2) empty)]
              [+ (auto-dx auto2) X-POS]]
[check-expect [moveAuto auto3 (make-WS (* 9 28) (list auto3) empty)]
              (auto-x auto3)]
[check-expect [moveAuto auto4 (make-WS (* 9 28) (list auto2) empty)]
              [+ (auto-dx auto4) (auto-x auto4)]]
               

[define [moveAuto aAuto ws]
  (if (string=? (whatLight2 ws) "red" )
      
      (if (and
           (>= [+ [auto-x aAuto] [auto-dx aAuto]] X-POS)
           (not(= (auto-speedMod aAuto) 1))
           )
          
          [auto-x aAuto]
          
          [+ [auto-x aAuto] [auto-dx aAuto]]
          )
      
      [+ [auto-x aAuto] [auto-dx aAuto]]
      )
  ]





(define sauto1 (make-auto 50 500 4 1 AUTO-IMAGE1 4))
(define sauto2 (make-auto (+ X-POS 50) 500 4 0.5 AUTO-IMAGE1 4) )


(check-expect (updateAutoSpeedMod sauto1 (make-WS (* 3 28) empty empty)) 1)

(check-expect (updateAutoSpeedMod sauto1 (make-WS (* 6 28) empty empty)) 0.75)

(check-expect (updateAutoSpeedMod sauto1 (make-WS (* 9 28) empty empty)) 0.5)

(check-expect (updateAutoSpeedMod sauto2 (make-WS (* 3 28) empty empty)) 1)


(define (updateAutoSpeedMod aAuto ws)
 
  (cond [(< (auto-x aAuto) X-POS) 
         (cond
           [(string=? (whatLight2 ws) "yellow" ) 0.75]
           
           [(string=? (whatLight2 ws) "red" ) 0.5]
           
           [else 1] 
           )]
        [else 1]
        )
  )






(check-expect (updateAutoSpeed sauto1 )
              (* (auto-realSpeed sauto1) (auto-speedMod sauto1)))
(check-expect (updateAutoSpeed sauto2 )
              (* (auto-realSpeed sauto2) (auto-speedMod sauto2)))


(define (updateAutoSpeed aAuto)
  (* (auto-realSpeed aAuto) (auto-speedMod aAuto))
  )

