

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname FINALAssignment_4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))




(require 2htdp/image)
(require 2htdp/universe)
(require racket/list) 







(define SIZE 700) 
(define MTS (empty-scene SIZE SIZE))
[define FONT-SIZE [round [* SIZE 0.2]]]
[define BAR-WIDTH [round [* SIZE 0.05]]]
[define LEFT/TOP-POS [* SIZE 0.35]]
[define RIGHT/BOTTOM-POS [* SIZE 0.65]]
[define BAR-CENTER [* SIZE 0.5]]
[define BAR-LENGTH [* SIZE 0.8]]
[define X-COLOR "red"]
[define O-COLOR "blue"]
[define BAR-COLOR "orange"]

[define BAR [line 0
                  BAR-LENGTH
                  [pen BAR-COLOR BAR-WIDTH "solid" "round" "round"]]]


[define left/top-Bound [* SIZE 0.1]]
[define right/bottom-Bound [- SIZE [* SIZE 0.1]]]
[define leftColumn/topRow-Bound [* SIZE 0.35]]
[define rightColumn/bottomRow-Bound [* SIZE 0.65]]



[define-struct marker [type position]]











[define M1 [make-posn [* SIZE 0.2] [* SIZE 0.2]]]
[define M2 [make-posn [* SIZE 0.50] [* SIZE 0.2]]]
[define M3 [make-posn [* SIZE 0.80] [* SIZE 0.2]]]
[define M4 [make-posn [* SIZE 0.2] [* SIZE 0.50]]]
[define M5 [make-posn [* SIZE 0.50] [* SIZE 0.50]]]
[define M6 [make-posn [* SIZE 0.80] [* SIZE 0.50]]]
[define M7 [make-posn [* SIZE 0.2] [* SIZE 0.80]]]
[define M8 [make-posn [* SIZE 0.50] [* SIZE 0.80]]]
[define M9 [make-posn [* SIZE 0.80] [* SIZE 0.80]]]








 


[define-struct WS [lom turn difficulty]]









 





[define startingBoard [make-WS empty 1 0]]

[define START startingBoard]

(define (main ws)
  (big-bang ws               
    (on-tick   tock)         
    (to-draw   render)       
    (on-mouse  handle-mouse) 
    (on-key    handle-key))) 








(check-expect (tock startingBoard) startingBoard)

(check-expect (tock [make-WS [list
                              [make-marker "O" 8]
                              [make-marker "X" 1]
                              [make-marker "O" 2]
                              [make-marker "X" 4]
                              [make-marker "O" 5]
                              [make-marker "X" 9]] 7 2])
              [make-WS [list
                        [make-marker "O" 8]
                        [make-marker "X" 1]
                        [make-marker "O" 2]
                        [make-marker "X" 4]
                        [make-marker "O" 5]
                        [make-marker "X" 9]] 7 2])



(check-expect [tock [make-WS [list [make-marker "X" 1]
                                   [make-marker "O" 2]
                                   [make-marker "X" 4]
                                   [make-marker "O" 5]
                                   [make-marker "X" 9]] 6 1]]
              [make-WS [list [make-marker "O" 8]
                             [make-marker "X" 1]
                             [make-marker "O" 2]
                             [make-marker "X" 4]
                             [make-marker "O" 5]
                             [make-marker "X" 9]] 7 1])

(check-expect [tock [make-WS [list [make-marker "X" 1]
                                   [make-marker "O" 2]
                                   [make-marker "X" 4]
                                   [make-marker "O" 5]
                                   [make-marker "X" 9]] 6 2]]
              [make-WS [list [make-marker "O" 8]
                             [make-marker "X" 1]
                             [make-marker "O" 2]
                             [make-marker "X" 4]
                             [make-marker "O" 5]
                             [make-marker "X" 9]] 7 2])

(check-expect [tock [make-WS [list [make-marker "X" 1]
                                   [make-marker "O" 3]
                                   [make-marker "X" 4]
                                   [make-marker "O" 8]
                                   [make-marker "X" 2]] 6 2]]
              [make-WS [list [make-marker "O" 7]
                             [make-marker "X" 1]
                             [make-marker "O" 3]
                             [make-marker "X" 4]
                             [make-marker "O" 8]
                             [make-marker "X" 2]] 7 2])

(define (tock ws)
  [local
    [[define [listOpenSpots lon currentList]
       [cond [[empty? lon] currentList]
             [else
              [listOpenSpots [rest lon] [remove [first lon] currentList]]]]]

     [define openSpots [listOpenSpots [buildPosList [WS-lom ws]]
                                      [list 1 2 3 4 5 6 7 8 9]]]

     [define [pickSpot lom]
       [list-ref openSpots [random [length openSpots]]]]

     [define [randomMove ws]
       [make-WS [addMarker [WS-lom ws] "O" [pickSpot [WS-lom ws]]]
                [add1 [WS-turn ws]]
                [WS-difficulty ws]]]

     [define [diff1-2 availableMoves winningMove ws lastPos char]
       [cond [[equal? winningMove char]
              [make-WS
               [append [list [make-marker "O" lastPos]] [WS-lom ws]]
               [add1 [WS-turn ws]] [WS-difficulty ws]]]
             [[equal? [length availableMoves] 0]
              ws]
             [else
              [diff1-2 [rest availableMoves]
                       [isGameOver? [make-WS
                                     [append [list [make-marker
                                                    char
                                                    [first availableMoves]]]
                                             [WS-lom ws]]
                                     [WS-turn ws]
                                     [WS-difficulty ws]]]
                       ws
                       [first availableMoves] char]]]]
     [define [move1 ws]
       [diff1-2 openSpots "" ws 10 "O"]]
     [define [move2 ws]
       [diff1-2 openSpots "" ws 10 "X"]]]
    [if [not [equal? [isGameOver? ws] ""]]
        ws
        [cond [[even? [WS-turn ws]] 
               [if [< [WS-turn ws] 4]
                   [randomMove ws]
                   [cond [[equal? [WS-difficulty ws] 0]
                          [randomMove ws]]
                         [[equal? [WS-difficulty ws] 1]
                          [if [equal? ws [move1 ws]]
                              [randomMove ws]
                              [move1 ws]]]
                         [[equal? [WS-difficulty ws] 2]
                          [if [equal? ws [move1 ws]]
                              [if [equal? ws [move2 ws]]
                                  [randomMove ws]
                                  [move2 ws]]
                              [move1 ws]]]]]] 
              [else
               [make-WS [WS-lom ws]
                        [WS-turn ws]
                        [WS-difficulty ws]]]]]])







[check-expect [isGameOver? startingBoard] ""]

[check-expect [isGameOver? [make-WS [list [make-marker "X" 1]
                                          [make-marker "O" 4]
                                          [make-marker "X" 2]
                                          [make-marker "O" 6]
                                          [make-marker "X" 3]]
                                    6 0]]
              "X"]

[check-expect [isGameOver? [make-WS [list [make-marker "X" 1]
                                          [make-marker "O" 2]
                                          [make-marker "X" 3]
                                          [make-marker "O" 5]
                                          [make-marker "X" 9]
                                          [make-marker "O" 8]]
                                    7 0]]
              "O"]

[check-expect [isGameOver? [make-WS [list [make-marker "X" 1]
                                          [make-marker "O" 2]
                                          [make-marker "X" 5]
                                          [make-marker "O" 6]
                                          [make-marker "X" 9]]
                                    6 0]]
              "X"]

[check-expect [isGameOver? [make-WS [list [make-marker "X" 1]
                                          [make-marker "O" 2]
                                          [make-marker "X" 3]
                                          [make-marker "O" 4]
                                          [make-marker "X" 5]
                                          [make-marker "O" 9]
                                          [make-marker "X" 8]
                                          [make-marker "O" 7]
                                          [make-marker "X" 6]]
                                    10 0]]
              "Draw"]



[define [isGameOver? ws]
  [local
    [[define [buildList lom char]
       [cond [[empty? lom] empty]
             [else
              [if [equal? char "X"]
                  [if [equal? [marker-type [first lom]] "X"]
                      [cons [first lom] [buildList [rest lom] char]]
                      [buildList [rest lom] char]]
                  [if [equal? [marker-type [first lom]] "O"]
                      [cons [first lom] [buildList [rest lom] char]]
                      [buildList [rest lom] char]]]]]]
     [define [checkWin lon char]
       [cond [[and [member 1 lon] [member 2 lon] [member 3 lon]] char]
             [[and [member 1 lon] [member 4 lon] [member 7 lon]] char]
             [[and [member 2 lon] [member 5 lon] [member 8 lon]] char]
             [[and [member 3 lon] [member 6 lon] [member 9 lon]] char]
             [[and [member 4 lon] [member 5 lon] [member 6 lon]] char]
             [[and [member 7 lon] [member 8 lon] [member 9 lon]] char]
             [[and [member 1 lon] [member 5 lon] [member 9 lon]] char]
             [[and [member 3 lon] [member 5 lon] [member 7 lon]] char]
             [else
              ""]]]
     [define listX [buildList [WS-lom ws] "X"]]
     [define listO [buildList [WS-lom ws] "O"]]
     [define listXPos [buildPosList listX]]
     [define listOPos [buildPosList listO]]]
    [cond [[< [length listX] 3] ""]
          [[equal? [checkWin listXPos "X"] "X"] "X"]
          [[equal? [checkWin listOPos "O"] "O"] "O"]
          [[equal? [length [WS-lom ws]] 9] "Draw"]
          [else
           ""]]]]






[check-expect [buildPosList [WS-lom startingBoard]] empty]
[check-expect [buildPosList [list [make-marker "X" 1] [make-marker "O" 2]]]
              [list 1 2]]



[define [buildPosList lom]
  [cond [[empty? lom] empty]
        [else
         [cons [marker-position [first lom]] [buildPosList [rest lom]]]]]]






[check-expect [handle-mouse startingBoard 0 0 "button-down"] startingBoard]

[check-expect [handle-mouse startingBoard
                            [* SIZE 0.09]
                            [* SIZE 0.2]
                            "button-down"]
              startingBoard]

[check-expect [handle-mouse startingBoard
                            [* SIZE 0.2]
                            [* SIZE 0.09]
                            "button-down"]
              startingBoard]

[check-expect [handle-mouse startingBoard
                            [- SIZE [* SIZE 0.09]]
                            [* SIZE 0.2]
                            "button-down"]
              startingBoard]

[check-expect [handle-mouse startingBoard
                            [* SIZE 0.2]
                            [- SIZE [* SIZE 0.09]]
                            "button-down"]
              startingBoard]

[check-expect [handle-mouse startingBoard
                            [* SIZE 0.15]
                            [* SIZE 0.15]
                            "button-down"]
              [make-WS [list [make-marker "X" 1]] 2 0]]

[check-expect [handle-mouse [make-WS
                             [list [make-marker "X" 2]
                                   [make-marker "O" 3]] 3 0]
                            [* SIZE 0.15]
                            [* SIZE 0.15]
                            "button-down"]
              [make-WS [list [make-marker "X" 1]
                             [make-marker "X" 2]
                             [make-marker "O" 3]] 4 0]]



(define (handle-mouse ws x y me)
  [if [not [equal? [isGameOver? ws] ""]] 
      ws
      (cond [(mouse=? me "button-down")
             
             [if [and [and [> x left/top-Bound] [< x right/bottom-Bound]]
                      [and [> y left/top-Bound] [< y right/bottom-Bound]]] 
                 [cond [[< y leftColumn/topRow-Bound]           
                        [cond [[< x leftColumn/topRow-Bound]      
                               [make-WS [addMarker [WS-lom ws] "X" 1]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [[< x rightColumn/bottomRow-Bound]  
                               [make-WS [addMarker [WS-lom ws] "X" 2]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [else                               
                               [make-WS [addMarker [WS-lom ws] "X" 3]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]]]
                       [[< y rightColumn/bottomRow-Bound]    
                        [cond [[< x leftColumn/topRow-Bound]      
                               [make-WS [addMarker [WS-lom ws] "X" 4]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [[< x rightColumn/bottomRow-Bound]  
                               [make-WS [addMarker [WS-lom ws] "X" 5]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [else                               
                               [make-WS [addMarker [WS-lom ws] "X" 6]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]]]
                       [else
                        [cond [[< x leftColumn/topRow-Bound]      
                               [make-WS [addMarker [WS-lom ws] "X" 7]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [[< x rightColumn/bottomRow-Bound]  
                               [make-WS [addMarker [WS-lom ws] "X" 8]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]
                              [else                               
                               [make-WS [addMarker [WS-lom ws] "X" 9]
                                        [add1 [WS-turn ws]]
                                        [WS-difficulty ws]]]]]]
                           
                 ws]] 
            [else
             ws])])





[check-expect [handle-key startingBoard "0"] [make-WS empty 1 0]]
[check-expect [handle-key startingBoard "1"] [make-WS empty 1 1]]
[check-expect [handle-key [make-WS [list [make-marker "X" 1]
                                         [make-marker "O" 2]] 3 0] "2"]
              [make-WS [list [make-marker "X" 1] [make-marker "O" 2]] 3 2]]


[define [handle-key ws key]
  [cond [[key=? "0" key]
         [make-WS [WS-lom ws] [WS-turn ws] 0]]
        [[key=? "1" key]
         [make-WS [WS-lom ws] [WS-turn ws] 1]]
        [[key=? "2" key]
         [make-WS [WS-lom ws] [WS-turn ws] 2]]
        [else
         ws]]]






[check-expect [addMarker [WS-lom startingBoard] "X" 1]
              [list [make-marker "X" 1]]]
[check-expect [addMarker [list [make-marker "X" 1]] "O" 2]
              [list [make-marker "O" 2] [make-marker "X" 1]]]


[define [addMarker lom char pos]
  [local
    
    [[define [search-list lom2 pos] 
       [cond
         [(empty? lom2) false]
         [(equal? pos (marker-position (first lom2))) true]
         [else
          [search-list (rest lom2) pos]]]]]
    [if [search-list lom pos]
        lom
        [cons [make-marker char pos] lom]]]]





(check-expect [render startingBoard]
              [place-images
               [append [list [text [string-append "Difficulty: "
                                                  [number->string
                                                   [WS-difficulty
                                                    startingBoard]]]
                                   [quotient FONT-SIZE 5] "black"]]
                       [list BAR BAR [rotate 90 BAR] [rotate 90 BAR]]
                       ]
               [append [list [make-posn [* SIZE 0.85] [* SIZE 0.05]]]
                       [list [make-posn LEFT/TOP-POS BAR-CENTER]
                             [make-posn RIGHT/BOTTOM-POS BAR-CENTER]
                             [make-posn BAR-CENTER RIGHT/BOTTOM-POS ]
                             [make-posn BAR-CENTER LEFT/TOP-POS]]] MTS])
(check-expect [render [make-WS [list [make-marker "X" 1]
                                     [make-marker "O" 5]] 3 1]]
              [place-images
               [append [list [text [string-append "Difficulty: "
                                                  "1"]
                                   [quotient FONT-SIZE 5] "black"]]
                       [list BAR BAR [rotate 90 BAR] [rotate 90 BAR]]
                       [getMarkerImages [list [make-marker "X" 1]
                                              [make-marker "O" 5]]]]
               [append [list [make-posn [* SIZE 0.85] [* SIZE 0.05]]]
                       [list [make-posn LEFT/TOP-POS BAR-CENTER]
                             [make-posn RIGHT/BOTTOM-POS BAR-CENTER]
                             [make-posn BAR-CENTER RIGHT/BOTTOM-POS ]
                             [make-posn BAR-CENTER LEFT/TOP-POS]]
                       [getMarkerPos [list [make-marker "X" 1]
                                           [make-marker "O" 5]]]] MTS]) 
(define (render ws)
  [local
    [[define [displayWinningText winner]
       [cond [[equal? "" winner]
              [text "" FONT-SIZE "black"]]
             [[equal? "X" winner]
              [text "You Won!" FONT-SIZE "green"]]
             [[equal? "O" winner]
              [text "You Lost!" FONT-SIZE "green"]]
             [[equal? "Draw" winner]
              [text "It's a Draw!" FONT-SIZE "green"]]]]]
    [place-images
     [append [list [text [string-append "Difficulty: "
                                        [number->string [WS-difficulty ws]]]
                         [quotient FONT-SIZE 5] "black"]]
             [list [displayWinningText [isGameOver? ws]]]
             [list BAR BAR [rotate 90 BAR] [rotate 90 BAR]]
             [getMarkerImages [WS-lom ws]]]
     [append [list [make-posn [* SIZE 0.85] [* SIZE 0.05]]]
             [list [make-posn [/ SIZE 2] [/ SIZE 2]]]
             [list [make-posn LEFT/TOP-POS BAR-CENTER]
                   [make-posn RIGHT/BOTTOM-POS BAR-CENTER]
                   [make-posn BAR-CENTER RIGHT/BOTTOM-POS ]
                   [make-posn BAR-CENTER LEFT/TOP-POS]]
             [getMarkerPos [WS-lom ws]]] MTS]])







[check-expect [getMarkerImages empty] empty]
[check-expect [getMarkerImages [list [make-marker "X" 1]]]
              [list [text "X" FONT-SIZE X-COLOR]]]
[check-expect [getMarkerImages [list [make-marker "X" 1]
                                     [make-marker "O" 2]]]
              [list [text "X" FONT-SIZE X-COLOR]
                    [text "O" FONT-SIZE O-COLOR]]]


[define [getMarkerImages lom]
  [local
    [[define [whichColor? marker]
       [if [equal? "X" [marker-type marker]]
           X-COLOR
           O-COLOR]]]
    [cond [[empty? lom] empty]
          [else
           [cons [text [marker-type [first lom]]
                       FONT-SIZE
                       [whichColor? [first lom]]]
                 [getMarkerImages [rest lom]]]]]]]










[check-expect [getMarkerPos empty] empty]
[check-expect [getMarkerPos  [list [make-marker "X" 1]]]
              [list M1]]
[check-expect [getMarkerPos [list [make-marker "X" 1] [make-marker "O" 2]]]
              [list M1 M2]]


[define [getMarkerPos lom]
  [cond [[empty? lom] empty]
        [else
         [cons
          [cond
            [[equal? 1 [marker-position [first lom]]] M1]
            [[equal? 2 [marker-position [first lom]]] M2]
            [[equal? 3 [marker-position [first lom]]] M3]
            [[equal? 4 [marker-position [first lom]]] M4]
            [[equal? 5 [marker-position [first lom]]] M5]
            [[equal? 6 [marker-position [first lom]]] M6]
            [[equal? 7 [marker-position [first lom]]] M7]
            [[equal? 8 [marker-position [first lom]]] M8]
            [[equal? 9 [marker-position [first lom]]] M9]]
          [getMarkerPos [rest lom]]]]]]






[define [minimax ws depth]
  [local
    [[define [listOpenSpots lon currentList]
      [cond [[empty? lon] currentList]
            [else
             [listOpenSpots [rest lon] [remove [first lon] currentList]]]]]
    [define openSpots [listOpenSpots [buildPosList [WS-lom ws]]
                                     [list 1 2 3 4 5 6 7 8 9]]]
    
    
    
    [define [try-moves listofWS]
      [local [[define first-in-list [isGameOver? [first listofWS]]]]
        [cond [[empty? listofWS] empty]
              [else
               [cons [cond
                       [[equal? first-in-list "O"] 1]
                       [[equal? first-in-list "X"] -1]
                       [[equal? first-in-list "Draw"] 0]
                       [else empty]]
                     [try-moves [rest listofWS]]]]]]]

    
    
    [define [create-next-moves lon ws]
      [cond [[empty? lon] empty]
            [else
             [cons [make-WS [append
                             [list [make-marker "O" [first lon]]] [WS-lom ws]]
                            [WS-turn ws]
                            [WS-difficulty ws]]
                   [create-next-moves [rest lon] ws]]]]]
    
    [define [fn-for-max ws depth]
      [if [< depth 2]
          [apply max [try-moves [create-next-moves openSpots ws]]]
          [apply max [fn-for-min ws [sub1 depth]]]]]

    [define [fn-for-min ws depth]
      [min [fn-for-max ws [sub1 depth]]]]]
    
[fn-for-max ws depth]]] 