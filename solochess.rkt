#lang racket

;; solo chess : from Madacasca. 
;; 2015/5/28
;
; chess-board: 3 5 7 7 7 5 3
;

(define poslist #(               (1 3) (1 4) (1 5)
                                (2 2) (2 3) (2 4) (2 5) (2 6)
                        (3 1) (3 2) (3 3) (3 4) (3 5) (3 6) (3 7)
                        (4 1) (4 2) (4 3) (4 4) (4 5) (4 6) (4 7)
                        (5 1) (5 2) (5 3) (5 4) (5 5) (5 6) (5 7)
                                (6 2) (6 3) (6 4) (6 5) (6 6)
                                        (7 3) (7 4) (7 5)
                        )) ; a static global variable
                        
(define poslen (vector-length poslist))  ; a static global variable

; Find arbitrary (x, y) position with a given index
(define (xy index) (vector-ref poslist index))

; encode the position to an index; return #f if there is no such index
(define (indexx xy) (vector-member xy poslist)) 

;===========================================================================================

; cautious: three variables with global scope
(define chess (make-vector poslen 1)) 
(define record null)               ; record of moves
(define chess-record null)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;===========================================================================================

(define strategy 100) ; a static global parameter

(define (directs pair)
  (cond  [(equal? pair '(0 1)) '>]
            [(equal? pair '(0 -1)) '<]
            [(equal? pair '(1 0)) 'V]
            [(equal? pair '(-1 0)) '^]
            [else (error 'directs "Wrong moving direction")]
             ))
;===========================================================================================
; initialization
; every time before using, have to intialize to all 1 again
(define (initi) (vector-fill! chess 1) (set! record null) (set! chess-record (list (vector->list chess)))
  (set! strategy (random 100))
  )

; randomly drop one
(define (dropfirst) (let [(ranpos (random poslen))]
;                          (set! record (append record (list (xy ranpos)) ))                  
                          (vector-set! chess ranpos 0) 
                          (set! chess-record (append chess-record (list (vector->list chess))))
                      ))
                      
; designating dropping (3, 3)
; call (drops a b) to drop the stone at position (a, b)
(define (drops a b) (vector-set! chess (indexx (list a b)) 0)
                            (set! chess-record (append chess-record (list (vector->list chess))))
)

; jump from '(x y) to direction (a, b)
(define (jump xy ab) ; xy is of form '(x y), ab is '(a b) 
  (let* [ (x (first xy)) (y (last xy))
           (a (first ab)) (b (last ab))
           (this (indexx (list x y)) )
           (next (indexx (list (+ x a) (+ y b))) )
           (next-to-next (indexx (list (+ x a a) (+ y b b))) )
        ]
  (if (not (and
              (= (vector-ref chess this) 1)
              (= (vector-ref chess next) 1)
              (= (vector-ref chess next-to-next) 0)
              ) ) (error 'jump "Wrong position") null)
  (vector-set! chess this 2)
  (vector-set! chess next 3)
  (set! chess-record (append chess-record (list (vector->list chess))))
  
  (vector-set! chess this 0)
  (vector-set! chess next 0)
  (vector-set! chess next-to-next 1)
  (set! chess-record (append chess-record (list (vector->list chess))))

  (set! record (append record (list (list xy (directs ab)))))
    ))

; withdraw a previous jump from '(x y) to direction (a, b)
(define (withdraw xy ab) ; xy is of form '(x y), ab is '(a b) 
  (let* [ (x (first xy)) (y (last xy))
           (a (first ab)) (b (last ab))
           (this (indexx (list x y)) )
           (next (indexx (list (+ x a) (+ y b))) )
           (next-to-next (indexx (list (+ x a a) (+ y b b))) )
        ]
  (if (not (and
              (= (vector-ref chess this) 0)
              (= (vector-ref chess next) 0)
              (= (vector-ref chess next-to-next) 1)
              ) ) (error 'withdraw "Wrong position") null)
  (vector-set! chess this 1)
  (vector-set! chess next 1)
  (vector-set! chess next-to-next 0)
  (set! record (drop-right record 1)) 
  (set! chess-record (drop-right chess-record 2)) 
    ))

; the empty positions or stones in the chess
(define (where flag) ; flag = 0, 1 ; if flag=1 it are stones, otherwise empties
   (let [ (idx -1) ] ; index
      (vector->list (vector-filter (lambda (x) x) 
         (vector-map (lambda (x) (set! idx (add1 idx)) (if (= x flag) idx #f) )
           chess)
       ) ) ))

; all possible moves, of the form (index, (a, b)) where a b = -1, 0, 1
(define (possible-move)
  (define (add-directions lst) ; add 4 directions to each stone or empty position
    (if (null? lst) lst (let [(head (car lst))] `((,head (0 1)) (,head (0 -1)) (,head (1 0)) (,head (-1 0)) ,@(add-directions (cdr lst))))))
  (define (valid some-move)
     (let* [ (posxy (xy (first some-move))) (ab (last some-move))
               (x (first posxy)) (y (last posxy))
              (a (first ab)) (b (last ab))
              (this (indexx (list x y)) )
              (next (indexx (list (+ x a) (+ y b))) )
              (next-to-next (indexx (list (+ x a a) (+ y b b))) )
            ]
    (and     this next next-to-next
              (= (vector-ref chess this) 1)
              (= (vector-ref chess next) 1)
              (= (vector-ref chess next-to-next) 0)
    ) ))
        (filter valid (add-directions (where 1)))
  )
        
;===========================================================================================
;
; totally randomly moving
;
(define (random-move)
  (let [(moves (possible-move))]
    (if (= (length moves) 0)
       (length (where 1)) ; how many remain finally
       (let [(move (list-ref moves 
                            (random (length moves))
                            ))]
         (jump (xy (first move)) (last move))
         (random-move) ; recursion
        )
     )
   )
)  
;
;===========================================================================================
; maximum-next-step-move
;
(define (randomchoose lst)
  (list-ref lst (random (length lst))))
;
;  The positions where maximum appears
(define (maxpositions lst)
  (define (maxof lst)
    (if (null? lst) 0 
      (let [(m (maxof (cdr lst)))]
        (if (> (car lst) m) (car lst) m))))
   (let [ (idx -1) (flag (maxof lst)) ] ; index
      (filter (lambda (x) x) 
         (map (lambda (x) (set! idx (add1 idx)) (if (= x flag) idx #f) )
           lst)
       ) ) )
;
(define (maximum-move)
  (let [(moves (possible-move))]
    (if (= (length moves) 0)
       (length (where 1)) ; how many remain finally
       (let [(move (list-ref moves 
          (let [ (maxlist (maxpositions (map 
                (lambda (m) 
                    (let [ (mxy (xy (first m)))
                            (ab (last m))
                            (ch 0) ; for checking
                            (r 0)]
                     (set! ch chess) ; for checking
                     (jump mxy ab)
                     (set! r (length (possible-move))) 
                     (withdraw mxy ab)
                     (if (not (eqv? ch chess)) (error 'withdraw "wrong move") null) ; for checking 
                     r))
                moves) ) ) ]
           (randomchoose maxlist)
          )))]
         (jump (xy (first move)) (last move))
         (maximum-move) ; recursion
        )
     )
   )
)  
;
;===========================================================================================
;
; a general module for trial strategy
; just need to implement pick-move
;
(define (trial-move)
  (let [(moves (possible-move))]
    (if (= (length moves) 0)
       (length (where 1)) ; how many remain finally
       (let [(move (list-ref moves 
                            (pick-move moves) ; moves is a list of (index, (a, b))
                            ))]
         (jump (xy (first move)) (last move))
         (trial-move) ; recursion
        )
     )
   )
)  
;
;===========================================================================================
;
; to define pick-move for the use of trial-move in the above

;(define (pick-move moves) ; random-move
;  (random (length moves)) )

;(define (pick-move moves)
;  (let [ (maxlist (maxpositions (map 
;               (lambda (m) (middleness (xy (first m)))) 
;                  moves) ) ) ]
;    (randomchoose maxlist)
;    ))


(define (middleness pair) ; test how far is a stone from the middle
   (let [ (x (first pair)) 
           (y (last pair))
         ]
    (+ (abs (- x 4)) (abs (- y 4)))
   ) )
;


; test if any stone is on the edge (should be removed first)
(define (is-edge pair)
  (define edge #(               (1 3) (1 4) (1 5)
                                (2 2)                         (2 6)
                        (3 1)                                         (3 7)
                        (4 1)                                         (4 7)
                        (5 1)                                         (5 7)
                                (6 2)                          (6 6)
                                        (7 3) (7 4) (7 5)
                        ))
 (not (eq? (vector-member pair edge) #f)))
;
(define (is-edge2 pair) ; also can use this one, less edge stones
  ; seems it is even better than is-edge! Find the solution quicker
  (define edge #(               (1 3)          (1 5)
                                (2 2)                         (2 6)
                        (3 1)                                         (3 7)

                        (5 1)                                         (5 7)
                                (6 2)                          (6 6)
                                        (7 3)          (7 5)
                        ))
 (not (eq? (vector-member pair edge) #f)))
;
(define (is-edge-strategy pair)
  (if (> strategy 50) (is-edge pair) 
     (is-edge2 pair)
     ))
  
;
(define (remove-edges move) ; how many edges one move removes?
  (let* [ (mxy (xy (first move)))
            (ab (last move))
            (x (first mxy)) (y (last mxy))
            (a (first ab)) (b (last ab))
            (this (list x y))
            (next (list (+ x a) (+ y b)))
            (next-to-next (list (+ x a a) (+ y b b)))         
            (r 1)
        ]
    (if (is-edge-strategy this) (set! r (+ r 1)) null)
    (if (is-edge-strategy next) (set! r (+ r 1)) null)
    (if (is-edge-strategy next-to-next) (set! r (- r 1)) null)
    r
   )
)         

(define (pick-move moves) ; may rename this to pick-move
     (let [ (maxlist (maxpositions (map 
              (lambda (m) (remove-edges m)) 
                  moves) ) ) ] ; only leave those moves removing edges the most

      (let [ (lst  (maxpositions (map 
                (lambda (m) 
                    (let [ (mxy (xy (first (list-ref moves m))))
                            (ab (last (list-ref moves m)))
                            (ch 0) ; for checking
                            (r 0)]
                     (set! ch chess) ; for checking
                     (jump mxy ab)
                     (set! r (length (possible-move))) 
                     (withdraw mxy ab)
                     (if (not (eqv? ch chess)) (error 'withdraw "wrong move") null) ; for checking 
                     r))
                maxlist) ) ) ]
                 
   (list-ref maxlist (randomchoose lst))
               ) 
  ))

;
(define (pick-move2 moves) ; may rename this to pick-move
          (let [ (maxlist  (maxpositions (map 
                (lambda (m) 
                    (let [ (mxy (xy (first m)))
                            (ab (last m))
                            (ch 0) ; for checking
                            (r 0)]
                     (set! ch chess) ; for checking
                     (jump mxy ab)
                     (set! r (length (possible-move))) 
                     (withdraw mxy ab)
                     (if (not (eqv? ch chess)) (error 'withdraw "wrong move") null) ; for checking 
                     r))
                moves) ) ) ]
            
      (let [ (lst (maxpositions (map 
               (lambda (m) (remove-edges (list-ref moves m))) 
                  maxlist) ) ) ] ; only leave those moves removing edges the most

   (list-ref maxlist (randomchoose lst))
               ) 
  ))

(define num 0)

(define (printme r)
  (if (> (length r) 0) 
   (begin
    (set! num (add1 num))
    (print num)
    (if (< num 10) (display " ") null)
    (display ":  ")
    (display (car (car r)))
    (display "  ")
    
    
    (display (last (car r)))
    (display "\n")
    (printme (cdr r))
   ) null
  ))

(define str "")
(define loopuntil 3) ; until when the search stops

(define (loop)  
  (initi)
  (dropfirst)  
  (let [(r (trial-move))]
    (set! str (string-join (list str (number->string r))))
    (if (> (string-length str) 10) (set! str (substring str (- (string-length str) 10))) null)
    (send my-msg set-label str)
    (set! num 0)
                         
    (if (<= r loopuntil) (begin (printme record)
                               (send my-msg set-label (string-join  (list "剩余" (number->string r))))
                     )  (loop))
  ))  

(define move-num 0)
(define radius 15)
(define size 1.78)

(define (coordinates index)
  
    (let* ([c-ij (xy index)]
           [c-i (first c-ij)]
           [c-j (second c-ij)]
           [x-coor (+ 7 (* radius (- c-i 1/2)) (* 2.2 (- c-i 1)))]
           [y-coor (+ (* radius (- c-j 1/2)) (* 4.7 (- c-j 1)))]
      )

      (list (* x-coor size) (* size y-coor))      
      )
  )


(define (re-draw dc)  

  (define now-chess null)
  
  (if (null? chess-record) (set! chess-record (list (vector->list chess))) null)
 
  (if (>= move-num (length chess-record))
     (set! now-chess (last chess-record))
     (set! now-chess (list-ref chess-record move-num)) ; else
  )   
  (set! move-num (add1 move-num))
 

      
  (define chesspos -1)    
  
  (define (pickstone) 
    (set! chesspos (add1 chesspos))
    
    (define c-xy (coordinates chesspos))
    (define state (list-ref now-chess chesspos)) ; get stone empty or filled depending on chess state
    
    (send dc set-brush (cond [(= state 0) "red"]
                             [(= state 1) "red"]
                             [(= state 2) "green"]
                             [(= state 3) "yellow"]
                             ) (if (= state 0) 'transparent 'solid) 
                               )    

    (send dc draw-ellipse (first c-xy) (second c-xy) (* radius size) (* radius size))  

    )
  
  (for ([index poslen])
    (pickstone)
    )
  
  )

(define (mouse-clicked dc x y)
  
  (define chosen #f)
  
  (for ([index poslen])
       #:break chosen
    (let* ([cur-xy (coordinates index)]
           [cur-x (first cur-xy)]
           [cur-y (second cur-xy)]
           [radsq (* radius size radius size)])
           
    (if (<= (+ (* (- x cur-x) (- x cur-x)) (* (- y cur-y) (- y cur-y)))
            radsq) 
        (set! chosen index)
        null
        )
      )
    )
  
  (if (null? chosen)
      null
      (let* ([cur-xy (coordinates chosen)]
             [cur-x (first cur-xy)]
             [cur-y (second cur-xy)])
         (send dc set-brush "pink" 'solid)
         (send dc draw-ellipse cur-x cur-y (* radius size) (* radius size))
      ) 
     )
        
)
  
; ==========================================================
; display the animation
;
(require racket/class racket/gui/base)
             
(define my-frame (new frame% [label "米勒酷孤独棋"] ; define a new frame
                                  [width 300] [height 400]
                                  [alignment '(center center)] ))

(define my-canvas ; set up a canvas for pictures
      (new (class canvas%
             (super-new [parent my-frame])
             [define/override (on-paint)
               (define my-dc (send my-canvas get-dc))
               (send my-dc clear)
               (re-draw my-dc)
                ]
             
             [define/override (on-char ch)
               (define key (send ch get-key-code))
               (if (eq? key 'release) (send my-canvas on-paint) null) ; when key is released, move one step and paint
             ]
             
             [define/override (on-event event)
               (define my-dc (send my-canvas get-dc))
               (define button-pressed (send event button-down? 'any))
               (if button-pressed
                  (mouse-clicked my-dc (send event get-x) (send event get-y))
                   null
               )   
               ; 
              ]   
             ))) 

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define my-pane (new horizontal-pane% ; define a horizontal line of buttons
                    [parent my-frame] [alignment '(center center)]
                    [stretchable-height #f] [spacing 4] ; determine the outlook
                    ))

(define search-button
(new button% [parent my-pane]
             [label "快速运行"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                           (send search-button set-label "搜寻中")
                           (loop)
                           (send search-button set-label "搜寻")
                         )]))

(define button-choices-list (list 1 2 3 4 10)) ; a static global parameter
(define loopcond-button
  (new choice% [parent my-pane] 
       [label "至剩余 "] [choices (map number->string button-choices-list)]
       [selection (- (length button-choices-list) (length (member loopuntil button-choices-list)))  ] 
       [callback (lambda (button event)
                   (set! loopuntil (list-ref button-choices-list (send loopcond-button get-selection))))]
    ))
    
; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define my-pane2 (new horizontal-pane% ; define a horizontal line of buttons
                    [parent my-frame] [alignment '(center center)]
                    [stretchable-height #f] [spacing 4] ; determine the outlook
                    ))

(new button% [parent my-pane2]
             [label "播放"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                           (send my-msg set-label (if (= (length chess-record) 1) "   无棋局" 
                                                                   (string-join  (list "剩余" (number->string (apply + (last chess-record)))))
                                                                  ))
                           (send my-canvas on-paint)
                         ) ])

(define (auto-play)  
   (send my-canvas on-paint)
   (sleep 0.6)
   (if (>= move-num (length chess-record)) null (auto-play))
  )

(new button% [parent my-pane2]
             [label "重放"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                           (set! move-num 0)
                           (send my-msg set-label "                ")
                           (send my-canvas on-paint)
                         ) ])

(define auto-play-button
(new button% [parent my-pane2]
             [label "慢放"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                          (set! move-num 0)
                          (send auto-play-button set-label "慢放中") 
                          (auto-play)
                          (send auto-play-button set-label "慢放") 
                          (send my-msg set-label "播放结束！"))
                            ]))

; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define my-pane3 (new horizontal-pane% ; define a horizontal line of buttons
                    [parent my-frame] [alignment '(center center)]
                    [stretchable-height #f] [spacing 4] ; determine the outlook
                    ))


(new button% [parent my-pane3]
             [label "载入"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                            (define file-name (get-file "您想载入的文件名：" my-frame #f #f "slu"))
                            (define in #f)
                            (if file-name
                              (begin
                              (set! in (open-input-file file-name))
                              (set! chess-record (read in))
                              (close-input-port in)
                              (set! move-num 0)
                              (send my-canvas on-paint)
                              (send my-msg set-label "载入成功！"))
                              null
                           ))])

(new button% [parent my-pane3]
             [label "保存"]
             [callback (lambda (button event)              ; Callback procedure for a button click
                            (define file-name (get-file "您想保存的文件名：" my-frame #f #f "slu"))
                            (define out #f)
                            (if file-name
                               (begin
                               (set! out (open-output-file file-name))
                               (write chess-record out)
                               (close-output-port out)
                               (send my-msg set-label "保存成功！"))
                               null
                               ))])
 
(define my-msg
  (new message% [parent my-frame] [label "   无棋局"]))

(send my-frame show #t) ; show the frame

