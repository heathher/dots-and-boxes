#lang racket
(require racket/gui racket/draw "dots-and-boxes.rkt")

(include "dots-and-boxes.rkt")

(define n 5)
(define 1-player #f)
(define p1 user-A)
(define p2 user-B)

(define main-window (new frame%
                         [label "Dots and Boxes"]
                         [min-width 700]
                         [min-height 500] 
                         [stretchable-width #f]
                         [stretchable-height #f]))

(define h-panel (new horizontal-panel% (parent main-window)))

(define canvas-with-events%
  (class canvas%
    (define/override (on-event event)
      (if (send event button-down? 'left)
          (mark (send event get-x) (send event get-y))
          #f))
    (super-new)))

(define gui-board (new canvas-with-events%
                       [parent h-panel]
                       [min-width 500]
                       [stretchable-width #f]))

; Make some pens and brushes
(define no-pen (make-object pen% "BLACK" 1 'transparent))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define blue-brush (make-object brush% "BLUE" 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define yellow-pen (make-object pen% "YELLOW" 2 'solid))
(define blue-pen (make-object pen% "BLUE" 2 'solid))
(define green-pen (make-object pen% "GREEN" 2 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))
(define pink-pen (make-object pen% "PINK" 2 'solid))

(define (draw-board dc)
  (send dc set-pen red-pen)    
  (for ((y n))
    (for ((x n))
      (send dc draw-rectangle
            (+ 125 (* 50 x))
            (+ 125 (* 50 y))
            50 50))))

(define dc (send gui-board get-dc))

(define game-over #f)
(define last-played-pos '(0 0 0))

(define (mark x y)
  (define (stick-rotate x)
    (let ((r (remainder (- x 25) 50)))
      (if (and (> r 10) (< r 40))
          0 1)))
 
  (define corner-x 0)
  (define corner-y 0)
  (define (cross)
    (send dc draw-line (+ 15 corner-x) (+ 15 corner-y) (+ 35 corner-x) (+ 35 corner-y))
    (send dc draw-line (+ 15 corner-x) (+ 35 corner-y) (+ 35 corner-x) (+ 15 corner-y)))
  (define (circle)
    (send dc draw-ellipse (+ 15 corner-x) (+ 15 corner-y) 20 20))
  
  (define (display-letters)
    (for ((y n))
      (for ((x n))
        (set! corner-x (+ 125 (* x 50)))
        (set! corner-y(+ 125 (* y 50)))
        (cond ((set-member? (as cur-board) (list y x 0)) (cross))
              ((set-member? (bs cur-board) (list y x 0)) (circle)) ))))

  (define (display-sticks)
    (let ((s (sticks cur-board)))
      (map (lambda(x) (one-stick (list-ref x 0) (list-ref x 1) (list-ref x 2))) (set->list s))))

  (define (one-stick x y z)
    (let ((x1 (+ 125 (* y 50)))
                (y1 (+ 125 (* x 50)))
                (x2 (if (= z 0) (+ 175 (* y 50)) (+ 125 (* y 50))))
                (y2 (if (= z 0) (+ 125 (* x 50)) (+ 175 (* x 50)))))
          ; add possible-move (нельзя вне поля и ход, который уже был сделан(?))
          (send dc draw-line x1 y1 x2 y2)))

  (define (make-turn)
    (define stick-y (floor (/ (- x 115) 50)))
    (define stick-x (floor (/ (- y 115) 50)))
    (define stick-z (stick-rotate x))

    (define (second-pos y x z)
      (if (= z 0)
          ( (+ 175 (* x 50)) (+ 125 (* y 50)))
          ( (+ 125 (* x 50)) (+ 175 (* y 50)))))

    (set! last-played-pos (list stick-x stick-y stick-z))
    

    (if (not game-over)
        (begin
          (send dc set-pen black-pen)
          (one-stick stick-x stick-y stick-z)
          (cond ((and my-turn (not 1-player))
                 (begin
                   (displayln "My turn")
                   (send p1 set!-value last-played-pos)
                   (send p1 your-turn cur-board)
                   (send process-msg-area set-label "First player's turn: X")))
                ((and (not my-turn) (not 1-player))
                (begin
                  (displayln "Not my turn")
                  (send p2 set!-value last-played-pos)
                  (send p2 your-turn cur-board)
                  (send process-msg-area set-label "Second player's turn: O")))
           
                ((and 1-player my-turn)
                 (begin
                   (displayln "My turn")
                   (send p1 set!-value last-played-pos)
                   (send p1 your-turn cur-board)
                   (send process-msg-area set-label "First player's turn: X")
                   (display-sticks)
                   (if (not my-turn)
                       (send p2 your-turn cur-board)
                       (send process-msg-area set-label "Second player's turn: O"))))
              (else (void))
              )
          (if (end-game? cur-board)
              (begin
                (game-over-processes)
                (send win-msg set-label (string-append "Player " (number->string (if my-turn 1 2)) " has won!"))
                (send win-notif show #t))
              (void))
          (display-letters)
          (display-sticks))
        (void)
    ))
  (make-turn)
  )

(define v-panel (new vertical-panel% (parent h-panel)))

(define msg-area (new message%
                      [parent v-panel]
                      [vert-margin 50]
                      [label "Welcome"]
                      [min-width 250]
                      [font (make-object font% 13.5 'system)]
                      [auto-resize #t]))

(define win-notif (new dialog% (label "Game Over")
                       (width 400)
                       (height 50)))
(define win-msg (new message% (parent win-notif)
                     (label "")
                     (horiz-margin 50)
                     (auto-resize #t)
                     [font (make-object font% 15 'system)]))
(define ok-button
  (new button%
       [parent win-notif]
       [label "OK"]
       [callback (lambda (button event)
                   (send win-notif show #f)
                   (send msg-area set-label "Game Over"))]))

(define process-msg-area
  (new message%
       [parent v-panel]
       [vert-margin 50]
       [label "You play first."]
       [min-width 250]
       [font (make-object font% 13.5 'system)]
       [auto-resize #t]))

(define (start-game-gui p1 p2 board)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send main-window show #t)
  (sleep/yield 0.05)
  (draw-board dc))

(define (game-over-processes)
  (set! game-over #t)
  (send process-msg-area set-label ""))

;;------------------ Restart Confirmation Dialog Box --------------

(define start-window
  (new dialog% (label "Start")))

(define start-msg
  (new message% (parent start-window)
       (label "Choose game mode")))

(define start-1
  (new button%
       [parent start-window]
       [label "Player vs PC"]
       [callback (lambda (button event)
                   (draw-board dc)
                   (set! game-over #f)
                   (set! 1-player #t)
                   (set! p2 player-B)
                   (send msg-area set-label "Player vs PC")
                   (send start-window show #f)
                   (send process-msg-area set-label "You play first."))]))

(define start-2
  (new button%
       [parent start-window]
       [label "Player vs Player"]
       [callback (lambda (button event)
                   (draw-board dc)
                   (set! game-over #f)
                   (set! 1-player #f)
                   (send msg-area set-label "Player vs Player")
                   (send start-window show #f)
                   (send process-msg-area set-label "You play first."))]))

(send start-window show #t)
(start-game-gui p1 p2 empty-board)