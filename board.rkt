#lang racket
(require racket/gui racket/draw "dots-and-boxes.rkt")

;; размер доски (5х5)
(define n 5)
;; режим 1-player = #t - пользователь и ИИ
;;       1-player = #f - пользователь #1 и пользователь #2
(define 1-player #f)

;; по умолчанию оба игрока - пользователи
(define p1 user-A)
(define p2 user-B)

;; флаг окончания игры
(define game-over #f)

;; последний сделанный ход в игре
(define last-played-pos '(0 0 0))


;; основное окно
(define main-window (new frame%
                         [label "Dots and Boxes"]
                         [min-width 700]
                         [min-height 500] 
                         [stretchable-width #f]
                         [stretchable-height #f]))

;; часть основного окна с информацией о состоянии игры
(define h-panel (new horizontal-panel% (parent main-window)))

;; часть основного окна с игровым полем
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

;; cоздадим черную и красную "ручку"
(define red-pen (make-object pen% "RED" 2 'solid))
(define black-pen (make-object pen% "BLACK" 2 'solid))

;; функция отрисовки игрового поля 5х5 красным цветом
(define (draw-board dc)
  (send dc set-pen red-pen)    
  (for ((y n))
    (for ((x n))
      (send dc draw-rectangle
            (+ 125 (* 50 x))
            (+ 125 (* 50 y))
            50 50))))

;; геттер для drawing context
(define dc (send gui-board get-dc))

;; функция обработки хода
;; x и y - координаты нажатия мышкой
(define (mark x y)
  
  ;; получим ориентацию палки
  ;; 0 - горизонтальная, 1 - вертикальная
  (define (stick-rotate x)
    (let ((r (remainder (- x 25) 50)))
      (if (and (> r 10) (< r 40))
          0 1)))
  
  ;; вспомогательные переменные и функции для отрисовки содержимого в клетках
  ;; Х - метка первого игрока, О - метка второго игрока
  (define corner-x 0)
  (define corner-y 0)
  (define (cross) ;; отрисовка X
    (send dc draw-line (+ 15 corner-x) (+ 15 corner-y) (+ 35 corner-x) (+ 35 corner-y))
    (send dc draw-line (+ 15 corner-x) (+ 35 corner-y) (+ 35 corner-x) (+ 15 corner-y)))
  (define (circle) ;; отрисовка О
    (send dc draw-ellipse (+ 15 corner-x) (+ 15 corner-y) 20 20))

  (define (display-letters) ;; основная отрисовка
    (for ((y n))
      (for ((x n))
        (set! corner-x (+ 125 (* x 50)))
        (set! corner-y(+ 125 (* y 50)))
        (cond ((set-member? (as cur-board) (list y x 0)) (cross))
              ((set-member? (bs cur-board) (list y x 0)) (circle)) ))))

  ;; отрисовка палочек (сделанных ходов)
  (define (display-sticks)
    (let ((s (sticks cur-board)))
      (map (lambda(x) (one-stick (list-ref x 0) (list-ref x 1) (list-ref x 2))) (set->list s))))

  ;; отрисовка одной палочки по координатам хода
  (define (one-stick x y z)
    (let ((x1 (+ 125 (* y 50)))
          (y1 (+ 125 (* x 50)))
          (x2 (if (= z 0) (+ 175 (* y 50)) (+ 125 (* y 50))))
          (y2 (if (= z 0) (+ 125 (* x 50)) (+ 175 (* x 50)))))
      (send dc draw-line x1 y1 x2 y2)))

  ;; проверим сделан ли ход в рамках игрового поля
  (define (possible-move? x y)
    (and (> x 115) (< x 385) (> y 115) (< y 385)))

  ;; проверим был ли сделан этот ход ранее
  (define (make-turn-early? pos)
    (set-member? (sticks cur-board) pos))

  ;; основная функция обработки хода
  (define (make-turn)

    ;; получим по координатам хода координаты палочки из all-sticks (dots-and-boxes.rkt)
    (define stick-y (floor (/ (- x 115) 50)))
    (define stick-x (floor (/ (- y 115) 50)))
    (define stick-z (stick-rotate x))

    ;; изменим состояние последнего хода в игре
    (set! last-played-pos (list stick-x stick-y stick-z))

    ;; ход ИИ игрока
    (define (ai-move)
      (if (and (not my-turn) (not (end-game? cur-board)))
          (begin (send process-msg-area set-label "Second player's turn: O \n Thinking...")
                 (send p2 your-turn cur-board)
                 (send process-msg-area set-label "Second player's turn: O"))
          (void)))
    
    (cond ((and (not game-over) (not (make-turn-early? last-played-pos)))
           (begin
             (send dc set-pen black-pen)
             (one-stick stick-x stick-y stick-z)
             (cond ;; ход первого игрока, когда играют два пользователя
                   ((and my-turn (not 1-player)) 
                    (begin
                      (displayln "My turn")
                      (send p1 set!-value last-played-pos)
                      (send p1 your-turn cur-board)
                      (send process-msg-area set-label "First player's turn: X")))
                   ;; ход второго игрока, когда играют два пользователя
                   ((and (not my-turn) (not 1-player)) 
                    (begin
                      (displayln "Not my turn")
                      (send p2 set!-value last-played-pos)
                      (send p2 your-turn cur-board)
                      (send process-msg-area set-label "Second player's turn: O")))
                   ;; ход пользователя в игре пользователь и ИИ
                   ((and 1-player my-turn)
                    (begin
                      (displayln "My turn")
                      (send p1 set!-value last-played-pos)
                      (send p1 your-turn cur-board)
                      (send process-msg-area set-label "First player's turn: X")
                      (display-sticks)
                      (for ((i 10))
                        (ai-move))
                      ))
                   )
             
             ;; проверка окончания игры
             (if (end-game? cur-board)
                 (begin
                   (set! game-over #t)
                   (send process-msg-area set-label "")
                   (send win-msg set-label (string-append "Player " (number->string (if my-turn 1 2)) " has won!"))
                   (send win-notif show #t))
                 (void))
             (display-letters)
             (display-sticks)))
          ;; cлучай, когда игрок сходил на уже имеющуюся на доске палочку
          ((make-turn-early? last-played-pos) (send process-msg-area set-label "This turn is already made!"))
    ))
  
  (if (possible-move? x y)
      (make-turn)
      (send process-msg-area set-label "Make correct turn!"))
  )

;; функция старта игры
(define (start-game-gui p1 p2)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send main-window show #t)
  (sleep/yield 0.05)
  (draw-board dc))

(define v-panel (new vertical-panel% (parent h-panel)))

(define msg-area (new message%
                      [parent v-panel]
                      [vert-margin 50]
                      [label "Welcome"]
                      [min-width 250]
                      [font (make-object font% 13.5 'system)]
                      [auto-resize #t]))

(define process-msg-area (new message%
                              [parent v-panel]
                              [vert-margin 50]
                              [label "You play first."]
                              [min-width 250]
                              [font (make-object font% 13.5 'system)]
                              [auto-resize #t]))

;;------------------ Окно окончания игры --------------


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


;;------------------ Cтартовое окно --------------

(define start-window
  (new dialog% (label "Start")))

(define start-msg
  (new message% (parent start-window)
       (label "Choose game mode")))
;; выбор режима "пользователь и ИИ"
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

;; выбор режима "пользователь и пользователь"
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
(start-game-gui p1 p2)