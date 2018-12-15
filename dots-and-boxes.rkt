#lang racket
(require racket/class racket/set compatibility/mlist lazy/force)
(provide game% interactive-player a% b% input-move empty-board start-game user-A user-B player-A player-B
         cur-board as bs my-turn sticks end-game?)

;; флаг хода первого игрока (gui)
(define my-turn #t)

;;--------------------------------------------------------------------
;; Реализация минимакса с альфа-бета отсечением
(define (minimax tree)
  (define (minimax-h node alpha beta max-player)
    (define (next-max x v)
      (if (or (null? x) (<= beta v)) 
                v
                (next-max (cdr x)
                      (max v (minimax-h (car x) v beta (not max-player)))))
    )
    (define (next-min x v)
      (if (or (null? x) (<= v alpha)) 
                v
                (next-min (cdr x)
                      (min v (minimax-h (car x) alpha v (not max-player)))))
    )
    (cond 
         ((number? node) node)
         ((null? node) 0.0)
         (max-player (next-max node alpha))
         (else (next-min node beta)))
          
  )
  (!(minimax-h tree -inf.0 +inf.0 #f))
)

;; получить все ячейки игровой доски, не занятые ходами одного из игроков 
(define (open-4-cells as/bs b)
  (set-subtract (free-sticks b) (as/bs b)))

;; получить количество квадратов, которые можно составить при помощи определенной палочки
(define (count-open-4 l)
   (foldl (lambda (x y) (if (subset? (list->set x) l) (+ 1 y) y)) 0 (mlist->list squares-positions)))
 
;; функция эвристической оценки позиции
;; из количества квадратов для первого игрока вычитается количество квадратов второго игрока
(define (f-h s)
  (- (count-open-4 (open-4-cells as s)) (count-open-4 (open-4-cells bs s)))
)  

;; функция старта игры через консоль
(define (start-game p1 p2 initial-state)
  (set-field! opponent p1 p2)
  (set-field! opponent p2 p1)
  (send p1 your-turn initial-state))

(define game%
  (class object%
    (super-new)
    (init-field my-win?         ; State -> Bool
                my-loss?        ; State -> Bool
                my-move         ; State Move -> State
                opponent-move   ; State Move -> State
                possible-moves  ; State -> (list Move)
                show-state)     ; State -> Any
                
   
    ;; optimal-move :: State -> Move
    ;; выбор оптимального хода по минимаксу 
    ;; из нескольких оптимальных выбирается один случайно
    (define/public ((optimal-move look-ahead) S)
      (!(argmax (lambda (m) (!(minimax (game-tree S m look-ahead))))
                 (shuffle (possible-moves S)))))

    ;; game-tree :: State -> (Move -> (Tree of Real))
    ;; построение дерева с оценками
    (define (game-tree St m look-ahead)
      ;; вспомогательная функция, строящая закольцованный список из пары элементов
      (define (help a b) (begin (define l (mlist a b a)) (set-mcdr! l (mcons b l)) l))
      (define (new-ply moves i s)	  
        (cond
          ((my-win? s) +inf.0) ; в выигрышной позиции оценка = + бесконечность
          ((my-loss? s) -inf.0) ; в проигрышной = - бесконечность
          ((>= i look-ahead)  (f-h s)) ; если исчерпана глубина, то используется эвристическая оценка 
          (else (map (lambda (x) (new-ply (mcdr moves) (+ 1 i) ((mcar moves) s x #f)))
                     (possible-moves s))) ; рассматриваем все возможные ходы и строим их оценки
		))
     (new-ply (help opponent-move my-move) 1 (my-move St m #f))
    )

    (define/public (make-move S move)
      (if (my-loss? S)   (values '() S 'loss) 
        (let* ((m* (!(move S)))
              (S* (my-move S m* #t)))
          (cond ((end-game? S*) (values m* S* 'end))
                ((my-win? S*) (values m* S* 'win))
                (else (values m* S* 'next)))
	)))
  ))

; State -> Bool  ;; окончание игры - все палочки расставлены и клетки заполнены
(define (end-game? s)
  ;; случай, когда клеток у одного из игроков больше половины - досрочная победа
  (or (>(set-count (as s)) 12) (> (set-count (bs s)) 12) (set-empty? (free-sticks s))))

(define (interactive-player game)
  (class game
    (super-new)
 
    (inherit-field show-state)
    (inherit make-move optimal-move)
    
    (init-field name
                [last-move '(0 0 0)] ;; последний ход игрока (gui)
                [gui #f]       ;; идентификатор игрока-пользователя (ввод хода осуществляется через gui)
                [look-ahead 1]
                [move-method (optimal-move look-ahead)]
                [opponent 'undefined]
                )

    ;; сеттер последнего хода игрока (используется в gui)
    (define/public (set!-value move)
      (set! last-move move))
    ;; геттер последнего хода игрока
    (define/public (get-move) last-move)
 
    (define/public (your-turn S)
      (define-values (m S* status) (make-move S (if gui (lambda(x) last-move) move-method)))
      (! (set! cur-board S*))
      (! (set! last-move m))
      (! (printf "\n~a makes move ~a\n" name m))
      (! (show-state S*))
      (! (case status
           ['stop (displayln "The game was interrupted.")]
           ['end (check-win S*)]
           ['win  (printf "~a wins!" name)]
           ['loss (printf "~a wins!" name)]
           [else
            ;; определяем кому достается следующий ход
            (let ((sq (is-square? S* squares-positions)))
              (if sq
                  (begin
                    (set! squares-positions (list->mlist (remove sq (mlist->list squares-positions))))
                    ;(send this your-turn S*)) ;; раскомментить для режима без gui
                  )
                  (if my-turn
                      (set! my-turn #f)
                      (set! my-turn #t))
                  ;(send opponent your-turn S*) ;; раскомментить для режима без gui
                  ))]
           )))))

;;--------------------------------------------------------------------
;; макрос для описания партнеров в игре
(define-syntax-rule 
  (define-partners game (A #:win A-wins #:move A-move) 
                        (B #:win B-wins #:move B-move))
  (begin
    (define A (class game 
                (super-new 
                 [my-win?  A-wins]
                 [my-loss? B-wins] 
                 [my-move  A-move]
                 [opponent-move B-move])))
    (define B (class game 
                (super-new 
                 [my-win?  B-wins]
                 [my-loss? A-wins] 
                 [my-move  B-move]
                 [opponent-move A-move])))))

;; вывод игровой доски в текстовом виде
(define (show-board b)
  (displayln "  0 1 2 3 4 5   ")
  (for ([i '(0 1 2 3 4)])
    (printf "~a " i)
    (for ([j '(0 1 2 3 4)])
      (display "*")
      (if (set-member? (sticks b) (list i j 0))(printf "-")(printf " ")))
    (display "*\n")
    ;; второй ряд
    (printf "  ")
    (for ([j '(0 1 2 3 4)])
      (if (set-member? (sticks b) (list i j 1))(printf "|")(printf " "))
      (cond ((set-member? (as b) (list i j 0)) (printf "A"))
            ((set-member? (bs b) (list i j 0)) (printf "B"))
            (else (printf " ")))
      )
    (if (set-member? (sticks b) (list i 5 1))(printf "|")(printf " "))
    (printf "\n")
    )
  (printf "5 ")
  (for ([j '(0 1 2 3 4)])
    (display "*")
    (if (set-member? (sticks b) (list 5 j 0))(printf "-")(printf " ")))
  (display "*\n")
  )

;; доска задается при помощи палочек и букв первого и второго игрока
(struct board (sticks a b))

;; геттеры полей структуры доски
(define sticks board-sticks)
(define as board-a)
(define bs board-b)

;; начальное состояние доски
(define empty-board (board (set) (set) (set)))

;; объект доски для взаимодействия между игроками (необходим при использовании gui)
(define cur-board empty-board)

;; все возможные ходы игрока
;; первые два числа - координаты левого верхнего угла
;; третье: 0 - горизонтальная палка, 1 - вертикальная палка 
(define all-sticks
  ;; горизонтальные палки 
  (set '(0 0 0) '(0 1 0) '(0 2 0) '(0 3 0) '(0 4 0)
       '(1 0 0) '(1 1 0) '(1 2 0) '(1 3 0) '(1 4 0)
       '(2 0 0) '(2 1 0) '(2 2 0) '(2 3 0) '(2 4 0)
       '(3 0 0) '(3 1 0) '(3 2 0) '(3 3 0) '(3 4 0)
       '(4 0 0) '(4 1 0) '(4 2 0) '(4 3 0) '(4 4 0)
       '(5 0 0) '(5 1 0) '(5 2 0) '(5 3 0) '(5 4 0)
  ;; вертикальные палки
       '(0 0 1) '(0 1 1) '(0 2 1) '(0 3 1) '(0 4 1) '(0 5 1)
       '(1 0 1) '(1 1 1) '(1 2 1) '(1 3 1) '(1 4 1) '(1 5 1)
       '(2 0 1) '(2 1 1) '(2 2 1) '(2 3 1) '(2 4 1) '(2 5 1)
       '(3 0 1) '(3 1 1) '(3 2 1) '(3 3 1) '(3 4 1) '(3 5 1)
       '(4 0 1) '(4 1 1) '(4 2 1) '(4 3 1) '(4 4 1) '(4 5 1)
       ))

;; клетки, которые заполняются метками игрока
;; клетка однозначно задается своим левым верхним углом
(define all-cells
  (set '(0 0 0) '(0 1 0) '(0 2 0) '(0 3 0) '(0 4 0)
       '(1 0 0) '(1 1 0) '(1 2 0) '(1 3 0) '(1 4 0)
       '(2 0 0) '(2 1 0) '(2 2 0) '(2 3 0) '(2 4 0)
       '(3 0 0) '(3 1 0) '(3 2 0) '(3 3 0) '(3 4 0)
       '(4 0 0) '(4 1 0) '(4 2 0) '(4 3 0) '(4 4 0)
       ))

;; множество свободных палочек (доступных для хода)
(define (free-sticks b)
  (set-subtract all-sticks (sticks b)))

;; всевозможные комбинации палочек, образующих квадрат
(define squares-positions
  (mlist (list '(0 0 0) '(0 0 1) '(0 1 1) '(1 0 0)) (list '(0 1 0) '(0 1 1) '(0 2 1) '(1 1 0))
        (list '(0 2 0) '(0 2 1) '(0 3 1) '(1 2 0)) (list '(0 3 0) '(0 3 1) '(0 4 1) '(1 3 0))
        (list '(0 4 0) '(0 4 1) '(0 5 1) '(1 4 0)) (list '(1 0 0) '(1 0 1) '(1 1 1) '(2 0 0))
        (list '(1 1 0) '(1 1 1) '(1 2 1) '(2 1 0)) (list '(1 2 0) '(1 2 1) '(1 3 1) '(2 2 0))
        (list '(1 3 0) '(1 3 1) '(1 4 1) '(2 3 0)) (list '(1 4 0) '(1 4 1) '(1 5 1) '(2 4 0))

        (list '(2 0 0) '(2 0 1) '(2 1 1) '(3 0 0)) (list '(2 1 0) '(2 1 1) '(2 2 1) '(3 1 0))
        (list '(2 2 0) '(2 2 1) '(2 3 1) '(3 2 0)) (list '(2 3 0) '(2 3 1) '(2 4 1) '(3 3 0))
        (list '(2 4 0) '(2 4 1) '(2 5 1) '(3 4 0)) (list '(3 0 0) '(3 0 1) '(3 1 1) '(4 0 0))
        (list '(3 1 0) '(3 1 1) '(3 2 1) '(4 1 0)) (list '(3 2 0) '(3 2 1) '(3 3 1) '(4 2 0))
        (list '(3 3 0) '(3 3 1) '(3 4 1) '(4 3 0)) (list '(3 4 0) '(3 4 1) '(3 5 1) '(4 4 0))

        (list '(4 0 0) '(4 0 1) '(4 1 1) '(5 0 0)) (list '(4 1 0) '(4 1 1) '(4 2 1) '(5 1 0))
        (list '(4 2 0) '(4 2 1) '(4 3 1) '(5 2 0)) (list '(4 3 0) '(4 3 1) '(4 4 1) '(5 3 0))
        (list '(4 4 0) '(4 4 1) '(4 5 1) '(5 4 0))))
        
;; предикат, проверяющий образование квадрата из имеющихся ходов
(define (is-square? s squares)
  (ormap (lambda (x) (if (andmap (lambda (y) (member y (set->list (sticks s)))) x) x #f)) (mlist->list squares)))

;; определение победившего игрока
(define (check-win b)
  (let ((a-count (set-count (as b)))
        (b-count (set-count (bs b))))
    (begin
      (displayln "\nGame is over!")
      (show-board b)
      (if (> a-count b-count)
          (printf "User A wins!")
          (printf "User B wins"))))) 

;; ход игрока А (с учетом возможного образования сразу двух квадратов)
(define (a-move b m remove_flag)
  (let* ((S* (board (set-add (sticks b) m) (as b) (bs b)))
         (sq (is-square? S* squares-positions)))
    (if sq
        (let* ((S** (board (set-add (sticks b) m) (set-add (as b)  (list-ref sq 0)) (bs b)))
              (sq* (is-square? S** (list->mlist (remove sq (mlist->list squares-positions))))))
          (if sq*
              (begin
                (cond (remove_flag (set! squares-positions (list->mlist (remove sq (mlist->list squares-positions))))))
                (board (set-add (sticks b) m) (set-union (as b)  (set (list-ref sq 0) (list-ref sq* 0))) (bs b)))
              S**))
        S*)))

;; ход игрока В (с учетом возможного образования сразу двух квадратов)
(define (b-move b m remove_flag)
  (let* ((S* (board (set-add (sticks b) m) (as b) (bs b)))
        (sq (is-square? S* squares-positions)))
    (if sq
        (let* ((S** (board (set-add (sticks b) m) (as b) (set-add (bs b) (list-ref sq 0))))
              (sq* (is-square? S** (list->mlist (remove sq (mlist->list squares-positions))))))
          (if sq*
              (begin
                (cond (remove_flag (set! squares-positions (list->mlist (remove sq (mlist->list squares-positions))))))
                (board (set-add (sticks b) m) (as b) (set-union (bs b) (set (list-ref sq 0) (list-ref sq* 0))) ))
              S**))
        S*)))

;; проверка, является ли ситуация на игровой доске выигрышной
(define ((wins? s-my s-op) b)
  (or (> (set-count (s-my b)) 12) (and (set-empty? (free-sticks b)) (> (set-count (s-my b)) (set-count (s-op b))))))

;; экземпляр класса игры dots and boxes
(define dots-and-boxes%
  (class game%
    (super-new
     [possible-moves   (compose set->list free-sticks)]
     [show-state       show-board])))

;; описания партнеров для крестиков-ноликов
(define-partners dots-and-boxes%
  (a% #:win (wins? as bs) #:move a-move)
  (b% #:win (wins? bs as) #:move b-move))

;; объекты-игроки, принимающие ввод пользователя
;; проверка ввода частичная
(define (input-move m)
            (case m
              ('q (exit))
              ('(0 0 0) m) ('(0 1 0) m) ('(0 2 0) m) ('(0 3 0) m) ('(0 4 0) m)
              ('(1 0 0) m) ('(1 1 0) m) ('(1 2 0) m) ('(1 3 0) m) ('(1 4 0) m)
              ('(2 0 0) m) ('(2 1 0) m) ('(2 2 0) m) ('(2 3 0) m) ('(2 4 0) m)
              ('(3 0 0) m) ('(3 1 0) m) ('(3 2 0) m) ('(3 3 0) m) ('(3 4 0) m)
              ('(4 0 0) m) ('(4 1 0) m) ('(4 2 0) m) ('(4 3 0) m) ('(4 4 0) m)
              ('(5 0 0) m) ('(5 1 0) m) ('(5 2 0) m) ('(5 3 0) m) ('(5 4 0) m)

              ('(0 0 1) m) ('(0 1 1) m) ('(0 2 1) m) ('(0 3 1) m) ('(0 4 1) m) ('(0 5 1) m)
              ('(1 0 1) m) ('(1 1 1) m) ('(1 2 1) m) ('(1 3 1) m) ('(1 4 1) m) ('(1 5 1) m)
              ('(2 0 1) m) ('(2 1 1) m) ('(2 2 1) m) ('(2 3 1) m) ('(2 4 1) m) ('(2 5 1) m)
              ('(3 0 1) m) ('(3 1 1) m) ('(3 2 1) m) ('(3 3 1) m) ('(3 4 1) m) ('(3 5 1) m)
              ('(4 0 1) m) ('(4 1 1) m) ('(4 2 1) m) ('(4 3 1) m) ('(4 4 1) m) ('(4 5 1) m)
              (else (input-move (read)))))

;; первый игрок-пользователь с ожиданием ввода хода				   
(define user-A 
  (new (force (interactive-player a%)) 
       [name "User A"]
       [gui #t]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
)

;; второй игрок-пользователь с ожиданием ввода хода
(define user-B 
  (new (force (interactive-player b%)) 
       [name "User B"]
       [gui #t]
       [move-method 
        (lambda (b) (input-move (read)))]
		)
)

;; ИИ-игроки
(define player-A (new (force (interactive-player a%))
                      [name "A"]
                      [look-ahead 2]))
(define player-B (new (force (interactive-player b%))
                      [name "B"]
                      [look-ahead 2]))
