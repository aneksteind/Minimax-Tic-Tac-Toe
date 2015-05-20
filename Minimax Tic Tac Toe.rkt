;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Minimax Tic Tac Toe|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)

(define GRID-SIZE 60)
(define SPACE (rectangle GRID-SIZE GRID-SIZE 'outline 'black))
(define ROW (beside SPACE SPACE SPACE))
(define BOARD (above ROW ROW ROW))              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct board [turn compmoves playermoves remaining])
; A Board is a (make-game symbol [List-of Number] [List-of Number] [List-of Number])
; interpretation -> who's turn is it, which spaces have been played by computer,
;                   which spaces have been played by the player, and
;                   which spaces remain

;Winning combinations (order doesn't matter, 
;and this is reflected in the 'game-over?' function
(define winr1 '(8 1 6))
(define winr2 '(3 5 7))
(define winr3 '(4 9 2))
(define winc1 '(8 3 4))
(define winc2 '(1 5 9))
(define winc3 '(6 7 2))
(define wind1 '(8 5 2))
(define wind2 '(4 5 6))

(define a-win (list winr1 winr2 winr3 winc1 winc2 winc3 wind1 wind2))

(define BOARD5 (make-board 'player '(9 3 1) '(4 8 6) '(2 5 7)))
(define BOARD1 (make-board 'player '(8 6 5 9) '(1 3 7 4 2) '()))
(define BOARD2 (make-board 'player '(4 2 5 8) '(1 9 6) '(3 7)))
(define BOARD3 (make-board 'computer '(3 9 5) '(4 2 7) '(8 1 6)))
(define BOARD4 (make-board 'computer '() '(4) '(1 2 3 5 6 7 8 9)))
(define BOARD0 (make-board 'player '() '() '(1 2 3 4 5 6 7 8 9)))
(define BOARD00 (make-board 'computer '() '() '(1 2 3 4 5 6 7 8 9)))
(define BOARD6 (make-board 'player '(8 6 5 9) '(3 7 4 2) '(1)))


;Board -> Number
;calculate the minimax score for the given board
(define (score-board board)
  (local (;flattens the list
          (define (flatten se)
            (cond
              [(number? se) (list se)]
              [(empty? se) '()]
              [else (append (flatten (first se)) (flatten (rest se)))]))
          (define list-scores
            (map score-board (potential-moves board)))
          (define max/min
            (if (player-turn? board) max min)))
    (cond
      [(game-over? board) (evaluate board)]
      [else (foldr max/min
                   (first (flatten list-scores))
                   (rest (flatten list-scores)))])))

(check-expect (score-board BOARD3) -1)
(check-expect (score-board BOARD2) -1)

;Board -> Board
;gives the board with the best move
(define (best-move board)
  (local ((define argmax/min
            (if (player-turn? board) argmax argmin))
          (define player/computer_move
            (if (player-turn? board)
                board-playermoves
                board-compmoves)))
    (cond
      [(= (length (board-remaining board)) 9) (+ 1 (random 9))]
      [else (first (player/computer_move (argmax/min score-board (potential-moves board))))])))

;(check-expect (best-move BOARD4) 5)
(check-expect (best-move BOARD3) 1)


;Board -> [List-of Board]
;returns a list of potential boards
(define (potential-moves board)
  (local (;Number -> [List-of Number]
          ;takes a referenced nummber in a list
          ;and adds it to another list          
          (define (add-move n)
            (cond
              [(player-turn? board)(cons (list-ref (board-remaining board) n)
                                         (board-playermoves board))]
              [else                (cons (list-ref (board-remaining board) n)
                                         (board-compmoves board))])))
    (cond
      [(game-over? board) '()]
      [else (build-list (length (board-remaining board)) 
                        (λ (i) (make-board (next-player (board-turn board))                                   
                                           (if (not (player-turn? board))
                                               (add-move i)
                                               (board-compmoves board))
                                           (if (player-turn? board)
                                               (add-move i)
                                               (board-playermoves board))
                                           (extract i (board-remaining board)))))])))

(check-expect (potential-moves BOARD2) '())
(check-expect (potential-moves BOARD6)
              (list (make-board 'computer 
                                (list 8 6 5 9) 
                                (list 1 3 7 4 2)
                                empty)))

;Number [List-of Number] -> [List-of Number]
;returns a list without the nth term
(define (extract n xs)
  (cond
    [(= n 0)(rest xs)]
    [else (cons (first xs) (extract (sub1 n) (rest xs)))]))

(check-expect (extract 0 '(1 2 3)) '(2 3))




;Symbol -> Symbol
;changes the turn
(define (next-player s)
  (if (symbol=? s 'player)
      'computer
      'player))

(check-expect (next-player 'player) 'computer)
(check-expect (next-player 'computer) 'player)


;Board -> Number
;evaluates the board
(define (evaluate board)
  (cond
    [(empty? (board-remaining board)) 0]
    [(player-turn? board) -1]
    [(computer-turn? board) 1]))

(check-expect (evaluate BOARD1) 0)
(check-expect (evaluate BOARD2) -1)

;Board -> Boolean
;the game is over if
; - there are no more moves remaining,
; - the player has a winning combination, or
; - the computer has a winning combination
(define (game-over? board)
  (or (empty? (board-remaining board))
      (ormap (λ (win) (in-moves? win (board-compmoves board))) a-win)
      (ormap (λ (win) (in-moves? win (board-playermoves board))) a-win)))

(check-expect (game-over? BOARD1) true)
(check-expect (game-over? BOARD2) true)

;[List-of Number] [List-of Number] -> Boolean
;are the values from the first list in the second, in any order?
(define (in-moves? combo moves)
  (andmap (λ (number) (member? number moves)) combo))

(check-expect (in-moves? '(4 5 6) '(4 1 8 6 5 3 2)) true)

;Board -> Boolean
;determines if it's the player's turn
(define (player-turn? board)
  (symbol=? 'player (board-turn board)))

(check-expect (player-turn? BOARD1) true)

;Board -> Boolean
;determines if it's the computer's turn
(define (computer-turn? board)
  (symbol=? 'computer (board-turn board)))

(check-expect (computer-turn? BOARD2) false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main board)
  (big-bang BOARD0
            (on-mouse make-move)
            (to-draw render)
            (on-tick computer-turn)
            (stop-when game-over?)))

;Board -> Board
;evaluates the computer's move
(define (computer-turn board)
  (local ((define argmax/min
            (if (player-turn? board) argmax argmin))
          (define player/computer_move
            (if (player-turn? board)
                board-playermoves
                board-compmoves)))
    (cond
      [(game-over? board) board]
      [(computer-turn? board)     
       (argmin score-board (potential-moves board))]
      [else board])))


;Board Number Numbe MouseEvent
;creates a new board depending on where the player clicks
(define (make-move board x y me)
  (local (;Number String -> Boolean
          ;Is the mouse clicking close to a number
          (define (on-sq? n)
            (and (close-to? x y (gridsq n)) (string=? me "button-down")))
          ;Number Number Posn -> Boolean
          ;checks to see if an x and y position are within a space
          (define (close-to? x y p)
            (and (<= (abs (- (posn-x p) x)) (/ GRID-SIZE 2))
                 (<= (abs (- (posn-y p) y)) (/ GRID-SIZE 2))))
          
          ;Number -> Board
          ;makes a new board
          (define (new-board n)
            (make-board 'computer
                        (board-compmoves board)
                        (cons n (board-playermoves board))
                        (remove n (board-remaining board)))))
    (cond
      [(on-sq? 1)(new-board 1)]
      [(on-sq? 2)(new-board 2)]
      [(on-sq? 3)(new-board 3)]
      [(on-sq? 4)(new-board 4)]
      [(on-sq? 5)(new-board 5)]
      [(on-sq? 6)(new-board 6)]
      [(on-sq? 7)(new-board 7)]
      [(on-sq? 8)(new-board 8)]
      [(on-sq? 9)(new-board 9)]
      [else board])))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Board -> Image
;draws the current board
(define (render board)
  (foldr place-o
         (foldr place-x
                BOARD
                (board-compmoves board)) 
         (board-playermoves board)))

;Number Image -> Image
;places an 'X' onto the tic tac toe board
(define (place-x number im)
  (place-thing number im "X"))

;Number Image -> Image
;places an 'O' onto the tic tac toe board
(define (place-o number im)
  (place-thing number im "O"))

;Number Image 1String -> Image
;places an image on the tic tac toe board
(define (place-thing number im letter)
  (place-image (text letter 30 'black)
               (posn-x (gridsq number))
               (posn-y (gridsq number))
               im))

;Number -> Posn
;puts a number at a posn
(define (gridsq number)
  (cond
    [(= number 8)(grid 1 1)]
    [(= number 1)(grid 2 1)]
    [(= number 6)(grid 3 1)]
    [(= number 3)(grid 1 2)]
    [(= number 5)(grid 2 2)]
    [(= number 7)(grid 3 2)]
    [(= number 4)(grid 1 3)]
    [(= number 9)(grid 2 3)]
    [(= number 2)(grid 3 3)]))

;Number Number -> Posn
;takes 2 numbers and creates a posn-x and posn-y with
;the first and second number, respectively
(define (grid x y)
  (make-posn (* GRID-SIZE (- x .5)) (* GRID-SIZE (- y .5))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
