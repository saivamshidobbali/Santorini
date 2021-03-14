#lang racket

(require json)
(require racket/hash)

(define (build-list row col)
  (list (list (- row 1) (- col 1)) (list (- row 1) col) (list row (- col 1)) (list (+ row 1) col) (list row (+ col 1)) (list (+ row 1) (+ col 1))
        (list (- row 1) (+ col 1)) (list (+ row 1) (- col 1))))

(define (score space-list token player-list row col)
  (if (and (and (> row -1) (> col -1)) (and (< row 5) (< col 5)))  
      (let* ([val (list-ref (list-ref space-list row) col)])
        (cond
          [(zero? val) 0]
          [(equal? val 1) 1]
          [(equal? val 2) 2]
          [(equal? val 3) 100]
          [(equal? val 4) -2]
          [else (error 'score "invalid cell")]))
      -200))

(define (helper scores lst max_score cell ret_type)
  (cond
    [(zero? (length scores)) (if (equal? ret_type #t) (list max_score cell) cell)]
    [else  (if (> (first scores) max_score)
               (helper (rest scores) (rest lst) (first scores) (first lst) ret_type)
               (helper (rest scores) (rest lst) max_score cell ret_type))]))


(define (second-player-nearby cell player-list)
  (ormap (lambda (c) (if ( or (equal? (list-ref player-list 0) c) (equal? (list-ref player-list 1) c )) #t #f))
         (build-list (list-ref cell 0) (list-ref cell 1))))

(define (score_build space-list cell player-list)
  (let* ([val (list-ref (list-ref space-list (list-ref cell 0)) (list-ref cell 1))])    
    (cond
      [(zero? val) 0]
      [(equal? val 1) 1]
      [(equal? val 2) (if (equal? (second-player-nearby cell player-list) #t) -1 2)]
      [(equal? val 3) 3]
      [(equal? val 4) -1]
      [else (error 'score "invalid cell")])))

(define (build-feasible cell original-token token-cell player-list)
(let* ([first-player (remove original-token (list-ref player-list 0))]
       [second-player (list-ref player-list 1)])
  (if (or (list? (member cell first-player)) (list? (member cell second-player)))
      #f
      #t)))

(define (find-max-build space-list original-token token-cell player-list)
  (let* ([score_list (map (lambda (cell) (if (and
                                             (and (and (> (list-ref cell 0) -1) (> (list-ref cell 1) -1)) (and (< (list-ref cell 0) 5) (< (list-ref cell 1) 5)))
                                             (build-feasible cell original-token token-cell player-list))
                                             (score_build space-list cell player-list)
                                             -200 )) (build-list (list-ref token-cell 0) (list-ref token-cell 1)))])
    (list token-cell (helper score_list (build-list (list-ref token-cell 0) (list-ref token-cell 1)) -200 (list -1 -1) #f)))) 

(define (feasible space-list token new-loc player-list)
  (let* ([row (list-ref new-loc 0)]
         [col (list-ref new-loc 1)]
         [curr-row (list-ref token 0)]
         [curr-col (list-ref token 1)]
         [new-val (list-ref (list-ref space-list row) col)]
         [val (list-ref (list-ref space-list curr-row) curr-col)]
         [first-player (list-ref player-list 0)]
         [second-player (list-ref player-list 1)])    
    (if (or
        (or (equal? new-loc (list-ref first-player 0)) (equal? new-loc (list-ref first-player 1)))
        (or (equal? new-loc (list-ref second-player 0)) (equal? new-loc (list-ref second-player 1))))
        #f
        (< (- new-val val) 2))))


(define (rate-move space-list token player-list)
  (let* ([val (map (lambda (elem)
                     (let* ([new-row (+ (list-ref token 0) (list-ref elem 0))] [new-col (+ (list-ref elem 1) (list-ref token 1))])
                       (if (and (and (and (> new-row -1) (> new-col -1)) (and (< new-row 5) (< new-col 5)))
                                (feasible space-list token (list new-row new-col) player-list))
                           (score space-list token player-list new-row new-col)
                           -200)))
                   (build-list 0 0))])
    (define max_score_token (helper val (build-list (list-ref token 0) (list-ref token 1)) -200 (list -1 -1) #t))
    (list (list-ref max_score_token 0) (find-max-build space-list token (list-ref max_score_token 1) player-list))))

(define (make_best_move moves)
  (if (> (list-ref (list-ref moves 0) 0) (list-ref (list-ref moves 1) 0)) (cons 0 (list-ref moves 0)) (cons 1 (list-ref moves 1))))


(define (change-at l x y to)
  (for/list ([row l] [i (length l)])
    (for/list ([e row] [j (length row)])
      (if (and (= x i) (= y j))
          to
          e))))

(define (create_new_board space-list move-row move-col)
  (change-at space-list move-row move-col (list-ref (list-ref space-list move-row) move-col)))

(define (make-move space-list player-list turn)
  ;# initial cases with player-list 1 and 2 size are not implemented.
  (let* ([moves (make_best_move (map (lambda (token) (rate-move space-list token player-list)) (list-ref player-list 0)))]
         [token_pos (list-ref (list-ref (cdr moves) 1) 0)]
         [build_pos (list-ref (list-ref (cdr moves) 1) 1)]
         [token_rating (list-ref (cdr moves) 0)]
         [build_row (list-ref build_pos 0)]
         [build_col (list-ref build_pos 1)]
         [token_num (car moves)]
         [hash_map (hash 'turn turn
                         'players (swap (modify-players (change-at player-list 0 token_num token_pos) 1))
                         'spaces (if (not (equal? token_rating 100))
                                     (change-at space-list build_row build_col (+ (list-ref (list-ref space-list build_row) build_col) 1))
                                      space-list))])
    hash_map))

(define (swap players-list)
  (let* ([first-player (list-ref players-list 0)]
         [second-player (list-ref players-list 1)])
    (list second-player first-player)))

(define (modify-players player-list addend)
  (let* ([first-player-token (list-ref (list-ref player-list 0) 0)]
         [first-player-token2 (list-ref (list-ref player-list 0) 1)]
         [second-player-token (list-ref (list-ref player-list 1) 0)]
         [second-player-token2 (list-ref (list-ref player-list 1) 1)])
    (list
     (list
      (list (+ (list-ref first-player-token 0) addend) (+ (list-ref first-player-token 1) addend))
      (list (+ (list-ref first-player-token2 0) addend) (+ (list-ref first-player-token2 1) addend)))
     (list
      (list (+ (list-ref second-player-token 0) addend) (+ (list-ref second-player-token 1) addend))
      (list (+ (list-ref second-player-token2 0) addend) (+ (list-ref second-player-token2 1) addend))))))


(define (play board)
  (cond
    [(and (list? board) (equal? (length board) 0))  (list (list (list 0 0) (list 0 1)))]
    [(and (list? board) (equal? (length board) 1)) (append board (list (list (list 3 3) (list 3 2))))]
    [(and (list? board) (equal? (length board) 2)) (hash 'turn 0
                                                         'players board
                                                         'spaces (list
                                                                  (list 0 0 0 0 0)
                                                                  (list 0 0 0 0 0)
                                                                  (list 0 0 0 0 0)
                                                                  (list 0 0 0 0 0)
                                                                  (list 0 0 0 0 0)))]
    [else (make-move (hash-ref board 'spaces) (modify-players (hash-ref board 'players) -1) (+ (hash-ref board 'turn) 1))]))  

		
(define (func)
  (let* ([mp (play (read-json) )])
    (write-json mp)
    (flush-output))
  (func))

(func)