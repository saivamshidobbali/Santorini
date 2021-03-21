#lang racket

(require json)
;#################################################################################
;# helper functions
;#################################################################################

(define (proc p)
(car (cdr p)))

(define (fetch2D lst row col)
  (list-ref (list-ref lst row) col))

(define (fetch lst row)
  (list-ref lst row))

(define (build-list row col)
  (list (list (- row 1) (- col 1)) (list (- row 1) col) (list row (- col 1)) (list (+ row 1) col) (list row (+ col 1)) (list (+ row 1) (+ col 1))
        (list (- row 1) (+ col 1)) (list (+ row 1) (- col 1))))

(define (build-list-artemis row col)
  (list (list (- row 1) (- col 1)) (list (- row 1) col) (list row (- col 1)) (list (+ row 1) col) (list row (+ col 1)) (list (+ row 1) (+ col 1))
        (list (- row 1) (+ col 1)) (list (+ row 1) (- col 1))
		
	(list (- row 2) (- col 2)) (list (- row 2) (- col 1)) (list (- row 2) col) (list (- row 2) (+ col 1))
	(list (- row 2) (+ col 2)) 
		
	(list (- row 1) (- col 2)) (list (- row 1) (+ col 2))
        (list row (- col 2)) (list row (+ col 2))
        (list (+ row 1) (- col 2)) (list (+ row 1) (+ col 2))
		
	(list (+ row 2) (+ col 2)) (list (+ row 2) (+ col 1)) (list (+ row 2) col) (list (+ row 2) (- col 1))
	(list (+ row 2) (- col 2))))

(define (identityFunction elem) elem)

(define (FindMaxTokenMove scores neighbors_list)
  (let* ([max_score (argmax identityFunction scores)])
    (list max_score (fetch neighbors_list (index-of scores max_score)))))

(define (FindMaxBuildSpace scores neighbors_list)
  (let* ([max_score (argmax identityFunction scores)])
    (fetch neighbors_list (index-of scores max_score))))

(define (second-player-nearby cell player-list)
  (ormap (lambda (c) (if ( or (equal? (fetch player-list 0) c) (equal? (fetch player-list 1) c )) #t #f))
         (build-list (fetch cell 0) (fetch cell 1))))

(define (change-at l x y to)
  (for/list ([row l] [i (length l)])
    (for/list ([e row] [j (length row)])
      (if (and (= x i) (= y j))
          to
          e))))
 
(define (create_new_board space-list move-row move-col)
  (change-at space-list move-row move-col (fetch2D space-list move-row move-col)))


;#################################################################
;# returns score for a token move
;#################################################################
(define (ScoreTokenMove space-list token player-list row col)
  (if (and (and (> row -1) (> col -1)) (and (< row 5) (< col 5)))  
      (let* ([val (fetch2D space-list row col)])
        (cond
          [(zero? val) 0]
          [(equal? val 1) 1]
          [(equal? val 2) 2]
          [(equal? val 3) 100]
          [(equal? val 4) -2]
          [else (error 'score "invalid cell")]))
      -200))

;#################################################################################
;# score_build
;# returns score for building cell in space-list
;#################################################################################
(define (ScoreBuildMove space-list cell player-list)
  (let* ([val (fetch2D space-list (fetch cell 0) (fetch cell 1))])    
    (cond
      [(zero? val) 0]
      [(equal? val 1) 1]
      [(equal? val 2) (if (equal? (second-player-nearby cell player-list) #t) -1 2)]
      [(equal? val 3) 3]
      [(equal? val 4) -1]
      [else (error 'score "invalid cell")])))

;#################################################################################
;# checks whether passed space is Valid to build.
;#################################################################################
(define (IsValidBuildSpace space original-token token-cell player-list)
(let* ([first-player (remove original-token (fetch player-list 0))]
       [second-player (fetch player-list 1)])
  (if (or (list? (member space first-player)) (list? (member space second-player)))
      #f
      #t)))

;#################################################################################
;# find-max-build
;# returns (list token_move build_move)
;#################################################################################
(define (FindBuildSpace space-list original-token token-cell player-list neighbors-list)
  (let* ([score_list (map (lambda (cell) (if (and
                                             (and (and (> (fetch cell 0) -1) (> (fetch cell 1) -1)) (and (< (fetch cell 0) 5) (< (fetch cell 1) 5)))
                                             (IsValidBuildSpace cell original-token token-cell player-list))
                                             (ScoreBuildMove space-list cell player-list)
                                             -200 )) neighbors-list)])
    (list token-cell (FindMaxBuildSpace score_list neighbors-list)))) 

;#################################################################################
;# checks whether the passed token move is a possible move
;#################################################################################
(define (IsValidTokenMove space-list token new-loc player-list)
  (let* ([row (fetch new-loc 0)]
         [col (fetch new-loc 1)]
         [curr-row (fetch token 0)]
         [curr-col (fetch token 1)]
         [new-val (fetch2D space-list row col)]
         [val (fetch2D space-list curr-row curr-col)]
         [first-player (fetch player-list 0)]
         [second-player (fetch player-list 1)])    
    (if (or
        (or (equal? new-loc (fetch first-player 0)) (equal? new-loc (fetch first-player 1)))
        (or (equal? new-loc (fetch second-player 0)) (equal? new-loc (fetch second-player 1))))
        #f
        (< (- new-val val) 2))))

;#################################################################################
;# rate-move
;# return (pair token_move_score (list token_move build_move))
;#################################################################################
(define (RateTokenAndBuildMoves space-list token player-list neighbors-list)
  (let* ([token_scores (map (lambda (elem)
                     (let* ([new-row (fetch elem 0)] [new-col (fetch elem 1)])
                       (if (and (and (and (> new-row -1) (> new-col -1)) (and (< new-row 5) (< new-col 5)))
                                (IsValidTokenMove space-list token (list new-row new-col) player-list))
                           (ScoreTokenMove space-list token player-list new-row new-col)
                           -200)))
                   neighbors-list)])
    (define max_score_token (FindMaxTokenMove token_scores neighbors-list))
    (cons (fetch max_score_token 0) (list (FindBuildSpace space-list token (fetch max_score_token 1) player-list
                                                          (build-list (fetch (fetch max_score_token 1) 0) (fetch (fetch max_score_token 1) 1)))))))

;#################################################################################
;# make_best_move
;# (token_num token_score (token_move build_move)
;#################################################################################
(define (make_best_move moves)
  (if (> (car (fetch moves 0)) (car (fetch moves 1))) (cons 0 (fetch moves 0)) (cons 1 (fetch moves 1))))

;#################################################################################
;# rate_token_swap_moves (Apollo card)
;# returns (pair token_num (pair token_move_score (list token_move build_move)))
;#################################################################################
(define (RateTokenSwapMoves space-list cards player-list)
  (for*/list ([first-player (fetch player-list 0)]
              [second-player (fetch player-list 1)])
    (if (and
         (< (abs(- (fetch first-player 0) (fetch second-player 0))) 2)
         (< (abs(- (fetch first-player 1) (fetch second-player 1))) 2))
        (cons (index-of (fetch player-list 0) first-player)
              (cons (ScoreTokenMove space-list first-player player-list (fetch second-player 0) (fetch second-player 1))
                    (list (FindBuildSpace space-list second-player second-player player-list (build-list (fetch second-player 0) (fetch second-player 1))))))
        (cons -200 (cons -1 -1)))))

;#################################################################################
(define (op_token_possible_moves op_num player-list)
   (ormap (lambda (cell) (and
                         (= (member (fetch player-list 1)) #f)
                         (= (member (fetch player-list 0)) #f))) (build-list (fetch (fetch2D player-list 1 op_num) 0) (fetch (fetch2D player-list 1 op_num) 1))))

(define (push_to_adjacent_cells op_num player-list)
   (let*
       ([lst (build-list (fetch (fetch2D player-list 1 op_num) 0) (fetch (fetch2D player-list 1 op_num) 1))])
      (fetch lst (index-of (map (lambda (cell) (and
                         (= (member (fetch player-list 1)) #f)
                         (= (member (fetch player-list 0)) #f))) lst) #t))))

(define (neighboring_op token_num player-list)
  (filter (lambda (op) (and
                       (< (abs (- (fetch (fetch2D player-list 0 token_num) 0) (fetch op 0))) 1)
                       (< (abs (- (fetch (fetch2D player-list 0 token_num) 1) (fetch op 1))) 1))) (fetch player-list 1)))

(define (custom_max elem)
  (fetch elem 0))

(define (IsValid? row col player-list)
(if
   (or
    (or (or (< row -1) (< col -1)) (or (> row 4) (> col 4)))
     (or (not (= (member (list row col) (first player-list)) #f))
       (not (= (member (list row col) (second player-list) #f)))))
     #f
     #t))

(define (positive_cell c)
  (and (> (first c) 0) (> (second c) 0)))
;#################################################################################
;# make-card-based-moves
;# makes moves based on players card.
;#################################################################################
(define (make-card-based-moves space-list cards player-list turn)
  (let* ([card (list-ref cards 0)])
    (cond
      [(equal? card "Apollo") (let* ([move (make_best_move (map (lambda (token)
                                                                  (RateTokenAndBuildMoves space-list token player-list (build-list (fetch token 0) (fetch token 1)))) (fetch player-list 0)))]
                                     [token_swap_moves (RateTokenSwapMoves space-list cards player-list)] 
                                     [move (argmax proc (cons move token_swap_moves))])
                                (if (list? (member (fetch player-list 1) (fetch2D move 2 0)))
                                    (make_move move cards space-list
                                               (list (car player-list)
                                                     (append (remove (fetch (fetch move 2) 0) (fetch player-list 1)) (fetch2D player-list 0 (car move)))
                                                     turn 1))
                                    (make_move move cards space-list player-list turn 1)))]
      [(equal? card "Artemis")  (let* ([move (make_best_move (map (lambda (token)
                                                                    (RateTokenAndBuildMoves space-list token player-list (build-list-artemis (fetch token 0) (fetch token 1)))) (fetch player-list 0)))])
                                  (make_move move cards space-list player-list turn 1))]
      [(equal? card "Atlas")
                                ;# need to move 
                                (let*
                                    ([move (make_best_move (map (lambda (token)
                                                                    (RateTokenAndBuildMoves space-list token player-list (build-list (fetch token 0) (fetch token 1)))) (fetch player-list 0)))]
                                    [token_move (first (third move))]   
                                    [op_moves (map (lambda (token) (RateTokenAndBuildMoves space-list token (swap player-list) (build-list (fetch token 0) (fetch token 1)))) (fetch player-list 1))]
                                    [tokens_neighbors (build-list (first token_move) (second token_move))])

                                  (cond
                                   [(and
                                    (= (first (first op_moves)) 100)
                                    (number? (index-of tokens_neighbors (first (second (first op_moves))))))
                                                                    (make_move (list (first move) (second move) (list (first (third move)) (first (second (first op_moves)))))
                                                                               cards
                                                                               space-list
                                                                               player-list
                                                                               turn
                                                                               4)]
                                   [(and
                                    (= (first (second op_moves)) 100)
                                    (number? (index-of tokens_neighbors (first (second (second op_moves))))))
                                                     (make_move (list (first move) (second move) (list (first (third move)) (first (second (second op_moves)))))
                                                                               cards
                                                                               space-list
                                                                               player-list
                                                                               turn
                                                                               4) ]
                                   [else (make_move move cards space-list player-list turn 1)]))]                                                                       
                               
                               
      [(equal? card "Demeter")
                                (let* ([move (make_best_move (map (lambda (token)
                                                                    (RateTokenAndBuildMoves space-list token player-list (build-list (fetch token 0) (fetch token 1)))) (fetch player-list 0)))]
                                      [token_move (fetch (third move) 0)]
                                      [build_move (fetch (third move) 1)]
                                      [token_rating (second move)]
                                      [token_num (car move)]
                                      [second_build_move (fetch (FindBuildSpace space-list (fetch (fetch player-list 0) token_num) token_move player-list
                                                                         (remove build_move (build-list (fetch token_move 0)
                                                                                                        (fetch token_move 1)))) 1)]
                                      [second_build_row (fetch second_build_move 0)]
                                      [second_build_col (fetch second_build_move 1)])

                                      (if (equal? token_rating 100)
                                          (make_move move cards space-list player-list turn 1)
                                          (make_move move cards (change-at space-list second_build_row second_build_col (+ (fetch2D space-list second_build_row second_build_col) 1))
                                                 player-list turn 1)))]
      [(equal? card "Hephastus")  (let* ([move (make_best_move (map (lambda (token)
                                                                      (RateTokenAndBuildMoves space-list token player-list (build-list (fetch token 0) (fetch token 1))))
                                                                    (fetch player-list 0)))]
                                         [build_move (fetch2D move 2 1)])

                                         (if (< (fetch2D space-list (fetch build_move 0) (fetch build_move 1)) 2)
                                             (make_move move cards space-list player-list turn 2)
                                             (make_move move cards space-list player-list turn 1)))] 
      
      [(equal? card "Minotaur") (let* ([move (make_best_move (map (lambda (token)
                                                                    (RateTokenAndBuildMoves space-list token player-list
                                                                                            (build-list-artemis (fetch token 0) (fetch token 1)))) (fetch player-list 0)))]
                                       [neighbors_with_score
                                         (map (lambda (cell) (list
                                                              (ScoreTokenMove space-list empty player-list (fetch cell 0) (fetch cell 1)) cell))
                                              (neighboring_op (car move) player-list))]
                                       [token_move (first (third move))]
                                       [token_num (first move)])
                                  (cond
                                  [(empty? neighbors_with_score) (make_move move cards space-list player-list turn 1)]
                                  [else
                                       (let*
                                           ([c (argmax custom_max neighbors_with_score)]
                                            [cell (second c)]
                                            [row_diff (- (first token_move) (first cell))]
                                            [col_diff (- (second token_move) (second cell))]
                                            [first-player (fetch player-list 0)]
                                            [second-player (fetch player-list 1)]
                                            [new-players (list (append cell (remove (fetch first-player token_num) first-player))
                                                              (list (list (+ (first cell) row_diff) (+ (second cell) col_diff)) (remove cell second-player)))])
                                        (if (and (< (fetch move 1) (fetch cell 0))
                                                 (IsValid? (+ (first cell) row_diff) (+ (second cell) col_diff)
                                                                                        (list (remove (fetch (first player-list) token_num) (first player-list)) (second player-list))))
                                            
                                            (make_move (cons token_num
                                                             (cons (first c) 
                                                                   (FindBuildSpace space-list (fetch2D player-list 0 token_num) cell 
                                                                                   (build-list (fetch cell 0) (fetch cell 1))))) cards space-list new-players turn 1) 
                                             
                                            (make_move move cards space-list player-list turn 1)))]
                                  ))] 
      [(equal? card "Pan") (let* ([move (make_best_move (map (lambda (token)
                                                               (RateTokenAndBuildMoves space-list token player-list (build-list-artemis (fetch token 0) (fetch token 1)))) (fetch player-list 0)))]
                                  [lst (map (lambda (token)
                                              (map (lambda (cell)
                                                     (if (and
                                                          (> (fetch2D space-list (first token) (second token))
                                                             (fetch2D space-list (first cell) (second cell)))
                                                          (>
                                                           (abs (- (fetch2D space-list (first token) (second token))
                                                                   (fetch2D space-list (first cell) (second cell)))) 1))
                                                         cell
                                                         empty))
                                                   (filter positive_cell (build-list (first token) (second token)))))
                                            (fetch player-list 0))]) 

                             
                             (cond
                                   [(number? (index-of (map (lambda (cell) (empty? cell)) (first lst)) #f))
                                      (make_move (list 0 100 (list (fetch (first lst) (index-of (map (lambda (cell) (empty? cell)) (first lst)) #f)) (list -1 -1)))
                                                 cards space-list player-list turn 1)]
                                   [(number? (index-of (map (lambda (cell) (empty? cell)) (second lst)) #f))
                                     
                                    (make_move (list 1  100 (list (fetch (second lst) (index-of (map (lambda (cell) (empty? cell)) (second lst)) #f)) (list -1 -1)))
                                                 cards space-list player-list turn 1)]
                                   [else
                                     
                                      (make_move move cards space-list player-list turn 1)]))]
     [(equal? card "Prometheus") (let* ([move (make_best_move (map (lambda (token)
                                                                    (RateTokenAndBuildMoves space-list token player-list (build-list (fetch token 0) (fetch token 1)))) (fetch player-list 0)))])
                                  (make_move move cards space-list player-list turn 1))])))


(define (make_move moves cards space-list player-list turn build_level)
 
  (let* ([token_pos (fetch2D (cdr moves) 1 0)]
         [build_pos (fetch2D (cdr moves) 1 1)]
         [token_rating (fetch (cdr moves) 0)]
         [build_row (fetch build_pos 0)]
         [build_col (fetch build_pos 1)]
         [token_num (car moves)]
         [token_list (swap (modify-players (change-at player-list 0 token_num token_pos) 1))]

         [hash_map (hash 'turn turn
                         'players (list (hash 'card (list-ref cards 1)
                                              'tokens (list-ref token_list 0))
                                        (hash 'card (list-ref cards 0)
                                              'tokens (list-ref token_list 1)))
                         'spaces (if (not (equal? token_rating 100))
                                     (change-at space-list build_row build_col (+ (fetch2D space-list build_row build_col) build_level))
                                      space-list))])
    
    
    hash_map))

;##############################################
; swaps players token before printing the json
;#############################################
(define (swap players-list)
  (let* ([first-player (list-ref players-list 0)]
         [second-player (list-ref players-list 1)])
    (list second-player first-player)))

;#################################################################################
;# Translates given token co-ordinates to board co-ordinates by adding +1/-1
;#################################################################################
(define (modify-players player-list addend) 
  (let* ([first-player-token (fetch2D player-list 0 0)]
         [first-player-token2 (fetch2D player-list 0 1)]
         [second-player-token (fetch2D player-list 1 0)]
         [second-player-token2 (fetch2D player-list 1 1)])
    (list
     (list
      (list (+ (fetch first-player-token 0) addend) (+ (fetch first-player-token 1) addend))
      (list (+ (fetch first-player-token2 0) addend) (+ (fetch first-player-token2 1) addend)))
     (list
      (list (+ (fetch second-player-token 0) addend) (+ (fetch second-player-token 1) addend))
      (list (+ (fetch second-player-token2 0) addend) (+ (fetch second-player-token2 1) addend))))))

;#################################################################################
;# returns a pair of cardnames and respective token co-ordinates of the players
;#################################################################################
(define (extract_tokens_and_cards players)
  (let* ([first-player (fetch players 0)]
         [second-player (fetch players 1)])
  (cons (list (hash-ref first-player 'card) (hash-ref second-player 'card))
          (list (hash-ref first-player 'tokens) (hash-ref second-player 'tokens)))))

;###################################################################################


(define (select_token_positions tokens)
 (let*
      ([token1 (first tokens)]
       [token2 (second tokens)]
       [test (list (list 3 3) (list 3 2) (list 2 2) (list 2 1))]
   
       [lst (filter cons? (map (lambda (c) (if (list? (member c tokens))
                            empty
                            c)) test))])      
       (list (first lst) (second lst))))
;###################################################################################

;(list (list (list 0 0) (list 0 1)))
(define (play board)

  (cond
    [(list? board) (let*
                         ([first-player-card (first board)]
                          [second-player-card (second board)])
                         (if (hash-has-key? second-player-card 'tokens)
                             (list second-player-card (hash 'card (hash-ref first-player-card 'card)
                                                                           'tokens (select_token_positions (hash-ref second-player-card 'tokens))))
                             (list second-player-card
                                   (hash 'card (hash-ref first-player-card 'card)
                                         'tokens (list (list 1 1) (list 1 2))))
                                   ))]
    [else
     (let* ([players (extract_tokens_and_cards (hash-ref board 'players))])
       (make-card-based-moves (hash-ref board 'spaces) (car players) (modify-players (cdr players) -1) (+ (hash-ref board 'turn) 1)))]))    

		
(define (func)
  (let* ([mp (play (read-json))])  
    (write-json mp)
    (flush-output))
  (func))

(func)