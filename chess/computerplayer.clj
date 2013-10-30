(ns chess.computerplayer)

(use 'chess.game)
(use 'chess.figure)
(use 'chess.figuremoves)
(use 'chess.field)
(use 'chess.color)
(use 'chess.rank)

(defn choose-randomly
  "Randomly chooses one of the elements of the given sequence."
  [moves]
  (if (empty? moves) nil
      (nth moves (rand-int (count moves)))))

(defn moves
  "Returns a sequence of the best ranked moves."
  [game]
  (let [moves (valid-games game)]
    (if (empty? moves) []
        (let [ranked-moves (map (fn [g] [g (rank g (game-color g))]) moves)
              ranked-moves-sorted (sort-by second ranked-moves)
              first-rank ((comp second first) ranked-moves-sorted)
              max-rank-moves (take-while (fn [[_ rank]] (= rank first-rank)) ranked-moves-sorted)]
          (map first max-rank-moves)))))

(defn make-move
  "Makes a move and returns the next game state."
  [game]
  (choose-randomly (moves game)))

(comment

(use 'chess.computerplayer)
(use 'chess.figure)
(use 'chess.field)
(use 'chess.game)
(def g1 (move (->Field 7 2) (->Field 7 4) nil (->GameStart)))
(def g2 (move (->Field 5 7) (->Field 5 6) nil g1))
(def g3 (move (->Field 6 2) (->Field 6 4) nil g2))
(def g4 (move (->Field 4 8) (->Field 8 4) nil g3))
(count (moves (->GameStart))) ; 2
(count (moves g1)) ; 2
(count (moves g2)) ; 2
(count (moves g3)) ; 1
(count (moves g4)) ; 0
(def g1 (make-move (->GameStart)))
(print(show-game g1))
(def g2 (make-move g1))
(print(show-game g2))
(def g3 (make-move g2))
(print(show-game g3))
(def g4 (make-move g3))
(print(show-game g4))

)
