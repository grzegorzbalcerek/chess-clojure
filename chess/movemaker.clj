(ns chess.movemaker)
(use 'chess.field :reload)
(use 'chess.figure :reload)
(use 'chess.move :reload)
(use 'chess.color :reload)
(use 'chess.figurerules :reload)
(use 'chess.game :reload)

(defn figure-rank [{figure-type :figure-type}]
  (case figure-type
    :queen 900
    :rook 450
    :knight 300
    :bishop 300
    :pawn 100
    :king 0))

(defn field-rank [{col :col row :row}]
  (letfn [(col-row-rank [cr]
            (if (>= cr 5) (- 9 cr) cr))]
    (* (* 2 (col-row-rank col)) (col-row-rank row))))

(defn figure-defending-other-figures-rank [game field figure]
  (quot (count (moves-to-occupied-fields game (figure-moves field figure true))) 2))

(defn check-rank [game color]
  (if (and (= (game-color game) (other color))
           (is-king-under-check game))
    50
    0))

(defn color-rank [game color]
  (let [ranks (for [[field figure] (game-board game)
                    :when (= (:figure-color figure) color)
                    :let [r1 (figure-rank figure)
                          r2 (field-rank field)
                          r3 (figure-defending-other-figures-rank game field figure)]]
                (+ r1 r2 r3))]
    (letfn [(sum [coll] (reduce + coll))]
      (+ (sum ranks) (check-rank game color)))))

(defn rank [game color]
  (- (color-rank game color)
     (color-rank game (other color))))

(defn moves [game]
  (let [moves (valid-games game)]
    (if (empty? moves) []
        (let [ranked-moves (map (fn [g] [g (rank g (game-color g))]) moves)
              ranked-moves-sorted (sort-by second ranked-moves)
              first-rank ((comp second first) ranked-moves-sorted)
              max-rank-moves (take-while (fn [[_ rank]] (= rank first-rank)) ranked-moves-sorted)]
          (map first max-rank-moves)))))

(defn make-move [game]
  (first (moves game)))

(comment

(use 'chess.movemaker :reload)
(map (fn [game] (print (show-board (game-board game)))) (moves (->GameStart)))
(moves (->GameStart))
(rank (->GameStart) :white)
(color-rank (->GameStart) :white)
(color-rank (->GameStart) :black)
(check-rank (->GameStart) :white)
(figure-defending-other-figures-rank (->GameStart) (->Field 2 1) (->Figure :knight :white))
(field-rank (->Field 2 2))
(figure-rank (->Figure :queen :black))

)
