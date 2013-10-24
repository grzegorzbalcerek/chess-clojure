(ns chess.rank)
(use 'chess.game)
(use 'chess.figure)
(use 'chess.figuremoves)
(use 'chess.field)
(use 'chess.color)

(defn figure-rank
  "Returns the rank of a figure of the given type."
  [{figure-type :figure-type}]
  (case figure-type
    :queen 900
    :rook 450
    :knight 300
    :bishop 300
    :pawn 100
    :king 0))

(defn field-rank
  "Returns the rank of the given field."
  [{col :col row :row}]
  (letfn [(col-row-rank [cr]
            (if (>= cr 5) (- 9 cr) cr))]
    (* (* 2 (col-row-rank col)) (col-row-rank row))))

(defn figure-defending-other-figures-rank
  "Returns the figure rank based on the figures it is defending."
  [game field figure]
  (quot (count (defended-destinations game (figure-moves figure field true))) 2))

(defn check-rank
  "Returns a rank value related to whethervthevKing is under check or not."
  [game color]
  (if (and (= (game-color game) (other color))
           (is-king-under-check game))
    50
    0))

(defn color-rank
  "Calculates the position rank from the point of view of a player."
  [game color]
  (let [ranks (for [[field figure] (game-board game)
                    :when (= (:figure-color figure) color)
                    :let [r1 (figure-rank figure)
                          r2 (field-rank field)
                          r3 (figure-defending-other-figures-rank game field figure)]]
                (+ r1 r2 r3))]
    (letfn [(sum [coll] (reduce + coll))]
      (+ (sum ranks) (check-rank game color)))))

(defn rank
  "Calculates the position rank."
  [game color]
  (- (color-rank game color)
     (color-rank game (other color))))

(comment

(use 'chess.computerplayer)
(use 'chess.figure)
(use 'chess.field)
(use 'chess.game)
(figure-rank (->Figure :queen :white)) ; 900
(figure-rank (->Figure :knight :black)) ; 300
(field-rank (->Field 1 1)) ; 2
(field-rank (->Field 2 5)) ; 16
(field-rank (->Field 4 4)) ; 32
(def g1 (move (->Field 1 2) (->Field 1 3) nil (->GameStart)))
(def g2 (move (->Field 1 7) (->Field 1 6) nil g1))
(figure-defending-other-figures-rank g2 (->Field 2 1) (->Figure :knight :white)) ; 1
(def g1 (move (->Field 7 2) (->Field 7 4) nil (->GameStart)))
(def g2 (move (->Field 5 7) (->Field 5 6) nil g1))
(def g3 (move (->Field 6 2) (->Field 6 4) nil g2))
(def g4 (move (->Field 4 8) (->Field 8 4) nil g3))
(check-rank (->GameStart) :white) ; 0
(check-rank g4 :white) ; 0
(check-rank g4 :black) ; 50
(color-rank (->GameStart) :white) ; 3928
(color-rank g1 :white) ; 3928
(color-rank g2 :white) ; 3935
(color-rank g3 :white) ; 3940
(color-rank g4 :white) ; 3947
(rank (->GameStart) :white) ; 8
(rank g1 :white) ; 0
(rank g2 :white) ; 7
(rank g3 :white) ; 5
(rank g4 :white) ; -32
(rank (->GameStart) :black) ; -8

)
