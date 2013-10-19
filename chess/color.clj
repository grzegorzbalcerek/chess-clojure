(ns chess.color
  "The `chess.color` namespace contains the functionality for representing
  one of the two colors (:black or :white)
  used in the game of Chess.")

(defn other
  "The `other` method returns the opposite color."
  [color]
  (case color
    :white :black
    :black :white))

(defn first-row
  "The `firstRow` method returns the coordinate of the first row
  from the point of view of a player who plays the given color."
  [color]
  (case color
    :white 1
    :black 8))

(comment

java clojure.main
(use 'chess.color)
(other :white) ; :black
(other :black) ; :white
(first-row :white) ; 1
(first-row :black) ; 8
(doc chess.color)
(doc other)
(doc first-row)

)
