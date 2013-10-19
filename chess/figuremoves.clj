(ns chess.figuremoves
  "The `chess.figuremoves` namespace contains the functionality for representing
  figure move rules.")
(use 'chess.color)
(use 'chess.field)

(defn rook-moves
  "Sequences of relative figure positions for rook moves."
  []
  [[(iterate inc 1)(repeat 0)]
   [(iterate dec -1)(repeat 0)]
   [(repeat 0)(iterate inc 1)]
   [(repeat 0)(iterate dec -1)]])

(defn bishop-moves
  "Sequences of relative figure positions for bishop moves."
  []
  [[(iterate inc 1)(iterate inc 1)]
   [(iterate dec -1)(iterate inc 1)]
   [(iterate inc 1)(iterate dec -1)]
   [(iterate dec -1)(iterate dec -1)]])

(defn queen-moves
  "Sequences of relative figure positions for queen moves."
  []
  (concat (rook-moves) (bishop-moves)))

(defn knight-moves
  "Sequences of relative figure positions for knight moves."
  []
  [[[1][2]]
   [[2][1]]
   [[-1][2]]
   [[2][-1]]
   [[-1][-2]]
   [[-2][-1]]
   [[1][-2]]
   [[-2][1]]])

(defn king-moves
  "Sequences of relative figure positions for king moves."
  []
  (map (fn [[c r]] [(take 1 c)(take 1 r)]) (queen-moves)))

(defn choose-figure-moves
  "Choose the sequences of relative figure positions
  based on the figure position, type, color,
  and whether the move is a capture move or not."
  [{figure-type :figure-type figure-color :figure-color} {row :row} capture]
  (case figure-type
    :rook   (rook-moves)
    :bishop (bishop-moves)
    :queen  (queen-moves)
    :king   (king-moves)
    :knight (knight-moves)
    :pawn   (case capture
                false (case figure-color
                        :white (if (= row 2)
                                 [[(repeat 0)[1 2]]]
                                 [[[0][1]]])
                        :black (if (= row 7)
                                 [[(repeat 0)[-1 -2]]]
                                 [[[0][-1]]]))
                true  (case figure-color
                        :white [[[-1][1]][[1][1]]]
                        :black [[[-1][-1]][[1][-1]]]))))

(defn zip
  [seq1 seq2]
  (map (fn [x y] [x y]) seq1 seq2))

(defn relative-field
  "Returns the field relative to the given field according to
  a pair of relative coordinates."
  [{col :col row :row} [c r]]
  (->Field (+ col c) (+ row r)))

(defn relative-fields
  "Returns fields relative to the given field according to
  the sequence of relative coordinates."
  [field [cols rows]]
  (take-while is-valid (map (partial relative-field field) (zip cols rows))))

(defn figure-moves [figure field capture]
  "Returns possible figure moves.
  The figure is on the field 'field' and the 'capture' flag indicate whether
  the move is a capture."
  (map (partial relative-fields field) (choose-figure-moves figure field capture)))

(comment

java clojure.main
(use 'chess.figuremoves)
(use 'chess.figure)
(use 'chess.field)
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (rook-moves)) ; ([(1 2 3 4) (0 0 0 0)] [(-1 -2 -3 -4) (0 0 0 0)] [(0 0 0 0) (1 2 3 4)] [(0 0 0 0) (-1 -2 -3 -4)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (bishop-moves)) ; ([(1 2 3 4) (1 2 3 4)] [(-1 -2 -3 -4) (1 2 3 4)] [(1 2 3 4) (-1 -2 -3 -4)] [(-1 -2 -3 -4) (-1 -2 -3 -4)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (queen-moves)) ; ([(1 2 3 4) (0 0 0 0)] [(-1 -2 -3 -4) (0 0 0 0)] [(0 0 0 0) (1 2 3 4)] [(0 0 0 0) (-1 -2 -3 -4)] [(1 2 3 4) (1 2 3 4)] [(-1 -2 -3 -4) (1 2 3 4)] [(1 2 3 4) (-1 -2 -3 -4)] [(-1 -2 -3 -4) (-1 -2 -3 -4)])
(knight-moves) ; [[[1] [2]] [[2] [1]] [[-1] [2]] [[2] [-1]] [[-1] [-2]] [[-2] [-1]] [[1] [-2]] [[-2] [1]]]
(king-moves) ; ([(1) (0)] [(-1) (0)] [(0) (1)] [(0) (-1)] [(1) (1)] [(-1) (1)] [(1) (-1)] [(-1) (-1)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :white) {:row 2} false)) ; ([(0 0 0 0) (1 2)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :white) {:row 4} false)) ; ([(0) (1)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :black) {:row 7} false)) ; ([(0 0 0 0) (-1 -2)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :black) {:row 5} false)) ; ([(0) (-1)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :white) {:row 2} true)) ; ([(-1) (1)] [(1) (1)])
(map (fn [[a b]] [(take 4 a)(take 4 b)]) (choose-figure-moves (->Figure :pawn :black) {:row 7} true)) ; ([(-1) (-1)] [(1) (-1)])
(relative-field (->Field 1 2) [1 1]) ; #chess.field.Field{:col 2, :row 3}
(relative-field (->Field 1 2) [0 2]) ; #chess.field.Field{:col 1, :row 4}
(relative-fields (->Field 2 2) [[0 0][1 2]]) ; (#chess.field.Field{:col 2, :row 3} #chess.field.Field{:col 2, :row 4})
(map (fn [fields] (map show-field fields)) (figure-moves (->Figure :rook :white) (->Field 3 4) false)) ; (("d4" "e4" "f4" "g4" "h4") ("b4" "a4") ("c5" "c6" "c7" "c8") ("c3" "c2" "c1"))
(map (fn [fields] (map show-field fields)) (figure-moves (->Figure :pawn :white) (->Field 2 2) false) ) ; (("b3" "b4"))
(map (fn [fields] (map show-field fields)) (figure-moves (->Figure :pawn :white) (->Field 2 2) true)  ) ; (("a3") ("c3"))
(map (fn [fields] (map show-field fields)) (figure-moves (->Figure :pawn :white) (->Field 1 2) true)  ) ; (() ("b3"))

)

