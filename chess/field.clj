(ns chess.field
  "The `chess.field` namespace contains the functionality for representing
  board fields. The game of chess is played on a board with 64 fields.
  The board has the shape of a square with eight rows — from 1 to 8
  — and eight columns — from `a` to `h`.
  The `Field` represents a board field.
  It has members representing the column and the row —
  the field coordinates on the chess board.
  Valid fields have coordinates in the range between 1 and 8.")
(use 'chess.color)

(defrecord Field [^Integer col ^Integer row])

(defn show-field
  "Shows field coordinates as a pair of characters:
  a letter representing the column and a number representing the row."
  [{col :col row :row}]
  (str (char (+ (int \a) col -1)) row))

(defn relative
  "Returns a new field with coordinates moved
  by the given number of rows and columns relative to the original field."
  [{col :col row :row} c r]
  (->Field (+ col c) (+ row r)))

(defn is-last-row
  "Returns a boolean value indicating
  whether the given field belongs to the last row from
  the point of view of a player."
  [{row :row} color]
  (= row (first-row (other color))))

(defn is-valid
  "Returns a boolean value indicating
  whether the field has valid coordinates, that is
  whether it belongs to the board."
  [{col :col row :row}]
  (and (>= col 1) (<= col 8) (>= row 1) (<= row 8)))

(comment

java clojure.main
(use 'chess.field)
(doc chess.field)
(->Field 1 1)
(doc show-field)
(show-field (->Field 1 1)) ; "a1"
(show-field (->Field 2 1)) ; "b1"
(show-field (->Field 2 3)) ; "b3"
(show-field (->Field 8 8)) ; "h8"
(doc relative)
(relative (->Field 2 3) 1 1) ; #chess.field.Field{:col 3, :row 4}
(show-field (relative (->Field 2 3) 4 5)) ; "f8"
(doc is-last-row)
(is-last-row (->Field 8 8) :white) ; true
(is-last-row (->Field 7 8) :white) ; true
(is-last-row (->Field 7 8) :black) ; false
(is-last-row (->Field 7 1) :black) ; true
(is-last-row (->Field 2 2) :white) ; false
(doc is-valid)
(is-valid (->Field 2 2)) ; true
(is-valid (->Field 0 2)) ; false
(is-valid (->Field 2 0)) ; false
(is-valid (->Field 2 9)) ; false
(is-valid (->Field 9 2)) ; false

)

