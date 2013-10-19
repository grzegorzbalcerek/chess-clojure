(ns chess.figure
  "The `chess.figure` namespace contains the functionality for representing
  chess figures."
  (:use (chess.color)))

(defrecord Figure [figure-type figure-color])

(defn show-figure
  "Returns a one-character string representing the figure."
   [{f :figure-type c :figure-color}]
  (case [f c]
    [:king   :white] "k"
    [:queen  :white] "q"
    [:rook   :white] "r"
    [:bishop :white] "b"
    [:knight :white] "n"
    [:pawn   :white] "p"
    [:king   :black] "K"
    [:queen  :black] "Q"
    [:rook   :black] "R"
    [:bishop :black] "B"
    [:knight :black] "N"
    [:pawn   :black] "P"))

(defn figure-symbol
  "Returns a one-character string with the Unicode symbol representing the figure."
  [{f :figure-type c :figure-color}]
  (str (char (case [f c]
               [:king   :white] 0x2654
               [:queen  :white] 0x2655
               [:rook   :white] 0x2656
               [:bishop :white] 0x2657
               [:knight :white] 0x2658
               [:pawn   :white] 0x2659
               [:king   :black] 0x265a
               [:queen  :black] 0x265b
               [:rook   :black] 0x265c
               [:bishop :black] 0x265d
               [:knight :black] 0x265e
               [:pawn   :black] 0x265f))))

(comment

java clojure.main
(use 'chess.figure)
(doc chess.figure)
(->Figure :king :white)
(doc show-figure)
(show-figure (->Figure :king :white)) ; "k"
(show-figure (->Figure :king :black)) ; "K"
(show-figure (->Figure :knight :white)) ; "n"
(doc figure-symbol)
(figure-symbol (->Figure :king :white))
(figure-symbol (->Figure :king :black))
(figure-symbol (->Figure :knight :white))


)

