(ns chess.move
  "The `chess.move` namespace contains the functionality for representing
  figure moves.")

(defrecord RegularMove [from to])
(defrecord PromotionMove [from to figure])
(defrecord EnPassantMove [from to captured])
(defrecord CastlingMove [from to from-rook to-rook])

(comment

java clojure.main
(use 'chess.move)

)

