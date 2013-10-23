(ns chess.board
  "The `chess.board` namespace contains the functionality for representing
  the game board.")
(use 'chess.field)
(use 'chess.figure)
(use 'chess.move)

(defn starting-board
  "The board state when the game starts."
  []
  {(->Field 1 1) (->Figure :rook :white)
   (->Field 2 1) (->Figure :knight :white)
   (->Field 3 1) (->Figure :bishop :white)
   (->Field 4 1) (->Figure :queen :white)
   (->Field 5 1) (->Figure :king :white)
   (->Field 6 1) (->Figure :bishop :white)
   (->Field 7 1) (->Figure :knight :white)
   (->Field 8 1) (->Figure :rook :white)
   (->Field 1 2) (->Figure :pawn :white)
   (->Field 2 2) (->Figure :pawn :white)
   (->Field 3 2) (->Figure :pawn :white)
   (->Field 4 2) (->Figure :pawn :white)
   (->Field 5 2) (->Figure :pawn :white)
   (->Field 6 2) (->Figure :pawn :white)
   (->Field 7 2) (->Figure :pawn :white)
   (->Field 8 2) (->Figure :pawn :white)
   (->Field 1 7) (->Figure :pawn :black)
   (->Field 2 7) (->Figure :pawn :black)
   (->Field 3 7) (->Figure :pawn :black)
   (->Field 4 7) (->Figure :pawn :black)
   (->Field 5 7) (->Figure :pawn :black)
   (->Field 6 7) (->Figure :pawn :black)
   (->Field 7 7) (->Figure :pawn :black)
   (->Field 8 7) (->Figure :pawn :black)
   (->Field 1 8) (->Figure :rook :black)
   (->Field 2 8) (->Figure :knight :black)
   (->Field 3 8) (->Figure :bishop :black)
   (->Field 4 8) (->Figure :queen :black)
   (->Field 5 8) (->Figure :king :black)
   (->Field 6 8) (->Figure :bishop :black)
   (->Field 7 8) (->Figure :knight :black)
   (->Field 8 8) (->Figure :rook :black)})

(defn show-board
  "Shows the board."
  [board]
  (letfn [(show-row [row]
            (str row
                 (apply str (map (partial show-field row) (range 1 9)))
                 row "\n"))
          (show-field [row col]
            (let [figure (board (->Field col row))]
              (case figure
                nil "."
                (show-figure figure))))]
    (str " abcdefgh\n"
         (apply str (map show-row (range 8 0 -1)))
         " abcdefgh\n")))

(defmulti update-board
  "Returns a new board, updated with a move."
  (fn [_ move] (class move)))
(defmethod update-board chess.move.RegularMove [board {from :from to :to}]
  (let [figure (board from)]
    (-> board (dissoc from) (assoc to figure))))
(defmethod update-board chess.move.PromotionMove [board {from :from to :to figure :figure}]
  (-> board (dissoc from) (assoc to figure)))
(defmethod update-board chess.move.EnPassantMove [board {from :from to :to captured :captured}]
  (let [figure (board from)]
    (-> board
        (dissoc captured)
        (dissoc from)
        (assoc to figure))))
(defmethod update-board chess.move.CastlingMove [board {from :from to :to from-rook :from-rook to-rook :to-rook}]
  (let [king-figure (board from)
        rook-figure (board from-rook)]
    (-> board
        (dissoc from-rook)
        (dissoc from)
        (assoc to king-figure)
        (assoc to-rook rook-figure))))

(comment

java clojure.main
(use 'chess.board)
(use 'chess.figure)
(use 'chess.field)
(use 'chess.move)
(starting-board)
(show-board (starting-board))
(print(show-board (starting-board)))
(print (show-board (update-board (starting-board) (->RegularMove (->Field 2 2) (->Field 2 3)))))
(print (show-board (update-board (starting-board) (->RegularMove (->Field 3 3) (->Field 2 3)))))
(print (show-board (update-board (starting-board) (->PromotionMove (->Field 2 2) (->Field 2 8) (->Figure :queen :white)))))
(print (show-board (update-board (starting-board) (->EnPassantMove (->Field 2 2) (->Field 3 3) (->Field 3 7)))))
(print (show-board (update-board (starting-board) (->CastlingMove (->Field 5 1) (->Field 3 1) (->Field 1 1) (->Field 4 1)))))

)
