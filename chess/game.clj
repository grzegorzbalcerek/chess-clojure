(ns chess.game)

(use 'chess.color)
(use 'chess.field)
(use 'chess.figure)
(use 'chess.figuremoves)
(use 'chess.board)
(use 'chess.move)

(defrecord GameStart [])
(defrecord OngoingGame [color board hist last-move])

(defmulti show-game
  "Returns the textual representation of a game."
  class)
(defmethod show-game GameStart [_]
  (str "White to begin:\n" (show-board (starting-board))))
(defmethod show-game OngoingGame [{color :color board :board last-move :last-move}]
  (str "Last move: " (other color) " " (:from last-move) " to "
       (:to last-move) "\n" (show-board board)))

(defmulti game-color class)
(defmethod game-color GameStart [_] :white)
(defmethod game-color OngoingGame [{color :color}] color)

(defmulti game-hist class)
(defmethod game-hist GameStart [_] [])
(defmethod game-hist OngoingGame [{hist :hist}] hist)

(defmulti game-board class)
(defmethod game-board GameStart [_] (starting-board))
(defmethod game-board OngoingGame [{board :board}] board)

(defmulti game-last-move class)
(defmethod game-last-move GameStart [_] nil)
(defmethod game-last-move OngoingGame [{last-move :last-move}] last-move)

(defn is-field-empty
  "Verifies if the given field is empty."
  [game field]
  (not (contains? (game-board game) field)))

(defn free-destinations
  "Returns free fields onto which the figure may be moved."
  [game fieldss]
  (mapcat (fn [fields] (take-while (partial is-field-empty game) fields)) fieldss))

(defn capture-destinations
  "Returns fields occupied by the enemy figures
  (including the case when that figure is the King)
  onto which the figure may be moved."
  [game fieldss]
  (letfn [(has-enemy-figure [field]
            (= (:figure-color ((game-board game) field)) (other (game-color game))))]
    (mapcat (comp
             (partial filter has-enemy-figure)
             (partial take 1)
             (partial drop-while (partial is-field-empty game)))
            fieldss)))

(defn defended-destinations
  "Returns fields occupied by the same color figures
  (including the case when that figure is the King)
  onto which the figure might be moved (if the field
  was taken by an enemy figure)."
  [game fieldss]
  (letfn [(has-same-color-figure [field]
            (= (:figure-color ((game-board game) field)) (game-color game)))]
    (mapcat (comp
             (partial filter has-same-color-figure)
             (partial take 1)
             (partial drop-while (partial is-field-empty game)))
            fieldss)))

(defn update-game
  "Returns a new game, updated with a move."
  [game move]
  (->OngoingGame (other (game-color game))
                 (update-board (game-board game) move)
                 (conj (game-hist game) game)
                 move))

(def next-games)

(defn is-other-king-under-check
  "Verifies if the enemy King is under check."
  [game]
  (letfn [(is-king-on-board [g]
            (some (fn [figure] (= figure (->Figure :king (other (game-color game)))))
                  (vals (game-board g))))]
    (not (every? is-king-on-board (next-games game)))))

(defn is-king-under-check
  "Verifies if the King of the player who is about to make a move is under check."
  [game]
  (let [new-game (->OngoingGame (other (game-color game))
                                (game-board game)
                                (conj (game-hist game) game)
                                (->RegularMove (->Field 0 0) (->Field 0 0)))]
    (is-other-king-under-check new-game)))

(defn castling
  "Verifies the conditions of when the castling move is permitted:
   whether the King and the Rook are on their initial positions,
   whether they were there from the begining of the game,
   whether the fields between them are free and
   whether the field to be passed by the King is checked or not.
   If the given castling move is permitted, the method returns a one-element sequence.
   Otherwise it returns an empty sequence."
  [game king-to rook-from rook-to other-col]
  (let [color (game-color game)
        row (first-row color)
        hist (game-hist game)
        board (game-board game)]
    (if (and
         (= (board (->Field 4 row)) (->Figure :king color))
         (= (board (->Field rook-from row)) (->Figure :rook color))
         (= (board (->Field rook-to row)) nil)
         (= (board (->Field king-to row)) nil)
         (= (board (->Field other-col row)) nil)
         (every? (fn [g] (= ((game-board g) (->Field 4 row)) (->Figure :king color))) hist)
         (every? (fn [g] (= ((game-board g) (->Field rook-from row)) (->Figure :rook color))) hist)
         (not (is-other-king-under-check (update-game game (->RegularMove (->Field 4 row)
                                                                          (->Field rook-to row))))))
      [update-game game (->CastlingMove (->Field 4 row) (->Field king-to row)
                                        (->Field rook-from row) (->Field rook-to row))]
      [])))

(defmulti is-en-passant-capture
  "Verifies if the en passant capture move is possible."
  (fn [cl _ _] (class cl)))
(defmethod is-en-passant-capture GameStart [_ _ _] false)
(defmethod is-en-passant-capture OngoingGame [{color :color board :board last-move :last-move} from to]
  (and (= (board (:to last-move)) (->Figure :pawn (other color)))
       (= (:to last-move) (->Field (:col to) (:row from)))
       (= (:from last-move) (->Field (:col to) (+ (:row from) (* 2 (- (:row to) (:row from))))))))

(defn next-games
  "Returns next games after possible next moves moves (including those
  moves after which the King is checked).
  The code itereates over all figures that have the same color as
  the color of the next move. The 'g' value contains sequences of game states
  corresponding to the possible next moves of the given figure.
  Figure moves depend on its kind. The Rook, the Knight, the Queen, the Bishop
  and the King are treated in a similar way, except for the King, for which
  the castling moves are included as well.
  Potentially there are two possible castling moves.
  Each of them is handled by a call to the 'castling' method.
  The most complex case handled by the mthod is the case of the Pawn moves.
  The Pawn may move forward onto a free field or forward and left or right onto
  a field occupied by an enemy figure. In both cases, if the destination field
  lies on the last row, the set of possible moves includes the possible
  promotions to other figures. In addition to that, the Pawn may make the so
  called en passant capture, which consists of moving the Pawn forward and left
  or right onto a free field which has been passed-by by an enemy Pawn in the
  previous move."
  [game]
  (for [[from figure] (game-board game)
        :when (= (:figure-color figure) (game-color game))
        next-game (case (:figure-type figure)
                    (:rook :bishop :queen :knight :king)
                      (concat
                       (let [fieldss (figure-moves figure from true)]
                         (map (fn [to] (update-game game (->RegularMove from to)))
                              (concat (free-destinations game fieldss)
                                      (capture-destinations game fieldss))))
                       (if (= (:figure-type figure) :king)
                         (concat (castling game 3 1 4 2) (castling game 7 8 6 7))))
                    :pawn
                      (let [regular-and-promotion-moves
                            (mapcat
                             (fn [to] (if (is-last-row to (game-color game))
                                        (map (fn [figure] (update-game game (->PromotionMove from to figure)))
                                             [(->Figure :queen (game-color game))
                                              (->Figure :rook (game-color game))
                                              (->Figure :bishop (game-color game))
                                              (->Figure :knight (game-color game))])
                                        [(update-game game (->RegularMove from to))]))
                             (concat
                              (free-destinations game (figure-moves figure from false))
                              (capture-destinations game (figure-moves figure from true))))
                            en-passant-moves
                            ((comp
                              (partial map (fn [to] (update-game game (->EnPassantMove from to (->Field (:col to) (:row from))))))
                              (partial filter #(is-en-passant-capture game from %)))
                             (free-destinations game (figure-moves figure from false)))]
                        (concat regular-and-promotion-moves en-passant-moves)))]
    next-game))

(defn valid-games
  "Filters out the next games in which the king is under check."
  [game]
  (filter (comp not is-other-king-under-check) (next-games game)))

(defn is-game-finished
  "Verifies if the game is over.
  The following end game conditions are handled:
  + after every possible move the King is under check,
  + only the two Kings are left on the board,
  + only the two Kings, one Bishop and one Knight are left on the board,
  + only the two Kings and two Knights of the same color are left on the board,
  + the same position occurred three times."
  [game]
  (letfn [(figure-sort [_] (partial sort-by show-figure))]
    (or (every? (fn [g] (is-other-king-under-check g)) (next-games game))
        (contains? [ (figure-sort [ (->Figure :king :white), (->Figure :king :black) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :bishop :white) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :bishop :black) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :knight :white) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :knight :black) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :knight :white), (->Figure :knight :white) ]),
                     (figure-sort [ (->Figure :king :white), (->Figure :king :black), (->Figure :knight :black), (->Figure :knight :black) ]) ]
                   (figure-sort (vals (game-board game))))
        ((comp not empty? (partial filter (fn [g] (>= (count g) 3))) vals (partial group-by identity) (partial map game-board))
         (conj (game-hist game) game)))))
  
(defn winner
  "Returns the color of the game winner or nil if there is no winner."
  [game]
  (if (and (is-game-finished game) (is-king-under-check game))
    (other (game-color game))
    nil))

(defn move
  "Returns a new game state after moving a figure. If the given
  move is not possible, it returns nil."
  [from to promotion game]
  (letfn [(is-matching [{move :last-move}]
            (and (= from (:from move))
                 (= to (:to move))
                 (if (= (class move) chess.move.PromotionMove)
                   (= promotion (:figure move))
                   (nil? promotion))))]
    (first (filter is-matching (valid-games game)))))

(comment

java clojure.main
(use 'chess.game)
(use 'chess.field)
(use 'chess.figure)
(use 'chess.figuremoves)
(print(show-game (->GameStart)))
(is-field-empty (->GameStart) (->Field 2 2)) ; false
(is-field-empty (->GameStart) (->Field 2 3)) ; true
(map show-field (free-destinations (->GameStart) (figure-moves (->Figure :rook :white) (->Field 3 4) false))) ; ("d4" "e4" "f4" "g4" "h4" "b4" "a4" "c5" "c6" "c3")
(map show-field (free-destinations (->GameStart) (figure-moves (->Figure :bishop :white) (->Field 3 4) false))) ; ("d5" "e6" "b5" "a6" "d3" "b3")
(map show-field (capture-destinations (->GameStart) (figure-moves (->Figure :rook :white) (->Field 3 4) false))) ; ("c7")
(map show-field (capture-destinations (->GameStart) (figure-moves (->Figure :bishop :white) (->Field 3 4) false))) ; ("f7")
(is-other-king-under-check (->GameStart)) ; false
(is-king-under-check (->GameStart)) ; false
(is-game-finished (->GameStart)) ; false
(winner (->GameStart)) ; nil
(count (next-games (->GameStart))) ; 20
(count (valid-games (->GameStart))) ; 20
(move (->Field 1 2) (->Field 1 5) nil (->GameStart)) ; nil
(def g1 (move (->Field 7 2) (->Field 7 4) nil (->GameStart)))
(def g2 (move (->Field 5 7) (->Field 5 6) nil g1))
(def g3 (move (->Field 6 2) (->Field 6 4) nil g2))
(def g4 (move (->Field 4 8) (->Field 8 4) nil g3))
(print(show-game g4))
(is-other-king-under-check g4) ; false
(is-king-under-check g4) ; true
(is-game-finished g4) ; true
(winner g4) ; :black
(count (next-games g4)) ; 20
(count (valid-games g4)) ; 0

)
