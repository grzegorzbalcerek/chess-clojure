(ns chess.gui)

(import [javax.swing JFrame JPanel])
(import [java.awt Dimension Color Rectangle Font AWTEvent])
(import [java.awt.event MouseAdapter MouseListener])
(use 'chess.color)
(use 'chess.field)
(use 'chess.figure)
(use 'chess.game)
(use 'chess.computerplayer)

(defn draw-fields
  "Draws the board fields using the yellow and red colors."
  [g fs]
  (doall (for [col (range 0 8)
               row (range 0 8)]
           (doto g
             (.setPaint (if (= 0 (mod (+ col row) 2)) Color/YELLOW Color/RED))
             (.fill (Rectangle. (+ (quot fs 2) (* col fs)) (+ (quot fs 2) (* row fs)) fs fs))))))

(defn draw-board-border
  "Draws the black board border."
  [g fs]
  (doto g
    (.setPaint Color/BLACK)
    (.draw (Rectangle. (quot fs 2) (quot fs 2) (* 8 fs) (* 8 fs)))))

(defn draw-coordinates
  "Draws the board coordinates, that is the row numbers and
  the letters denoting the columns."
  [g fs]
  (.setFont g (Font. (.. g (getFont) (getName)) (.. g (getFont) (getStyle)) (quot (* 2 fs) 5)))
  (let [rect (.. g (getFont) (getStringBounds "0" (.getFontRenderContext g)))
        x-offset (int (+ (.getX rect) (/ (.getWidth rect) 2)))
        y-offset (int (+ (.getY rect) (/ (.getHeight rect) 2)))]
    (doall (for [j (range 1 9)
                 :let [row (str (- 9 j))
                       col (str (char (+ (int \a) -1 j)))]]
             (doto g
               (.drawString row (int (+(quot fs 4)(- x-offset))) (int (- (* fs j) y-offset)))
               (.drawString row (int (+(quot fs 4)(/ fs 2)(* 8 fs)(- x-offset))) (int (- (* fs j) y-offset)))
               (.drawString col (int (- (* fs j) x-offset)) (int (+(quot fs 4)(- y-offset))))
               (.drawString col (int (- (* fs j) x-offset)) (int (+(quot fs 4)(/ fs 2)(* 8 fs)(- y-offset)))))))))

(defn draw-figures
  "Draws the figures on the board."
  [g fs game]
  (.setFont g (Font. (.. g (getFont) (getName)) (.. g (getFont) (getStyle)) fs))
  (let [rect (.. g (getFont) (getStringBounds (figure-symbol (->Figure :king :white)) (.getFontRenderContext g)))
        x-offset (int (+ (.getX rect) (/ (.getWidth rect) 2)))
        y-offset (int (+ (.getY rect) (/ (.getHeight rect) 2)))]
    (doall (for [[{col :col row :row} figure] (game-board @game)]
             (doto g
               (.drawString (figure-symbol figure) (int (- (* col fs) x-offset)) (int (- (* (- 9 row) fs) y-offset))))))))

(defn draw-selected-field
  "Draws a blue border around the field selected by the player."
  [g fs field]
  (when-not (nil? field)
    (let [x (- (* (:col field) fs) (quot fs 2))
          y (- (* (- 9 (:row field)) fs) (quot fs 2))]
      (doto g
        (.setPaint Color/BLUE)
        (.draw (Rectangle. x y fs fs))
        (.draw (Rectangle. (inc x) (inc y) (- fs 2) (- fs 2)))))))

(defn field-clicked
  "Transforms the mouse click data into
  information about the field board that has been clicked."
  [e fs]
  (let [col (int (quot (+ (.. e getPoint x) (/ fs 2)) fs))
        row (int (- 9 (quot (+ (.. e getPoint y) (/ fs 2)) fs)))
        field (->Field col row)]
    (if (is-valid field) field nil)))

(defn field-size
  "Calculates the size of a single board field."
  [panel]
  (quot (min (.getWidth (.getSize panel))
             (.getHeight (.getSize panel))) 9))

(defn game-over
  "This method is called when the game is over.
  It shows a messages with
  the information about the game end and the result."
  [panel game]
  (javax.swing.JOptionPane/showMessageDialog
           panel
           (str "Game over. "
                (case (winner game)
                  :white "Winner: White."
                  :black "Winner: Black."
                  "No winner."))))

(defn promotion-figure
  "Verifies if there are any Pawn moves which require a promotion
  in the set of valid moves. If that is the case and there are
  at least two different figures possible to be chosen, a window
  is presented to the user and the user can choose the figure."
  [panel game from to]
  (let [promotion-figures
        (into-array (set
         (map :figure
          (filter (fn [move] (and (= (class move) chess.move.PromotionMove)
                                  (= from (:from move))
                                  (= to (:to move))))
                  (map game-last-move (valid-games game))))))
        figures-map
        (zipmap (map :figure-type promotion-figures) promotion-figures)
        figure
        (case (alength promotion-figures)
          0 nil
          1 (first promotion-figures)
          (javax.swing.JOptionPane/showInputDialog
           panel
           "Choose the figure to promote the Pawn to"
           "Promotion"
           javax.swing.JOptionPane/PLAIN_MESSAGE
           nil
           (into-array (keys figures-map))
           (first (keys figures-map))))]
    (figures-map figure)))

(defn game-panel
  "Implements the window content."
  [player-color]
  (let [game (atom (if (= player-color :white)
                    (->GameStart)
                    (make-move (->GameStart))))
        selected-field (atom nil)
        panel (proxy [JPanel] []
                (paintComponent [g]
                  (proxy-super paintComponent g)
                  (let [fs (field-size this)]
                    (doto g
                      (draw-fields fs)
                      (draw-board-border fs)
                      (draw-coordinates fs)
                      (draw-figures fs game)
                      (draw-selected-field fs @selected-field)))))]
    (doto panel
      (.setPreferredSize (Dimension. 450 450))
      (.addMouseListener
       (proxy [MouseAdapter] []
         (mouseClicked [e]
           (let [field (field-clicked e (field-size panel))]
             (when-not (nil? field)
               (if (= player-color (game-color @game))
                 (let [from @selected-field]
                   (if (nil? from)
                     (do
                       (reset! selected-field field)
                       (.repaint panel))
                     (do
                       (reset! selected-field nil)
                       (let [new-game (move from field
                                            (promotion-figure panel @game from field)
                                            @game)]
                         (when (not (nil? new-game))
                           (reset! game new-game)
                           (.repaint panel)
                           (if (is-game-finished new-game)
                             (game-over panel @game)
                             (let [new-game2 (make-move new-game)]
                               (reset! game new-game2)
                               (.repaint panel)))))))))))))))))

(defn app
  "Implements the main application window."
  [playerColor]
  (doto (JFrame. (str "Chess game. Player: " playerColor
                      ". Computer: " (other playerColor) "."))
    (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
    (.add (game-panel playerColor))
    .pack
    (.setVisible true)))

(defn play-white
  "Starts the game.
  The player plays white and the computer plays black figures."
  []
  (app :white))

(defn play-black
  "Starts the game.
  The player plays black and the computer plays white figures."
  []
  (app :black))

(comment

java clojure.main
(use 'chess.gui)
(play-white)

)
