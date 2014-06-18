(ns minesweeper.core
  (:require clojure.set))

;;array of sets with border of nil to help with out of bounds stuff
(def board
  [[nil nil            nil      nil            nil      nil]
   [nil #{:mine :flag} nil      nil            #{:mine} nil]
   [nil nil            nil      #{:flag}       nil      nil]
   [nil nil            nil      #{:flag :mine} nil      nil]
   [nil nil            #{:mine} nil            nil      nil]
   [nil nil            nil      nil            nil      nil]])

(def sizex 6)
(def sizey 6)

;; Functions for handling the board

(defn flattened-pos
  "returns a position in a flattened board"
  [x y]
  (+ (* sizey y) x))

(defn cell-at
  "returns a cell at a coordinate"
  [board x y]
  (nth (into [] (flatten board)) (flattened-pos x y)))

(defn has-mine?
  "takes a board and x,y coordinate, returns true if coordinate has a mine"
  [board x y]
  (contains? (nth (flatten board) (flattened-pos x y)) :mine))

(defn cell-has-mine?
  "returns if a cell (which is a set) has a mine"
  [cell]
  (contains? cell :mine))

(defn cell-has-flag?
  "returns if a cell has a flag"
  [cell]
  (contains? cell :flag))

(defn cell-has-boom?
  [cell]
  (contains? cell :boom))

(defn nbr-mines-total
  "return the total number of mines on the board or sub board"
  [board]
  (count (filter cell-has-mine? (flatten board))))

(defn nbr-flags-total
  "returns the number of flags placed"
  [board]
  (count (filter cell-has-flag? (flatten board))))

(defn has-boom?
  "returns true if the board has an exploded mine"
  [board]
  (not= (count (filter cell-has-boom? (flatten board))) 0)
  )


(defn place-flag
  "returns a new board with a new flag placed on the x y coordinate"
  [board x y]
  (let [cell (cell-at board x y)]
    (println cell)
    (assoc (into [] (flatten board)) (flattened-pos x y) (clojure.set/union cell #{:flag}))))

(defn sweep-cell
  "opens a cell, might go boom"
  [board x y]
  (let [cell (cell-at board x y)
        neighbors (neighboring-mines-total board x y)]
    (println cell)
    (println neighbors)
    (if (cell-has-mine? cell)
      (do
        (println "boom!!1")
        (assoc (into [] (flatten board)) (flattened-pos x y) (clojure.set/union cell #{:boom})))
      (assoc (into [] (flatten board)) (flattened-pos x y) (clojure.set/union cell #{:sweeped} #{{:neighbors neighbors}})))))


(defn neighbors
  "returns an array with all the neighboring cells"
  [board x y]
  (let [flat-board (flatten board)]
    (let [neighbors-with-centre (into []
                                      (concat
                                       (subvec (vec flat-board) (- (flattened-pos x y) (+ sizex 1)) (- (flattened-pos x y) (- sizex 2)))
                                       (subvec (vec flat-board) (- (flattened-pos x y) 1) (+ (flattened-pos x y) 2))
                                       (subvec (vec flat-board) (+ (flattened-pos x y) (- sizex 1)) (+ (flattened-pos x y) (+ sizex 2)))
                                       ))]
      (println neighbors-with-centre)
      (into []
            (concat
             (subvec neighbors-with-centre 0 4 ) (subvec neighbors-with-centre 5 9))))))

(defn neighboring-mines-total
  "return the total number of neighboring mines for a position x y on a board"
  [board x y]
  (nbr-mines-total (neighbors board x y)))

(defn print-cell
  "prints a cell"
  [cell]
  (println cell))

(defn print-board
  "prints the board"
  [board]
  (doseq [row (range 0 (- sizey 1))]
    (doseq [column (range 0 (- sizex 1))]
      (print-cell (cell-at board column row)))))

;; Functions for interactions

(defn read-input
  "Converts an input, 'f 2 3', as a :flag on cell at x=2 y=3 "
  []
  (let [input (read-line)]
    (println input)
    (re-seq #"[1-9a-zA-Z]" input)))

(defn make-move
  "Perform the move on the board"
  [board type x y]
  (println type x y)
  (case type
    "f" (place-flag board (Integer. x) (Integer. y))
    "s" (sweep-cell board (Integer. x) (Integer. y)))
  )
  
(defn input-move
  "Ask a user to place a flag or sweep a cell"
  [board]
  (println "Board:")
  (println board)
  (println "Place a flag or sweep a cell")
  (let [move (read-input)]
    (println move)
    (make-move board (first move) (second move) (nth move 2))
    ))


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
