(ns minesweeper.core)

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

(defn flattened-pos
  "returns a position in a flattened board"
  [x y]
  (+ (* sizey y) x))

(defn has-mine?
  "takes a board and x,y coordinate, returns true if coordinate has a mine"
  [board x y]
  (contains? (nth (flatten board) (+ (* sizey y ) x)) :mine))

(defn cell-has-mine?
  "returns if a cell (which is a set) has a mine"
  [cell]
  (contains? cell :mine))

(defn nbr-mines-total
  "return the total number of mines on the board or sub board"
  [board]
  (count (filter cell-has-mine? (flatten board))))


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

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
