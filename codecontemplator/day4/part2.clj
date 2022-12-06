(require ['clojure.string :as 'string])

(defn readFile [fn]
  (string/split-lines (slurp fn)))

(defn parseLine [s]
  (let [[_ s1 e1 s2 e2] (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" s)]
    [[(Integer. s1) (Integer. e1)],[(Integer. s2) (Integer. e2)]]))

(defn overlap? [[[s1 e1] [s2 e2]]]
  (let [s (- (min e1 e2) (max s1 s2))]
    (>= s 0)))
      
(defn solve [fn]
  (count
   (filter overlap?
      (map parseLine (readFile fn)))))

(println (solve "input.txt"))
    