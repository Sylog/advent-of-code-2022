(require ['clojure.string :as 'string])

(defn readFile [fn]
  (string/split-lines (slurp fn)))

(defn parseLine [s]
  (let [[_ s1 e1 s2 e2] (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" s)]
    [[(Integer. s1) (Integer. e1)],[(Integer. s2) (Integer. e2)]]))

(defn fullyContained? [[[s1 e1] [s2 e2]]]
  (or (and (>= s1 s2) (<= e1 e2))
      (and (>= s2 s1) (<= e2 e1))))

(defn solve [fn]
  (count 
   (filter fullyContained?
      (map parseLine (readFile fn)))))

(println (solve "sample.txt"))
(println (solve "input.txt"))
    