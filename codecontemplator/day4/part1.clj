(require ['clojure.string :as 'string])

(defn readFile [fn]
  (string/split-lines (slurp fn)))

(defn parseLine [s]
  (let [[_ s1 e1 s2 e2] (re-matches #"(\d+)-(\d+),(\d+)-(\d+)" s)]
    [[(Integer. s1) (Integer. e1)],[(Integer. s2) (Integer. e2)]]))

(defn fullyContained? [[[s1 e1] [s2 e2]]]
  (or (and (>= s1 s2) (<= e1 e2))
      (and (>= s2 s1) (<= e2 e1))))

(def lines (readFile "sample.txt"))
(println lines)

(println (parseLine "2-4,6-8"))
(println (fullyContained? (parseLine "2-4,6-8")))
(println (fullyContained? (parseLine "6-8,2-4")))
(println (fullyContained? (parseLine "2-4,4-4")))
(println (fullyContained? (parseLine "2-4,2-2")))
(println (fullyContained? (parseLine "2-5,3-4")))
(println (fullyContained? (parseLine "4-4,2-4")))
(println (fullyContained? (parseLine "2-2,2-4")))
(println (fullyContained? (parseLine "3-4,2-5")))
;(println (map parseLine lines))

(defn solve [fn]
  (count 
   (filter fullyContained?
      (map parseLine (readFile fn)))))

(println (solve "sample.txt"))
(println (solve "input.txt"))
    