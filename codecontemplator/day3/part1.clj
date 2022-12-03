(require ['clojure.string :as 'str])
(require ['clojure.set :as 'nsset])


(defn readFile [fn]
  (def text (slurp fn))
  (def result (str/split-lines text))
  result)

(defn splitInHalf [s]
    (def half (/ (count s) 2))
    [(subs s 0 half) (subs s half)])

(defn priority [ch]
  (if 
    (Character/isUpperCase ch)    
    (+ (- (int ch) (int \A)) 27)
    (+ (- (int ch) (int \a)) 1)))

(defn priorityStr [s]
  (let [[str1 str2] (splitInHalf s)
        overlap (nsset/intersection (set str1) (set str2))
        prios (map priority overlap)
        ]
  (reduce + prios)))

(defn solve [fn]
  (let [rows (readFile fn)]
    (reduce +
        (map priorityStr rows))))

(println (solve "sample.txt"))
(println (solve "input.txt"))
