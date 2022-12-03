(require ['clojure.string :as 'str])
(require ['clojure.set :as 'nsset])


(defn readFile [fn]
  (def text (slurp fn))
  (partition 3 (str/split-lines text)))

(defn priority [ch]
  (if 
    (Character/isUpperCase ch)    
    (+ (- (int ch) (int \A)) 27)
    (+ (- (int ch) (int \a)) 1)))

(defn priorityGroup [[str1 str2 str3]]
  (let [overlap (nsset/intersection (set str1) (set str2) (set str3))
        prios (map priority overlap)
        ]
  (reduce + prios)))

(defn solve [fn]
  (let [groups (readFile fn)]
    (reduce +
        (map priorityGroup groups))))

(println (solve "sample.txt"))
(println (solve "input.txt"))
