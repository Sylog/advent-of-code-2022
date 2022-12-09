(use '[clojure.string :only [index-of]])

(defn chunks [s]
  (let [r (range 0 (- (count s) 3))]
    (map (fn [i] (take 4 (drop i s))) r)))

(defn unique? [s]
  (= (count (set s)) (count s)))

(defn solve [s]
  (let [subs (first (filter unique? (chunks s)))
        index (index-of s (apply str subs))
        ]
    (+ index 4)))
  
(println (solve (slurp "input.txt")))