(use '[clojure.string :only [index-of]])

(def size 14)

(defn chunks [s]
  (let [r (range 0 (- (count s) (- size 1)))]
    (map (fn [i] (take size (drop i s))) r)))

(defn unique? [s]
  (= (count (set s)) (count s)))

(defn solve [s]
  (let [subs (first (filter unique? (chunks s)))
        index (index-of s (apply str subs))
        ]
    (+ index size)))
  
(println (solve (slurp "input.txt")))