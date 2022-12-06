(require ['clojure.string :as 'string])
(require ['clojure.core.reducers :as 'reducers])

(defn toIndex [s] (- (Integer. s) 1))

(defn parseLine [s]
  (let [[_ c f t] (re-matches #"move (\d+) from (\d+) to (\d+)" s)]
    [(Integer. c) (toIndex f) (toIndex t)]))

(defn evalLine [stacks [c f t]]
  (let [fromStack (nth stacks f)
       toStack (nth stacks t)
       fromStackTop (take c fromStack)
       fromStackBottom (drop c fromStack)
       toStackFinal (concat fromStackTop toStack)
       intermediate (assoc stacks f fromStackBottom)
       final (assoc intermediate t toStackFinal)
       ]
      final))

(defn solve [initialStacks text]
  (let [lines (map parseLine (string/split-lines text))
        finalStacks (reduce evalLine initialStacks lines)]
     (map first finalStacks)))

(println 
   (solve [[:N :Z] [:D :C :M] [:P]] (slurp "sample.txt")))

(println
   (solve [[:T :R :G :W :Q :M :F :P]
          [:R :F :H]
          [:D :S :H :G :V :R :Z :P]
          [:G :W :F :B :P :H :Q]
          [:H :J :M :S :P]
          [:L :P :R :S :H :T :Z :M]
          [:L :M :N :H :T :P]
          [:R :Q :D :F]
          [:H :P :L :N :C :S :D]] (slurp "input.txt")))