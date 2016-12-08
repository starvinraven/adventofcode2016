(ns adventofcode2016.day-3
  (:require [clojure.string :as string]
            [clojure.math.combinatorics :as combo]))

(defn- load-input
  []
  (map (fn [s] (map (fn [i] (Integer/parseInt i)) (string/split (string/trim s) #"\s+")))
       (string/split (slurp "resources/input.3") #"\n")))

(defn- valid-triangle?
  [sides]
  (let [combos (combo/permutations sides)]
    (every? (fn [[a b c]] (< c (+ a b))) combos)))

(defn run-pt1
  []
  (let [results (map valid-triangle? (load-input))
        valids  (filter identity results)]
    (println "ok" (count valids) "failed" (- (count results) (count valids)))))

(defn- get-elements-at
  [matrix a b c]
  [(nth (nth matrix (second a)) (first a))
   (nth (nth matrix (second b)) (first b))
   (nth (nth matrix (second c)) (first c))])

(defn run-pt2
  []
  (let [input    (load-input)
        rows     (count input)
        elements (partition 3 (for [row (range 0 rows)
                                    col (range 0 3)
                                    :let [iter (quot row 3)]]
                                [(mod row 3) (+ col (* iter 3))]))
        results  (map (fn [[a b c]] (valid-triangle? (get-elements-at input a b c))) elements)
        valids   (filter identity results)]
    (println "ok" (count valids) "failed" (- (count results) (count valids)))))