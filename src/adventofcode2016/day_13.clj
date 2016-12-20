(ns adventofcode2016.day-13
  (:require [loom.alg-generic :refer [dijkstra-path bf-traverse]]))

(def input 1364)
(def start-node [1 1])
(def end-node [31 39])

(defn is-wall?
  [x y]
  (let [str      (Long/toBinaryString (+ (* x x) (* 3 x) (* 2 x y) (* y y) y input))
        num-ones (count (filter (fn [x] (= x \1)) str))
        even?    (= 0 (mod num-ones 2))]
    (not even?)))

(defn neighbors
  [[x y]]
  (filter (fn [[xx yy]]
            (and (not (is-wall? xx yy))
                 (>= xx 0)
                 (>= yy 0)))
          [[(inc x) y]
           [(dec x) y]
           [x (inc y)]
           [x (dec y)]]))

(defn run-pt1
  []
  (let [path (dijkstra-path
               neighbors
               (constantly 1)
               start-node
               end-node)]
    (println (dec (count path)))
    path))

(defn run-pt2
  []
  (count
    (bf-traverse
      neighbors
      start-node
      :when (fn [_ _ depth] (<= depth 50)))))