(ns adventofcode2016.day-8
  (:require [clojure.string :as string]
            [clojure.pprint :refer [pprint]]))


(def initial-state (vec (repeatedly 6 #(vec (repeat 50 0)))))

(defn load-input
  []
  (map (fn [row] (string/split row #" ")) (string/split (slurp "resources/input.8") #"\n")))

(defn print-matrix
  [state]
  (doseq [row state]
    (println (map #(if (= % 1) "X" " ") row)))
  (println)
  state)

; hack to fix file reloading (defmulti uses defonce):
(def do-instruction nil)

(defmulti do-instruction
          (fn [instr state]
            (if (= "rotate" (first instr))
              [(first instr) (second instr)]
              [(first instr)])))

(defn set-cell-on
  [state [x y]]
  (assoc-in state [y x] 1))

(defmethod do-instruction ["rect"]
  [instr state]
  (let [dimensions   (second instr)
        [h w] (map read-string (rest (re-matches #"(\d+)x(\d+)" dimensions)))
        cells-to-set (for [x (range h)
                           y (range w)] [x y])]
    (println "rect" w h)
    (reduce set-cell-on state cells-to-set)))

(defn- rotate-row-step
  [state y]
  (let [row         (get state y)
        rotated-row (vec (concat [(last row)] (butlast row)))]
    (assoc-in state [y] rotated-row)))

(defmethod do-instruction ["rotate" "row"]
  [instr state]
  (let [y (Integer/parseInt (last (re-find #"y=(\d+)" (get instr 2))))
        n (Integer/parseInt (get instr 4))]
    (println "rotate row" y n)
    (->> state
         (iterate #(rotate-row-step % y))
         (take (inc n))
         (last)
         (vec))))

(defn- rotate-column-step
  [state x]
  (let [col         (mapv #(get % x) state)
        rotated-col (vec (concat [(last col)] (butlast col)))]
    (map-indexed (fn [i row]
                   (vec (assoc-in row [x] (rotated-col i)))) state)))

(defmethod do-instruction ["rotate" "column"]
  [instr state]
  (let [x (Integer/parseInt (last (re-find #"x=(\d+)" (get instr 2))))
        n (Integer/parseInt (get instr 4))]
    (println "rotate col" x n)
    (->> state
         (iterate #(rotate-column-step % x))
         (take (inc n))
         (last)
         (vec))))

(defmethod do-instruction :default
  [instr state]
  (println "don't know" (first instr))
  state)

(defn run
  []
  (let [result (reduce (fn [state instr]
                         (print-matrix (do-instruction instr state)))
                       initial-state (load-input))]
    (reduce (fn [acc row] (+ acc (apply + row))) 0 result)))