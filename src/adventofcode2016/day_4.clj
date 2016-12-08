(ns adventofcode2016.day-4
  (:require [clojure.string :as string]))

(defn- load-input
  []
  (string/split (slurp "resources/input.4") #"\n"))

(defn- sector-id-for-valid-room
  [s]
  (let [letters              (sort (string/replace (re-find #"[^\d]+" s) "-" ""))
        sector-id            (Integer/parseInt (re-find #"\d+" s))
        check-str-found      (second (re-find #"\[(\w+)\]" s))
        freqs                (sort-by identity
                                      (fn [a b]
                                        (let [length-diff (- (count b) (count a))]
                                          (if (not= length-diff 0)
                                            length-diff
                                            0)))
                                      (partition-by identity letters))
        check-str-calculated (apply str (take 5 (map first freqs)))
        valid?               (= check-str-found check-str-calculated)]
    (println "got" s sector-id check-str-found "vs" check-str-calculated " = " valid?)
    (if valid?
      sector-id
      0)))

(defn run-pt1
  []
  (let [input        (load-input)
        valid-id-sum (reduce + (map sector-id-for-valid-room input))]
    (println "sum" valid-id-sum)))

(def letters "abcdefghijklmnopqrstuvwxyz")

(defn- rotate-letters
  [s]
  (let [name         (re-find #"[^\d]+" s)
        sector-id    (Integer/parseInt (re-find #"\d+" s))
        rotate-by    (mod sector-id (count letters))
        rotated-name (reduce (fn [acc letter]
                               (if (= letter \-)
                                 (str acc " ")
                                 (let [letter-idx (string/index-of letters letter)
                                       new-idx    (mod (+ rotate-by letter-idx) (count letters))
                                       new-letter (str (nth letters new-idx))]
                                   (str acc new-letter)))) "" name)
        name-with-id (str rotated-name ": " sector-id)]
    name-with-id))

(defn run-pt2
  []
  (let [input         (load-input)
        rotated-names (map rotate-letters input)]
    (doseq [rotated-name rotated-names]
      (if (string/includes? rotated-name "north")
        (println rotated-name)))))