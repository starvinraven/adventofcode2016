(ns adventofcode2016.day-6
    (:require [clojure.string :as string]))

(defn load-input
      []
      (string/split (slurp "resources/input.6") #"\n"))

(def str-length 8)

(defn- get-frequent
       [selector-fn m]
       (println "maek map" m)
       (key
         (selector-fn
           (into
             (sorted-map-by (fn [a b]
                                (compare
                                  [(get m a)]
                                  [(get m b)]))) m))))

(def get-most-frequent (partial get-frequent last))
(def get-least-frequent (partial get-frequent first))

(defn- get-frequencies
      [strings selector-fn i]
      (let [freqs (frequencies
              (map #(nth % i) strings))]
           (selector-fn freqs)))

(defn- run
       [selector-fn]
       (apply str
              (map
                (partial get-frequencies (load-input) selector-fn)
                (range str-length))))

(defn run-pt1
      []
      (run get-most-frequent))

(defn run-pt2
      []
      (run get-least-frequent))

