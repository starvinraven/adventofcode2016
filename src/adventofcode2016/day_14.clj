(ns adventofcode2016.day-14
    (:require [clojure.string :as string]
      [digest :as digest]))

(def input "ihaygndm")

(def initial-state
  {:input         input
   :three-matches []
   :found-keys    []
   :idx           0})

(defn find-matches
      [threes fives]
      (filter
        (fn [h] (some fives h))
        threes))

(defn- run
      [hash-fn]
      (loop [{:keys [input three-matches idx found-keys] :as state} initial-state]
            (let [hash (hash-fn input idx)
                  new-three-matches (set (map second (re-seq #"(.)\1\1" hash)))
                  new-five-matches (set (map second (re-seq #"(.)\1\1\1\1" hash)))
                  actual-matches (find-matches three-matches new-five-matches)]
                 (if (<= 64 (count found-keys))
                   (println (nth found-keys 65))
                   (recur
                     {:three-matches (filter (fn [m]
                                               (and
                                                 (not (some #(= (first m) (first %)) actual-matches))
                                                 (< (- idx (first m)) 1000)))
                                             (into three-matches
                                                   (map (fn [m] [idx m])
                                                        new-three-matches)))
                      :found-keys    (into found-keys (map (fn [[a b]] [a idx b]) actual-matches))
                      :idx           (inc idx)
                      :input         input})))))

(defn run-pt1
      []
      (run (fn [input idx] (digest/md5 (str input idx)))))

(defn run-pt2
      []
      (run (fn [input idx]
               (nth (iterate digest/md5 (str input idx)) 2017))))