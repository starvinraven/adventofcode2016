(ns adventofcode2016.day-5
  (:require [clojure.string :as string]
            [digest :as digest]))

(def input "ffykfhsq")

(defn- find-next-match
  [input-str index]
  (loop [s input-str
         i index]
    (let [d (digest/md5 (str s i))]
      (if (string/starts-with? d "00000")
        [i (nth d 5) (nth d 6)]
        (recur s (inc i))))))

(defn run-pt1
  []
  (loop [password ""
         i        0]
    (let [[new-index char] (find-next-match input i)
          new-password (str password (str char))]
      (println "-" new-index char ":" new-password)
      (if (= (count new-password) 8)
        new-password
        (recur new-password (+ 1 new-index))))))

(defn run-pt2
  []
  (apply str
         (loop [password (vec (repeat 8 nil))
                i        0]
           (let [[new-index pos char] (find-next-match input i)
                 decimal-pos    (read-string (str "0x" pos))
                 is-valid-pos?  (< decimal-pos 8)
                 pos-available? (and
                                  is-valid-pos?
                                  (nil? (nth password decimal-pos)))
                 new-password   (if pos-available?
                                  (assoc password decimal-pos char)
                                  password)]
             (println "-" new-index pos char ":" new-password)
             (if (every? some? new-password)
               new-password
               (recur new-password (inc new-index)))))))