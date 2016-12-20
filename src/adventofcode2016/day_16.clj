(ns adventofcode2016.day-16
  (:require [clojure.string :as string]))

(def ^:static input "01111001100111011")
(def ^:static length-to-fill-pt-1 272)
(def ^:static length-to-fill-pt-2 35651584)

(defn- create-dragon
  [initial length]
  (loop [a initial]
    (let [b          (-> a
                         (string/reverse)
                         (string/replace #"0" "x")
                         (string/replace #"1" "0")
                         (string/replace #"x" "1"))
          new-output (str a "0" b)]
      (if (>= (count new-output) length)
        (subs new-output 0 length)
        (recur new-output)))))

(defn- create-checksum
  [input]
  (loop [a input]
    (let [pairs  (map #(apply str %) (partition 2 a))
          result (apply str (map
                              (fn [[a b]]
                                (if (= a b)
                                  "1"
                                  "0"))
                              pairs))]
      (if (-> result
              (count)
              (odd?))
        result
        (recur result)))))

(defn- run
  [length-to-fill]
  (-> (create-dragon input length-to-fill)
      (create-checksum)))

(def run-pt1 (partial run length-to-fill-pt-1))
(def run-pt2 (partial run length-to-fill-pt-2))