(ns adventofcode2016.day-9
  (:require [clojure.string :as string]))

(defn- load-input
  []
  "Load as vector of strings (letters) to facilitate processing"
  (mapv str
        (slurp "resources/input.9")))

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYXZ")

(defn is-letter?
  [c]
  (string/includes? letters c))

(def read-input nil)
(defmulti read-input :state)

(defmethod read-input :read-marker
  [{:keys [input idx output] :as acc}]
  (let [parsed-marker    (re-find #"^(\d+)x(\d+)" (subs (apply str input) idx))
        marker-length    (-> parsed-marker (first) (count) (inc))
        num-chars        (Integer/parseInt (second parsed-marker))
        repeat-times     (Integer/parseInt (last parsed-marker))
        str-start-idx    (+ idx marker-length)
        str-end-idx      (+ str-start-idx num-chars)
        str-to-repeat    (subs (apply str input) str-start-idx str-end-idx)
        decompressed-str (apply str (repeat repeat-times str-to-repeat))]
    (merge acc {:output (str output decompressed-str)
                :idx    (+ idx marker-length (count str-to-repeat))
                :state  :read-letter})))

(defmethod read-input :read-letter
  [{:keys [output input idx] :as acc}]
  (let [letter (input idx)]
    (cond (is-letter? letter) (merge acc {:output (str output letter)
                                          :idx    (inc idx)})
          (= "(" letter) (merge acc {:state :read-marker
                                     :idx   (inc idx)}))))

(def initial-state {:state  :read-letter
                    :output ""
                    :input  (load-input)
                    :idx    0})

(defn run
  []
  (loop [acc initial-state]
    (let [{:keys [state output input idx] :as new-state} (read-input acc)]
      (if (= idx (count input))
        (println "end" (count output) ":" output)
        (recur (read-input new-state))))))
