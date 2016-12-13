(ns adventofcode2016.day-12
  (:require [clojure.string :as string]))

(defn load-input
  []
  (mapv #(string/split % #"\s")
        (string/split (slurp "resources/input.12") #"\n")))

(def initial-state
  {:input     (load-input)
   :idx       0
   :registers {:a 0
               :b 0
               :c 1
               :d 0}})

(def read-instr nil)
(defmulti read-instr (fn [_ instr]
                       (-> instr
                           (first)
                           (keyword))))

(defmethod read-instr :cpy
  [state instr]
  (let [value-key (instr 1)
        value     (if (re-matches #"\d+" value-key)
                    (Integer/parseInt value-key)
                    (get-in state [:registers (keyword value-key)]))
        register  (keyword (instr 2))]
    (-> state
        (assoc-in [:registers register] value)
        (update-in [:idx] inc))))

(defmethod read-instr :jnz
  [state instr]
  (let [register     (keyword (instr 1))
        register-val (get-in state [:registers register])
        jmpval       (Integer/parseInt (instr 2))]
    (if (not= 0 register-val)
      (update-in state [:idx] (partial + jmpval))
      (update-in state [:idx] inc))))

(defmethod read-instr :inc
  [state instr]
  (let [register (keyword (instr 1))]
    (-> state
        (update-in [:registers register] inc)
        (update-in [:idx] inc))))

(defmethod read-instr :dec
  [state instr]
  (let [register (keyword (instr 1))]
    (-> state
        (update-in [:registers register] dec)
        (update-in [:idx] inc))))

(defn run
  []
  (loop [{:keys [input idx] :as state} initial-state]
    (if (<= (count input) idx)
      (println "end" (:registers state))
      (recur (read-instr state (input idx))))))
