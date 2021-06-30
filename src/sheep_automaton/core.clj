(ns automaton-clj.core
  (:require [clojure.string :as string]))

(defn dfa [charcters mapping acceptions]
  (let [index (zipmap charcters (range))]
    (fn [state [head & tail]]
      (loop [s state
             h head
             t tail]
        (cond
          (and (mapping s) (index h))
          (recur ((mapping s) (index h))
                 (first t)
                 (rest t))

          (and (nil? h) (acceptions s))
          {:accepted s}

          :else
          {:rejected s
           :rest (string/join (concat h t))})))))

(defn random-mapping [states trans-n]
  (let [n (count states)]
    (zipmap states
            (repeatedly 
             (fn [] 
               (vec (take trans-n (repeatedly #(states (rand-int n))))))))))

(defn random-automaton [characters states]
  (let [random-mapping (random-mapping states (count characters))
        random-acceptions (apply hash-set (random-sample 0.5 states))]
    (println "Transition table:" random-mapping)
    (println "Acception set:" random-acceptions)
    (dfa
     characters
     random-mapping
     random-acceptions)))