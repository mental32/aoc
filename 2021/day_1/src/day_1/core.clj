(ns day-1.core
  (:require [clojure.string :refer [split-lines]]))

(defn parse-input [input] (map read-string (split-lines (slurp input))))

(defn part-one
  ([seq] (part-one (vector (first seq)) (rest seq) 0 0))
  ([done rem incrs decrs]
   (cond
     (empty? rem) {:n-incrs incrs, :n-decrs decrs}
     (< (last done) (first rem)) (part-one (conj done (first rem)) (rest rem) (+ incrs 1) decrs)
     (> (last done) (first rem)) (part-one (conj done (first rem)) (rest rem) incrs (+ decrs 1))
     :else (part-one (conj done (first rem)) (rest rem) incrs decrs))))

(defn sum-of-head [seq] (reduce + (take 3 seq)))

(defn part-two
  ([seq] (part-one (part-two [] seq)))
  ([head tail]
   (cond
     (empty? tail) head
     :else (part-two (conj head (sum-of-head tail)) (rest tail)))))

(defn -main
  [& _args]
  (let [input (parse-input "./input")]
    (do
      (println (part-one input))
      (println (part-two input)))))
