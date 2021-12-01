(ns day-1.core
  (:require [clojure.string :refer [split-lines]]))

(defn parse-input [input] (map read-string (split-lines (slurp input))))

(defn part-one
  ([seq] (part-one (vector (first seq)) (rest seq) 0 0))
  ([done rem incrs decrs]
   (cond
     (empty? rem) {:n-incrs incrs, :n-decrs decrs}
     :else (let [x (last done)
                 y (first rem)
                 incr-if (fn [b, n] (+ n ({true 1, false 0} b)))]
             (part-one
              (conj done y)
              (rest rem)
              (incr-if (< x y) incrs)
              (incr-if (> x y) decrs))))))


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
    (println (part-one input))
    (println (part-two input))))
