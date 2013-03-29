(ns primes.core
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as math]))

;; Problem statement:
;; You are given a function ‘secret()’ that accepts a single integer parameter
;; and returns an integer. In your favorite programming language, write a
;; program that determines if this function is an additive function
;; [ secret(x+y) = secret(x) + secret(y) ] for all prime numbers under 100.

(with-test
  (defn- generate-sieve-list [x]
    (doall (apply hash-map (mapcat #(identity [%1 false]) (range 1 (+ x 1))))))
  (is (= (generate-sieve-list 1) {1 false}))
  (is (= (generate-sieve-list 3) {1 false, 2 false, 3 false}))
  (is (= (generate-sieve-list 4) {1 false, 2 false, 3 false, 4 false})))

(defn generate-primes
  "Returns a list of primes up to 'limit', using Sieve of Atkin
   Uses a transient map to store the information to make it more efficient"
  [limit]
  (cond
    ;; Handle the cases that the Sieve of Atkin doesn't handle
    (<= limit 1) []
    (= limit 2) [2]
    (= limit 3) [2 3]

    ;; Run the Sieve of Atkin, using a transient map to speed it up
    :else (let [sieve-list (transient (generate-sieve-list limit))
                sqrt-limit (math/sqrt limit)]
            (loop [x 1]
              (loop [y 1]
                (let [n1 (+ (* 4 x x) (* y y))
                      n2 (+ (* 3 x x) (* y y))
                      n3 (- (* 3 x x) (* y y))]
                  (when (and (<= n1 limit)
                             (or (= (mod n1 12) 1)
                                 (= (mod n1 12) 5)))
                    (assoc! sieve-list n1 true))
                  (when (and (<= n2 limit) (= (mod n2 12) 7))
                    (assoc! sieve-list n2 true))
                  (when (and (> x y) (<= n3 limit) (= (mod n3 12) 11))
                    (assoc! sieve-list n3 true)))
                (when (<= (+ y 1) sqrt-limit) (recur (+ y 1))))
              (when (<= (+ x 1) sqrt-limit) (recur (+ x 1))))
            (concat [2 3]
                    (sort (map #(first %)
                               (filter #(second %)
                                       (persistent! sieve-list))))))))

(deftest test-generate-primes
  ;; Make sure to test edge cases: -1 through 5
  (is (= (generate-primes -1) []))
  (is (= (generate-primes 0) []))
  (is (= (generate-primes 1) []))
  (is (= (generate-primes 2) [2]))
  (is (= (generate-primes 3) [2 3]))
  (is (= (generate-primes 4) [2 3]))
  (is (= (generate-primes 5) [2 3 5]))
  (is (= (generate-primes 19) [2 3 5 7 11 13 17 19]))
  (is (= (generate-primes 100)
         [2 3 5 7 11 13 17 19 23 25 29 31 37 41 43 47 53 59
          61 65 67 71 73 79 83 85 89 91 97])))

(defn is-additive
  "Test if a function 'secret' is additive:
      secret(x+y) = secret(x) + secret(y)"
  ([secret] (is-additive secret 100))
  ([secret limit]
     (let [primes-limit (generate-primes limit)]
       (every? true?
               (for [x primes-limit
                     y primes-limit]
                 (= (secret (+ x y))
                    (+ (secret x) (secret y))))))))

(deftest test-is-additive
  (is (is-additive #(identity %)))
  (is (is-additive #(* % 2)))
  (is (is-additive #(- %)))
  (is (is-additive #(if (< % 1000) (identity %) 1000)))
  (is (not (is-additive (fn [x] 1))))
  (is (not (is-additive #(math/expt 2 %))))
  (is (not (is-additive #(math/sqrt %))))
  (is (not (is-additive #(if (< % 1000) (identity %) 100) 1000))))

(defn secret 
  "The secret function"
  [x]
  (if (< x 1000) 
    (identity x) 
    1000))

(defn -main
  ([] (-main "100")) ;; If no argument provided, use 100
  ([limit]
     (if (is-additive secret (Integer/parseInt limit))
       ;; Output whether the secret function is additive over 
       ;; the prime limit specified by the argument
       ;; Using ANSI colors, of course
       (println "\033[32mThe secret function IS additive!\033[0m")
       (println "\033[31mThe secret function IS NOT additive!\033[0m"))))
