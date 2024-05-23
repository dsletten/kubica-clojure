;;;;
;;;;
;;;;   One of the nice things about Clojure is that it lets you fix Java
;;;;   -- Rich Hickey
;;;;
;;;;   Name:               sort_test.clj
;;;;
;;;;   Started:            Thu May 16 00:12:41 2024
;;;;   Modifications:
;;;;
;;;;   Purpose:
;;;;
;;;;
;;;;
;;;;   Calling Sequence:
;;;;
;;;;
;;;;   Inputs:
;;;;
;;;;   Outputs:
;;;;
;;;;   Example:
;;;;
;;;;   Notes:
;;;;
;;;;

(ns kubica.sort-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as string]
            [kubica.ch01 :as sort])
  (:import java.util.Arrays))

;; (defn vec-= [v1 v2]
;;   (every? true? (map == v1 v2)))

(defn- random-ordered-array []
  (let [low (rand-int 200)
        high (inc (+ low (rand-int 100)))]
    (long-array (range low high))))

(defn- random-ordered-vector []
  (let [low (rand-int 200)
        high (inc (+ low (rand-int 100)))]
    (vec (range low high))))

(deftest test-insertion-sort
  (are [expected raw]
    (is (Arrays/equals (into-array Number expected)
                       (sort/insertion-sort (into-array Number raw))))

    [] []
    [10] [10]
    [10 20] [10 20]
    [10 20] [20 10]
    [1 1.0 2 3.0 3] [3.0 1 2 3 1.0] ; Stable
    [1 2 3 4 5] [1 2 3 4 5]
    [1 2 3 4 5] [5 4 3 2 1]
    [1/5 1/4 1/3 1/2 1] [1 1/3 1/5 1/2 1/4]
    [4 20 37 61 67 82 85 98] [61 82 67 4 98 20 37 85])
  (dotimes [_ 100]
    (let [a (random-ordered-array)
          b (long-array (shuffle (vec a)))]
      (is (Arrays/equals a (sort/insertion-sort b))))))

(defn- safe-denominator [m]
  (if (ratio? m)
    (denominator m)
    1))

(deftest test-insertion-sort*
  (are [expected raw test]
    (is (Arrays/equals (object-array expected)
                       (sort/insertion-sort* (object-array raw) test)))

    [] [] < 
    [10] [10] < 
    [10 20] [10 20] < 
    [10 20] [20 10] < 
    [1 1.0 2 3.0 3] [3.0 1 2 3 1.0] < ; Stable
    [3.0 3 2 1 1.0] [3.0 1 2 3 1.0] > ; Stable
    [1 2 3 4 5] [1 2 3 4 5] < 
    [1 2 3 4 5] [5 4 3 2 1] < 
    [5 4 3 2 1] [1 2 3 4 5] >
    [5 4 3 2 1] [5 4 3 2 1] > 
    [1/5 1/4 1/3 1/2 1] [1 1/3 1/5 1/2 1/4] < 
    [1 1/2 1/3 1/4 1/5] [1 1/3 1/5 1/2 1/4] >
    [1 1/2 1/3 1/4 1/5] [1 1/3 1/5 1/2 1/4]
    (fn [m n] (< (safe-denominator m) (safe-denominator n)))

    [4 20 37 61 67 82 85 98] [61 82 67 4 98 20 37 85] <

    ["pung" "foo" "baz" "bar"] ["pung" "foo" "bar" "baz"]
    (comp pos? compare)

    ["Pung" "FOO" "baz" "BAR"] ["Pung" "FOO" "BAR" "baz"]
    (fn [a b] (pos? (compare (string/lower-case a) (string/lower-case b))))

    ["bar" "baz" "foo" "pung"] ["pung" "foo" "bar" "baz"]
    (comp neg? compare)

    ["BAR" "baz" "Foo" "pUNG"] ["pUNG" "Foo" "BAR" "baz"]
    (fn [a b] (neg? (compare (string/lower-case a) (string/lower-case b)))) 

    ["foo" "bar" "baz"] ["foo" "bar" "baz"]
    (fn [a b] (< (count a) (count b))) ; Stable

    ["foo" "bar" "baz"] ["foo" "bar" "baz"]
    (fn [a b] (> (count a) (count b))) ; Stable

    [[:z 2] [:k 3] [:p 4] [:a 5] [:b 9]] [[:a 5] [:b 9] [:k 3] [:p 4] [:z 2]]
    (fn [[_ v1] [_ v2]] (neg? (compare v1 v2)))

    [[:a 5] [:b 9] [:k 3] [:p 4] [:z 2]] [[:b 9] [:a 5] [:k 3] [:p 4] [:z 2]]
    (fn [[k1 _] [k2 _]] (neg? (compare k1 k2))))
  (dotimes [_ 100]
    (let [a (random-ordered-array)
          b (long-array (shuffle (vec a)))]
      (is (Arrays/equals a (sort/insertion-sort* b))))))
