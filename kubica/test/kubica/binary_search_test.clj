;;;;
;;;;
;;;;   To build a brand new language and use lisp syntax on the JVM, you either gotta be a crazy person, or got some really cool ulterior motive. I met Rich and he's not a crazy person.
;;;;   -- Neal Ford
;;;;
;;;;   Name:               binary_search_test.clj
;;;;
;;;;   Started:            Sat Jun  8 22:32:16 2024
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

(ns kubica.binary-search-test
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as string]
            [kubica.ch02 :as bs]))

(deftest test-binary-search
  (let [a [-5 -1 0 3 9 11 15 17 30 35 51 54]]
    (are [index target]
      (is (== index (bs/binary-search a target)))
      
       0 -5
       3  3
       4  9
       8 30
      11 54
      -1 -8
      -1 12
      -1 60))
  (let [a (vec (reverse [-5 -1 0 3 9 11 15 17 30 35 51 54]))]
    (are [index target]
      (is (== index (bs/binary-search a target >)))
      
       0 54
       3 30
       4 17
       8  3
      11 -5
      -1 -8
      -1 12
      -1 60))
  (let [a ["Clojure" "java" "JavaScript" "LISP" "Prolog" "ruby"]
        test (fn [a b] (neg? (compare (string/lower-case a)
                                      (string/lower-case b))))]
    (are [index target]
      (is (== index (bs/binary-search a target test)))

       0 "clojure"
       1 "Java"
       2 "JAVASCRIPT"
       3 "Lisp"
       4 "prolog"
       5 "Ruby"
      -1 "C#")))
