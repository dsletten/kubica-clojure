;;;;
;;;;
;;;;   I think of Clojure as kind of the greatest hits of the last 20 or 30 years of computer science. It's like that mix tape from the Guardians of the Galaxy, only in software.
;;;;   -- Russ Olsen
;;;;
;;;;   Name:               ch02.clj
;;;;
;;;;   Started:            Sat Jun  8 22:27:01 2024
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
;;;;   Notes: https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
;;;;
;;;;

(ns kubica.ch02)

(defn binary-search
  ([a target] (binary-search a target <))
  ([a target test]
   (letfn [(search [low high]
             (if (< high low)
               -1
               (let [mid (long (/ (+ low high) 2))
                     current (a mid)]
                 (cond (test current target) (recur (inc mid) high)
                       (test target current) (recur low (dec mid))
                       :else mid))))]
     (search 0 (dec (count a)))) ))

(defn binary-search
  ([a target] (binary-search a target <))
  ([a target test]
   (loop [low 0
          high (dec (count a))]
     (if (< high low)
       -1
       (let [mid (long (/ (+ low high) 2))
             current (a mid)]
         (cond (test current target) (recur (inc mid) high)
               (test target current) (recur low (dec mid))
               :else mid)))) ))
