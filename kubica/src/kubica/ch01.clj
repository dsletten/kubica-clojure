;;;;
;;;;
;;;;   With Clojure we found that the very very low friction to get things done enables you to do things that you'd otherwise never even consider
;;;;   -- Orestis Markou
;;;;
;;;;   Name:               ch01.clj
;;;;
;;;;   Started:            Wed May 15 23:59:56 2024
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

(ns kubica.ch01)

;;;
;;;    Nested loop/recur
;;;    
(defn insertion-sort [a]
  (let [n (alength a)]
    (loop [i 1]
      (if (>= i n)
        a
        (let [current (aget a i)]
          (loop [j (dec i)]
            (if (or (neg? j) (<= (aget a j) current))
              (aset a (inc j) current)
              (do (aset a (inc j) (aget a j))
                  (recur (dec j)))))
          (recur (inc i)))) )))

;;;
;;;    loop/recur + helper function
;;;    
(defn insertion-sort [a]
  (let [n (alength a)
        sort-pass (fn [i]
                    (let [current (aget a i)]
                      (loop [j (dec i)]
                        (if (or (neg? j) (<= (aget a j) current))
                          (aset a (inc j) current)
                          (do (aset a (inc j) (aget a j))
                              (recur (dec j)))))))]
    (loop [i 1]
      (if (>= i n)
        a
        (do (sort-pass i)
            (recur (inc i)))))))

;;;
;;;    Recursive top-level + local
;;;    
(defn insertion-sort
  ([a] (insertion-sort a 1))
  ([a i]
   (let [n (alength a)]
     (letfn [(sort-pass [j current]
               (if (or (neg? j) (<= (aget a j) current))
                 (aset a (inc j) current)
                 (do (aset a (inc j) (aget a j))
                     (recur (dec j) current))))]
       (if (>= i n)
         a
         (do (sort-pass (dec i) (aget a i))
             (recur a (inc i))))))))

;;;
;;;    Recursive local functions
;;;    
(defn insertion-sort [a]
  (let [n (alength a)]
    (letfn [(sort [i]
              (if (>= i n)
                a
                (do (sort-pass (dec i) (aget a i))
                    (recur (inc i)))) )
            (sort-pass [j current]
              (if (or (neg? j) (<= (aget a j) current))
                (aset a (inc j) current)
                (do (aset a (inc j) (aget a j))
                    (recur (dec j) current))))]
      (sort 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                          
(defn insertion-sort*
  ([a] (insertion-sort* a <))
  ([a test]
   (let [n (alength a)]
     (loop [i 1]
       (if (>= i n)
         a
         (let [current (aget a i)]
           (loop [j (dec i)]
             (if (or (neg? j) (not (test current (aget a j))))
               (aset a (inc j) current)
              (do (aset a (inc j) (aget a j))
                  (recur (dec j)))))
           (recur (inc i)))) ))))

(defn insertion-sort*
  ([a] (insertion-sort* a <))
  ([a test]
   (let [n (alength a)]
     (letfn [(sort [i]
               (if (>= i n)
                 a
                 (do (sort-pass (dec i) (aget a i))
                     (recur (inc i)))) )
             (sort-pass [j current]
               (if (or (neg? j) (not (test current (aget a j))))
                 (aset a (inc j) current)
                 (do (aset a (inc j) (aget a j))
                     (recur (dec j) current))))]
       (sort 1)))) )

(defn insertion-sort-vec
  ([v] (insertion-sort-vec v <))
  ([v test]
   (let [n (count v)]
     (loop [i 1
            vi v]
       (if (>= i n)
         vi
         (let [current (vi i)
               vi (loop [vj vi
                         j (dec i)]
                    (if (or (neg? j) (not (test current (vj j))))
                      (assoc vj (inc j) current)
                      (recur (assoc vj (inc j) (vj j)) (dec j))))]
           (recur (inc i) vi)))) )))

(defn insertion-sort-vec
  ([v] (insertion-sort-vec v <))
  ([v test]
   (let [n (count v)]
     (letfn [(sort [v i]
               (if (>= i n)
                 v
                 (recur (sort-pass v (dec i) (v i)) (inc i))))
             (sort-pass [v j current]
               (if (or (neg? j) (not (test current (v j))))
                 (assoc v (inc j) current)
                 (recur (assoc v (inc j) (v j)) (dec j) current)))]
       (sort v 1)))) )

(defn insertion-sort-transient
  ([v] (insertion-sort-transient v <))
  ([v test]
   (let [n (count v)
         v* (transient v)]
     (letfn [(sort [v i]
               (if (>= i n)
                 v
                 (recur (sort-pass v (dec i) (v i)) (inc i))))
             (sort-pass [v j current]
               (if (or (neg? j) (not (test current (v j))))
                 (assoc! v (inc j) current)
                 (recur (assoc! v (inc j) (v j)) (dec j) current)))]
       (persistent! (sort v* 1)))) ))

(comment
(def v (shuffle [72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136]))

(time (dotimes [_ 1000] (insertion-sort-transient v)))
"Elapsed time: 91.819539 msecs"
nil
(time (dotimes [_ 1000] (insertion-sort-vec v)))
"Elapsed time: 190.449271 msecs"
nil
(time (dotimes [_ 1000] (insertion-sort* (object-array v))))
"Elapsed time: 22746.946395 msecs"
nil

(def a (object-array v))
(time (dotimes [_ 1000] (insertion-sort* a)))
"Elapsed time: 1415.144721 msecs"
nil

(time (dotimes [_ 1000] (insertion-sort* a (case (rand-int 2) 0 < 1 >))))
"Elapsed time: 24128.348205 msecs"
nil)
