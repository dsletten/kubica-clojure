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
