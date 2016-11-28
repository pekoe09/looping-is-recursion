(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n k]
                 (if (zero? k)
                   acc
                   (recur (* acc n) n (dec k))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [n-seq a-seq
         i 0]
    (cond
      (empty? n-seq) nil
      (pred (first n-seq)) i
      :else (recur (rest n-seq) (inc i)))))

(defn avg [a-seq]
  (loop [k 0
         i 0
         n-seq a-seq]
    (cond
      (empty? n-seq) (/ k i)
      :else (recur (+ k (first n-seq)) (inc i) (rest n-seq)))))

(defn parity [a-seq]
  (loop [n-set #{}
         n-seq a-seq]
    (cond
      (empty? n-seq) n-set
      :else (recur
              (if (contains? n-set (first n-seq))
                (disj n-set (first n-seq))
                (conj n-set (first n-seq)))
              (rest n-seq)))))

(defn fast-fibo [n]
  (cond
    (= 0 n) 0
    (= 1 n) 1
    :else (loop [f-n 1
                 f-n-1 0
                 k 2]
            (cond
              (= k n) (+ f-n f-n-1)
              :else (recur (+ f-n f-n-1) f-n (inc k))))))

(defn cut-at-repetition [a-seq]
  (loop [n-vector []
         n-seq a-seq]
    (cond
      (empty? n-seq) n-vector
      (contains? (set n-vector) (first n-seq)) n-vector
      :else (recur (conj n-vector (first n-seq)) (rest n-seq)))))

