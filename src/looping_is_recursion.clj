(ns looping-is-recursion)

(defn power [base exp]
  (if (= 0 base)
    0
    (let [helper (fn [acc n]
                 (if (<= n 0)
                   acc
                   (recur (* acc base) (dec n))))]
    (helper 1 exp))))

(defn last-element [a-seq]
  (if(empty? a-seq)
    nil
    (let [helper (fn [a]
                   (if (empty? (rest a))
                     (first a)
                     (recur (rest a))))]
      (helper a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq-1 seq-2] (cond
                                   (and (empty? seq-1) (empty? seq-2)) true
                                   (or (empty? seq-1) (empty? seq-2)) false
                                   :else (and (= (first seq-1) (first seq-2)) (recur (rest seq-1) (rest seq-2)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [aseq a-seq
         n 0]
    (if (empty? aseq)
      nil
      (if (pred (first aseq))
        n
        (recur (rest aseq) (inc n))))))

(defn avg [a-seq]
  (loop [n 0
         acc 0
         aseq a-seq]
    (if (empty? aseq)
      (/ acc n)
      (recur (inc n) (+ acc (first aseq)) (rest aseq)))))

(defn toggle [a-set elem]
 (if (contains? a-set elem)
   (disj a-set elem)
   (conj a-set elem)))

(defn parity [a-seq]
  (loop [new-seq #{}
         aseq a-seq]
    (if (empty? aseq)
      new-seq
      (recur (toggle new-seq (first aseq)) (rest aseq)))))

(defn fast-fibo [n]
  (cond
   (= n 0) 0
   (= n 1) 1
   (= n 2) 1
   :else (loop [a 1
                b 1
                x (- n 3)]
      (if (zero? x)
        (+ a b)
        (recur b (+ a b) (dec x))))))

(defn cut-at-repetition [a-seq]
  (loop [aseq a-seq
         new-seq []]
    (if (empty? a-seq)
      []
      (if (empty? aseq)
        new-seq
        (if (some #(= (first aseq) %) new-seq)
         (recur (rest aseq) new-seq)
         (recur (rest aseq) (conj new-seq (first aseq))))))))
