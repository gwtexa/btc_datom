(ns zbtc.snippet)

;; ===========================================================
;; Calc character frequency in the string 

;exercise: clojure way:
(def cnt frequencies)

;gregs
(defn cnt [s] 
  (letfn [(cnter [result c]
                 (update-in result [c] #(if (nil? %) 1 (inc %)))
                 )]
    (reduce cnter {} s)))

;net 1
(defn cnt [s] 
  (let [a (sort s)
      b (partition-by identity a)
      c (map (juxt first count) b)]
    (sort-by second > c)))

;net 2
(defn cnt [s]
  (reduce #(assoc %1 %2 (inc (get %1 %2 0))) {} s))

;net 3
(defn cnt [s]
  (apply merge-with + (map (fn [c] {c 1}) s)))

(time (cnt 
        (apply str (take 3000000 (repeatedly #(char (+ 97 (rand-int 26))))))
        ))



;; ===============================================================
;; Reading binary file
;; this is idiomatic and we get elegant lazy seq but it's terribly slow
(defn file-to-lazy-seq [raf pos]
    (.seek raf pos)
    ;;do buffering here
    (defn- lazy-file-seq []
	    (lazy-seq
		    (let [b (.read raf)]
		      (if (= b -1)
	          nil
	          (cons b (lazy-file-seq))))))
    (lazy-file-seq))

;(def raf (java.io.RandomAccessFile. "blocks/blk00000.dat" "r"))
;(def fseq (file-to-lazy-seq raf 100))
;(first fseq)
;(count fseq)
;(.close raf)
