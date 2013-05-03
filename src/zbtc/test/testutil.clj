(ns zbtc.test.testutil)
(use 'zbtc.util)


(defn gen "Generate stream for testing longs encoded with Satoshi compact varint" [] 
  (map #(unchecked-byte %) (cons 255 (repeatedly 8 #(rand-int 256)))))


(defn f3 [coll]
  [(drop 9 coll) (bit-or (read-uint-le (next coll)) (bit-shift-left (read-uint-le (drop 5 coll)) 32))])
  
(doseq [i (range 100)]
(let [g (gen)
      r1 (read-compact-varint g)
      r2 (f3 g)]
  (if (= r1 r2)
    (print \.)
    (println "\n" g r1 r2)
  )))


