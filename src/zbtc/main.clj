(ns zbtc.main)

(use 'clojure.stacktrace)
(use 'zbtc.util)
(use 'zbtc.struct :reload)
(use 'zbtc.leveldb)

(import 'zbtc.struct.Script)
(import 'zbtc.struct.Aot)

(compile 'zbtc.struct)
(Aot. "111", "222", "333")
 

;;read block index
(ldb-parse
  (ldb-readentry
    (nth (filter
             (fn [entry] (= \b ((ldb-readentry entry) 0))) 
             (seq dbh)) 3)))

(def bb (file-to-seq "blocks/blk00000.dat" 34986475, 6000))
(def bb (file-to-seq "blocks/blk00000.dat" 99971108, 6000))

(def b
  (let [[v block] (parse-Block bb)]
    block))

(println b)


