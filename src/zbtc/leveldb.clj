(ns zbtc.leveldb)

(use 'clojure.stacktrace)
(use 'zbtc.util)
(use 'zbtc.struct)

;pure java library to access leveldb
(import [org.iq80.leveldb DB DBIterator Options])
(import [org.iq80.leveldb.impl Iq80DBFactory])

; * What K/V represent in bitcoin levelDB ?
; * 
; * Keys have prefixes describing type of value
; * 
; * b - block index
; * b(blockHash) = blockIndex
; *  
; * 
; * c - CCoins
; * c(uint256hash) = serialized CCoins 
; * 
; * B - best chain - this is not a prefix - single B as a key means best chain
; * B = uint256hash
; * 
; * I - best invalid work
; * I = bignumBestInvalidWork
; * 
; * f - block file info
; * f(int*n*File) = CBlockFileInfo
; * where  *n* is which # file this block is stored in (blk?????.dat)
; * 
; * l - last block file
; * l = intLastBlockFile
; * 
; * R - reindexing
; * R = 1
; * 
; * t - tx index
; * t(uint256txid) = CDiskTxPos
; * 
; * F - flag
; * F(name) = 0 or 1 as string
; * e.g.: Ftxindex = 0
; * 

(defn ldb-open [dir]
  (let [options (Options.)]
    (.createIfMissing options true)
    (.open Iq80DBFactory/factory (java.io.File. dir) options)))


(def dbh)
(when (bound? #'dbh) 
  (.close dbh))

(def dbh (ldb-open "blocks/index"))



(defn ldb-readentry "parse given DbEntry into [prefix key value]" [entry]
  (let [prefix (first (.getKey entry))
        k (rest (.getKey entry))
        v (.getValue entry)]
    [(char prefix) k v]))



(defn ldb-parse "return [prefix key value]" [entry]
  (let [[prefix k v] entry]
	  (cond
      ;; Named binary flag
	    ;; \F (name) = (0 or 1)
	    (= prefix \F) [prefix (apply str (map char k)) (apply str (map char v))]
      ;; Block 
	    ;; \b (block hash) = (block index CDiskBlockIndex)
	    (= prefix \b) [prefix (read32 k) (parse-BlockIndex (vec v))]
      ;; Block File Info
      ;; \f (nFile) = (CBlockFileInfo)
      (= prefix \f) [prefix (read-int-le k) (parse-BlockFileInfo (vec v) (read-int-le k))]
      ;; \l = (last block file)
      (= prefix \l) [prefix nil (read-int-le (vec v))]
	    )))

;;read block file info
(map ldb-parse 
  (map ldb-readentry
    (filter
      (fn [entry] (= \f ((ldb-readentry entry) 0)))
      (seq dbh))))


;;read flag
(map ldb-parse
  (map ldb-readentry
       (filter
         (fn [entry] (= \F ((ldb-readentry entry) 0))) 
         (seq dbh))))


;;read block index
(ldb-parse
  (ldb-readentry
    (first (filter
             (fn [entry] (= \b ((ldb-readentry entry) 0))) 
             (seq dbh)))))

;;read last block file number
(ldb-parse
  (ldb-readentry
    (first (filter
             (fn [entry] (= \l ((ldb-readentry entry) 0))) 
             (seq dbh)))))


(count (seq dbh))

