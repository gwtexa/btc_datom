(ns zbtc.util)


(use 'clojure.stacktrace)

(defn ns-clean
       "Remove all internal mappings from a given name space or the current one if no parameter given."
   ([] (ns-clean *ns*)) 
   ([ns] (map #(ns-unmap ns %) (keys (ns-interns ns)))))


(defn classpath "Get current classpath" [] (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))


(defn bytes-to-biginteger "convert byte array to java BigInteger" [bytes]
  (java.math.BigInteger. bytes))

(defn bytes-to-biginteger-le "convert byte array to java BigInteger - little endian" [bytes]
  (bytes-to-biginteger (byte-array (reverse bytes))))


(defn to-unsigned-int [n]
  (bit-and n 0x0ffffffff))

(defn to-unsigned-long [n]
  (bit-and (unchecked-long n) (unchecked-long 0x0ffffffffffffffff)))


(defn iter-seq "Create lazy sequence from java Iterable"
  ([iterable] 
    (iter-seq iterable (.iterator iterable)))
  ([iterable i] 
    (lazy-seq 
      (when (.hasNext i)
        (cons (.next i) (iter-seq iterable i))))))


(defn hexify "Convert byte sequence to hex string" [coll]
  (let [hex [\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f]]
	  (letfn [(hexify-byte [b]
	    (let [v (bit-and b 0xFF)]
	      [(hex (bit-shift-right v 4)) (hex (bit-and v 0x0F))]))]
	    (apply str (mapcat hexify-byte coll)))))

(defn hexify-str [s]
  (hexify (.getBytes s)))

(defn hexify-le "hexify little-endian" [coll] 
  (hexify (reverse coll)))



(defn unhexify "Convert hex string to byte sequence" [s] 
	  (letfn [(unhexify-2 [c1 c2] 
	             (unchecked-byte 
	               (+ (bit-shift-left (Character/digit c1 16) 4)
	                  (Character/digit c2 16))))]
     (map #(apply unhexify-2 %) (partition 2 s))))

(defn unhexify-str [s]
  (apply str (map char (unhexify s)))) 

(defn unhexify-le "unhexify little-endian" [s]
  (reverse (unhexify s)))
  



(defn read-int [coll]
  (let [bytes4 (into-array java.lang.Byte/TYPE (take 4 coll))
        bbuf (java.nio.ByteBuffer/wrap bytes4)]
    (.getInt bbuf)))

(defn read-uint [coll]
  (to-unsigned-int (read-int coll)))

(defn read-int-le [coll]
  (let [bytes4 (into-array java.lang.Byte/TYPE (take 4 coll))
        bbuf (java.nio.ByteBuffer/wrap bytes4)
        bbuf (.order bbuf java.nio.ByteOrder/LITTLE_ENDIAN)]
    (.getInt bbuf)))

(defn read-uint-le [coll]
  (to-unsigned-int (read-int-le coll)))

(defn stream-read-int [coll]
  [(nthrest coll 4) (read-int coll)])

(defn stream-read-uint [coll]
  [(nthrest coll 4) (read-uint coll)])

(defn stream-read-int-le [coll]
  [(nthrest coll 4) (read-int-le coll)])

(defn stream-read-uint-le [coll]
  [(nthrest coll 4) (read-uint-le coll)])



(defn read-long [coll]
  (let [bytes8 (into-array java.lang.Byte/TYPE (take 8 coll))
        bbuf (java.nio.ByteBuffer/wrap bytes8)]
    (.getLong bbuf)))

(defn read-ulong [coll]
  (to-unsigned-long (read-long coll)))

(defn read-long-le [coll]
  (let [bytes8 (into-array java.lang.Byte/TYPE (take 8 coll))
        bbuf (java.nio.ByteBuffer/wrap bytes8)
        bbuf (.order bbuf java.nio.ByteOrder/LITTLE_ENDIAN)]
    (.getLong bbuf)))

(defn read-ulong-le [coll]
  (to-unsigned-long (read-long-le coll)))

(defn stream-read-long [coll]
  [(nthrest coll 8) (read-long coll)])

(defn stream-read-ulong [coll]
  [(nthrest coll 8) (read-ulong coll)])

(defn stream-read-long-le [coll]
  [(nthrest coll 8) (read-long-le coll)])

(defn stream-read-ulong-le [coll]
  [(nthrest coll 8) (read-ulong-le coll)])

(defn long-to-bytes [n]
  (let [bbuf (java.nio.ByteBuffer/allocate 8)]
    (.array (.putLong bbuf n))))

(defn long-to-bytes-le [n]
  (let [bbuf (java.nio.ByteBuffer/allocate 8)
        bbuf (.order bbuf java.nio.ByteOrder/LITTLE_ENDIAN)]
    (.array (.putLong bbuf n))))


(defn read-unsigned-byte [coll]
  (bit-and 0xff (first coll)))

(defn stream-read-unsigned-byte [coll]
  [(rest coll) (bit-and 0xff (first coll))])

(defn read32 [coll]
  (take 32 coll))

(defn stream-read32 [coll]
  [(nthrest coll 32) (take 32 coll)])

(defn read-hash256 [coll] 
  (hexify-le (take 32 coll)))

(defn stream-read-hash256 "Return [tail of coll, hex hash]" [coll] 
  [(nthrest coll 32) (hexify-le (take 32 coll)) ])



(defn read-pod-varint "VarInt for serializing arrays and POD (CFlatData) - script compress/decompress. Return [tail of coll, decoded int] " 
  ([coll] (read-pod-varint coll 0))
  ([coll n] 
    (let [c (first coll)
          n (bit-or (bit-shift-left n 7) (bit-and c 0x7f))]
      (if (zero? (bit-and c 0x80))
        [(rest coll) n]
        (recur (rest coll) (inc n))))))

(defn read-pod-varuint [coll]
  (let [[v n] (read-pod-varint coll)]
    [v (to-unsigned-int n)]))
  


(defn read-compact-varint "decode varint compact Satoshi encoding" [coll]
  (let [b1 (bit-and 0xff (first coll))]
    (cond
      (< b1 253) [(next coll) b1]
      (= b1 253) [(drop 3 coll) (bit-or (bit-and 0xff (nth coll 2)) (bit-and 0xff (bit-shift-left (nth coll 3) 8)))]
      (= b1 254) [(drop 5 coll) (read-uint-le (next coll))]
      (= b1 255) [(drop 9 coll) (read-ulong-le (next coll))]
      ;alternative way:
      ;(= b1 255) [(drop 9 coll) (bit-or (read-uint-le (next coll)) (bit-shift-left (read-uint-le (drop 5 coll)) 32))]
      )))


;; This is high performance
(defn file-to-array "Read n bytes of file starting from pos and return as byte array" [f pos n]
  (with-open [fis (java.io.FileInputStream. f)
              fc (.getChannel fis)]
    
    ;;The contents of direct buffers may reside outside of the normal garbage-collected heap
    ;;The advantage is that MappedByteBuffer allows for random access
    (let [nlimited (min n (- (.length (java.io.File. f)) pos))
          buf (.map fc java.nio.channels.FileChannel$MapMode/READ_ONLY pos nlimited)  
          bytes (make-array java.lang.Byte/TYPE nlimited)]
      (while (.hasRemaining buf)
        (.get buf bytes))
      bytes
      )))

(defn file-to-vec [f pos n]
  (vec (file-to-array f pos n)))

(defn file-to-seq [f pos n]
  (seq (file-to-array f pos n)))

;(aget (file-to-array "blocks/blk00000.dat" 40000 600000) 800) ;; really fast



