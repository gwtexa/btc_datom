(ns zbtc.struct)

(use 'zbtc.util)
;(require '[zbtc.util :refer :all])

;; CBlockHeader
;; used both in leveldb and dat
(defrecord BlockHeader [
  version
  hashPrev  ;;Block header prev tx hash
  hashMerkleRoot 
  time 
  bits ;; difficulty target 
  nonce
  ])

(defmethod clojure.core/print-method BlockHeader [t writer]
  (.write writer (apply str "BlockHeader\n        " (interpose "\n        " t))))


;; CBlockIndex
;; used only in leveldb
(defrecord BlockIndex [
  version
  height  ;;height of the entry in the chain. The genesis block has height 0
	status  ;;Verification status of this block. See enum BlockStatus
  txCount  ;;Number of transactions in this block.
	fileNo  ;;Which # file this block is stored in (blk?????.dat)
	dataPos  ;;Byte offset within blk?????.dat where this block's data is stored
	undoPos  ;;Byte offset within rev?????.dat where this block's undo data is stored
  blockHeader ;;BlockHeader
  ])

(defmethod clojure.core/print-method BlockIndex [t writer]
  (.write writer (apply str "BlockIndex\n    " (interpose "\n    " t))))


;; CBlockFileInfo
;; used only in leveldb
(defrecord BlockFileInfo [
  fileNo ;;dat file number                          
  blocks ;;number of blocks stored in file
  size  ;;number of used bytes of block file
  undoSize ;;number of used bytes in the undo file
  heightFirst ;;lowest height of block in file
	heightLast  ;;highest height of block in file
	timeFirst ;;earliest time of block in file
	timeLast ;;latest time of block in file
  ])


;;COutPoint - An outpoint - a combination of a transaction hash and an index n into its vout
(defrecord OutPoint [hashTx outIndex])

(defmethod clojure.core/print-method OutPoint [t writer]
  (.write writer (apply str "OutPoint\n                " (interpose "\n                " t)))) 


;;CScript - Serialized script, used inside transaction inputs and outputs 
(defrecord Script [size script])

;; override toString on Script record
(defmethod clojure.core/print-method Script [scr writer]
  (.write writer (str "Script of size " (:size scr))))

;; CTxIn - An input of a transaction.  It contains the location of the previous
;; transaction's output that it claims and a signature that matches the output's public key.
(defrecord TxIn [
  outPoint ;;OutPoint
  script ;;Script record
  sequence
  ])

(defmethod clojure.core/print-method TxIn [t writer]
  (.write writer (apply str "\n        TxIn\n            " (interpose "\n            " t)))) 



;; CTxOut - An output of a transaction.  
;; It contains the public key that the next input must be able to sign with to claim it.
(defrecord TxOut [amount script])

(defmethod clojure.core/print-method TxOut [t writer]
  (.write writer (apply str "\n        TxOut\n            " (interpose "\n            " t)))) 


;; CTransaction - The basic transaction that is broadcasted on the network and contained in blocks
;; A transaction can contain multiple inputs and outputs.
(defrecord Tx [
  version
  txInCount
  txins
  txOutCount
  txouts
  lockTime
  ])

(defmethod clojure.core/print-method Tx [t writer]
  (.write writer (apply str "\n    Tx\n        " (interpose "\n        " t)))) 




;; CBlock
(defrecord Block [blockHeader txCount txs])

(defmethod clojure.core/print-method Block [t writer]
  (.write writer (apply str "Block\n    " (interpose "\n    " t)))) 






(defn parse-BlockHeader [coll]
	  (let [v coll
         [v version] (stream-read-uint-le v) ;;Block header version
         [v hashPrev] (stream-read32 v)  ;;Block header prev tx hash
         [v hashMerkleRoot] (stream-read32 v) ;;Block header 
         [v time] (stream-read-uint-le v)  ;;Block header 
         [v bits] (stream-read-uint-le v)  ;;Block header, difficulty target 
         [v nonce] (stream-read-uint-le v)]  ;;Block header
             [v (BlockHeader. version hashPrev hashMerkleRoot time bits nonce)]))


;; BlockIndex is used to marshal pointers into hashes for db storage
(defn parse-BlockIndex "returns BlockIndex only without vector tail" [coll]
	  (let [v coll
         [v version] (read-pod-varint v)
         [v height] (read-pod-varint v)  ;;height of the entry in the chain. The genesis block has height 0
	       [v status] (read-pod-varint v)  ;;Verification status of this block. See enum BlockStatus
         [v txCount] (read-pod-varint v)  ;;Number of transactions in this block.
	       [v fileNo] (read-pod-varint v)  ;;Which # file this block is stored in (blk?????.dat)
	       [v dataPos] (read-pod-varint v)  ;;Byte offset within blk?????.dat where this block's data is stored
	       [v undoPos] (read-pod-varint v)  ;;Byte offset within rev?????.dat where this block's undo data is stored
         [v blockHeader] (parse-BlockHeader v)]
           (BlockIndex. version height status txCount fileNo dataPos undoPos blockHeader)))




(defn parse-BlockFileInfo "returns BlockFileInfo only without vector tail" [coll fileNo]
	  (let [v coll
         [v blocks] (read-pod-varint v) ;;number of blocks stored in file
         [v size] (read-pod-varint v)  ;;number of used bytes of block file
	       [v undoSize] (read-pod-varint v)  ;;number of used bytes in the undo file
         [v heightFirst] (read-pod-varint v)  ;;lowest height of block in file
	       [v heightLast] (read-pod-varint v)  ;;highest height of block in file
	       [v timeFirst] (read-pod-varint v) ;;earliest time of block in file
	       [v timeLast] (read-pod-varint v)] ;;latest time of block in file
            (BlockFileInfo. fileNo blocks size undoSize heightFirst heightLast timeFirst timeLast)))




(defn parse-OutPoint [coll]
	  (let [v coll
         [v hashTx] (stream-read32 v)
         [v outIndex] (stream-read-int-le v)]
     [v (OutPoint. hashTx outIndex)]))


(defn parse-Script [coll]
	  (let [v coll
         [v size] (stream-read-unsigned-byte v)
         [v script] [(nthrest v size) (take size v)]]
     [v (Script. size script)]))


(defn parse-TxIn [coll]
	  (let [v coll
         [v outPoint] (parse-OutPoint v)
         [v script] (parse-Script v)
         [v sequence] (stream-read-int-le v)]
     [v (TxIn. outPoint script sequence)]))


(defn parse-list "from byte stream v parse list on n elements to be parsed using f; return [stream tail, list of records]" 
  ([v n f] (parse-list v n f ()))
  ([v n f lst]
	  (if (zero? n)
	    [v lst]
	    (let [[v rec] (f v)]
	      (recur v (dec n) f (conj lst rec))))))




(defn parse-TxOut [coll]
	  (let [v coll
         [v amount] (stream-read-long-le v)
         [v script] (parse-Script v)]
     [v (TxOut. amount script)]))
     
;(defn parse-txouts [coll n]
;  (if (zero? n)
;    coll
;    (let [[v nAmount nSize script] (dat-parse-txout coll)]
;      (recur v (dec n)))))
                                       




(defn parse-Tx [coll]
	  (let [v coll
         [v version] (stream-read-int-le v)
         [v txInCount] (read-compact-varint v)
         [v txins] (parse-list v txInCount parse-TxIn)
         [v txOutCount] (read-compact-varint v)
         [v txouts] (parse-list v txOutCount parse-TxOut)
         [v lockTime] (stream-read-int-le v)]
     [v (Tx. version txInCount txins txOutCount txouts lockTime)]))


(defn parse-Block [coll]
  (let [v coll
        [v blockHeader] (parse-BlockHeader v)
        [v txCount] (read-compact-varint v)
        [v txs] (parse-list v txCount parse-Tx)]
    [v (Block. blockHeader txCount txs)]))




;;for nSpecialSize - not used for now
(defn get-script-special-size [nSize]
  nSize)
;  (cond
;    (#{0 1} nSize) 20
;    (#{2 3 4 5} nSize) 32
;    :else (- nSize 6)))
;    :else nSize)) ;;TODO according to c++ code correct should be above


(defrecord Aot [aa bb cc])
  
