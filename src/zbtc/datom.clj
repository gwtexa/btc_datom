(ns zbtc.datom)

(use 'clojure.stacktrace)
(use 'zbtc.util)
(use 'zbtc.struct)
(use 'zbtc.leveldb)

(import 'zbtc.struct.BlockIndex)

(use '[datomic.api :only [q db] :as d])
(use 'clojure.pprint)

;; store database uri
;;(def uri "datomic:mem://useremail")
(def uri "datomic:free://localhost:4334/zbtc")

(d/create-database uri)
;;(d/delete-database uri)

(def conn (d/connect uri))

(def schema-tx (read-string (slurp "schema/zbtc.dtm")))
@(d/transact conn schema-tx)


(ldb-parse
  (ldb-readentry
    (first (filter
             (fn [entry] (= \b ((ldb-readentry entry) 0))) 
             (seq dbh)))))


(def b-entries 
  (filter
    (fn [entry] (= \b ((ldb-readentry entry) 0)))
    (seq dbh)))

(count b-entries)

(doseq [block-entry (map (comp ldb-parse ldb-readentry) (take 1000 b-entries))]
  (let [[prefix hash block] block-entry
        bh (:blockHeader block)
        hash-bigint (bytes-to-biginteger-le hash)
        exists? (seq (q '[:find ?h :in $ ?h :where [_ :block/hash ?h]] (db conn) hash-bigint))]
    (when-not exists?
		  @(d/transact conn [{
	       ;:db/id (d/tempid :db.part/user)
	       :db/id -1
		     :block/version (:version bh)
		     :block/height (:height block)
		     :block/time (java.util.Date. (:time bh))
	       :block/hash hash-bigint
		     :block/bits (:bits bh)
		     :block/nonce (:nonce bh)
		     :block/hashPrev (bytes-to-biginteger-le (:hashPrev bh))
		     :block/hashMerkleRoot (bytes-to-biginteger-le (:hashMerkleRoot bh))
		     }]))))

(count (q '[:find ?h :where [_ :block/height ?h]] (db conn)))

(seq (q '[:find ?h :where [?b :block/height ?h] [(< ?h 100)]] (db conn)))



;@(d/transact conn [{:block/version 8, :db/id #db/id[db.part/user -2000004]}])

;;when given a db source, finds the names of all the attributes
(def attrs (q '[:find ?name :where [_ :db.install/attribute ?a] [?a :db/ident ?name]] (db conn)))





;; find all users, return entity ids
(def results (q '[:find ?u :where [?u :account/user]] (db conn)))
(count results)

;; get first entity id in results and make an entity map
(def id (ffirst results))
(def entity (-> conn db (d/entity id)))

;; display the entity map's keys
(keys entity)

(class entity)
(:account/email entity)



(def emails (q '[:find ?e :where [?e :email/name]] (db conn)))
(count emails)
(def emailid1 (ffirst emails))
(def email-entity1 (-> conn db (d/entity emailid1)))
(def emailid2 (first (second emails)))
(def email-entity2 (-> conn db (d/entity emailid2)))


(def emails (q '[:find ?e :where [?e :email/name]] (db conn)))

; :account/_email won't work because the underscore notation is only a kind of syntactic sugar for schema definition 
; and there is no such key in your EntityMap

;; get accounts linked to the given email  [?e :email/name "greg@work.com"]
(def linked-accounts (q '[:find ?a :in $ ?e :where [?a :account/email ?e]  ] (db conn) (:db/id email-entity2) ))
(count linked-accounts)


(:db/id email-entity2) (:email/name email-entity2)

(:account/_email email-entity2)

(:
(.get email-entity2 :account/_email)


(q '[:find ?a :in $ ?e :where [?a :account/email ?e]  ] (db conn) (:db/id email-entity2) )


(def acc1 (.entity (.db conn) 17592186045434))
(class acc1)
(def em1set (.get acc1 :account/email))
(class em1set)
(class (first em1set))
(def em1 (first em1set))

(.get em1 ":account/_email")

(keys em1)
(keys acc1)

(.containsKey acc1 ":account/email")
(.containsKey em1 :account/_email)
(.touch em1)
(keys (.cache em1))

(keys (.. em1 (touch) (cache)))


(.get acc1 ":email/_account")
