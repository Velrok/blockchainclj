(ns blockchain.core
  (:require 
    [digest]
    [clojure.spec.alpha :as s]
    [clojure.future :refer :all]))

(defprotocol BlockchainNetwork
  "A network of blockchain nodes."
  )

(defprotocol Blockchain
  "Blockchain abstraction."
  (blocks [this])
  (validate [this]))

(defrecord InMemoryBlockchain
  []
  Blockchain)

(def natural-numbers (iterate inc 0))

(def block
  {:headers {:content-type "application/edn"}
   :nonce nil
   :previous ""
   :hash ""
   :body ""})

(defn has-valid-hash?
  [blk]
  (.startsWith (:hash blk) "0000"))

(defn with-nounce
  [nounce block]
  (assoc block :nonce nounce))

(defn hash-block
  [blk]
  (assoc blk 
         :hash (-> blk
                   (dissoc :hash)
                   prn-str
                   digest/sha-256)))

(defn sign
  [blk]
  (->> natural-numbers
       (map #(with-nounce % blk))
       (map hash-block)
       (filter has-valid-hash?)
       first))

(defn valid?
  [blk]
  (and (has-valid-hash? blk)
       (= (:hash blk)
          (:hash (hash-block blk)))))

(comment

  (valid? block)

  (valid? signed-block)
  (valid? (assoc signed-block :body "fun"))

  (clojure.pprint/pprint signed-block)
  (hash-block signed-block)

  (def signed-block (sign block))

  (time (sign block))
  
  
  )

(defn in-mem-blockchain
  []
  (InMemoryBlockchain.))


(comment

  (take 3 natural-numbers)

  (filter 
        (for [i natural-numbers
              :when  (= i 10)]
          i))

  (in-mem-blockchain)
  
  (def bc ())
  
  )
