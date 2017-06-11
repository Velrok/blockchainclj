(ns blockchain.block
  (:require
    [blockchain.util :refer [natural-numbers]]))

(defprotocol Verifyable
  (valid? [blk])
  (sign [blk]))

(defn- with-nonce
  [blk nonce]
  (assoc blk :nonce nonce))

(defn- rehash [blk]
  (assoc blk :hash 
         (-> blk
             (dissoc :hash)
             prn-str
             digest/sha-256)))

(defn- has-valid-hash?
  [blk]
  (.startsWith (:hash blk) "0000"))

(defrecord Block
  [headers nonce previous hash body]
  Verifyable

  (valid? [blk]
    (= (:hash blk)
       (:hash (rehash blk))))

  (sign [blk]
    (->> natural-numbers
         (map #(with-nonce blk %))
         (map rehash)
         (filter has-valid-hash?)
         first)))

(def genesis
  (memoize #(sign (map->Block 
                    {:headers {}
                     :nonce 0
                     :previous ""
                     :hash ""
                     :body nil}))))
