(ns blockchain.core
  (:require 
    [digest]
    [blockchain.block :as block]
    [blockchain.util :refer [natural-numbers]]
    [clojure.spec.alpha :as s]
    [clojure.future :refer :all]))

(defprotocol Blockchain
  (blocks [chain])
  (latest [chain])
  (offer  [chain block])
  (add!   [chain block])
  )

(defrecord InMemoryBlockchain
  [list-atom domain-validation]
  Blockchain

  (latest [chain]
    (-> chain :list-atom deref first))

  (blocks [chain]
    (some-> chain :list-atom deref))

  (offer [chain blk]
    (cond
      (false? (block/valid? blk))
      {:type :error
       :message "Given block is invalid."}
      
      (not (= (:hash (latest chain))
              (:previous blk)))
      {:type :error
       :message "Previous is not referencing latest block in chain."
       :info {:lastest (:hash (latest chain))
              :given   (:previous blk)}}
      
      
      (not (= :ok (:type (domain-validation chain blk))))
      {:type :error
       :message "Failed chain domain validation."
       :domain-validation-result (domain-validation chain blk)}
      
      :otherwise
      {:type :ok}))
  
  (add! [chain blk]
    (let [validation (offer chain blk)]
      (cond
        (= :ok (:type validation))
        (do (swap! (:list-atom chain)
               conj
               blk)
            chain)
        
        :otherwise
        (throw (ex-info "Block not acceptable." validation))))))

(defn strictly-growing-number
  [chain blk]
  (let [before (:body (latest chain))
        genesis? (nil? before)
        nxt (:body blk)]
    (cond
      (not (int? nxt))
      {:type :error
       :message "New block value must be an int."}

      genesis?
      {:type :ok}

      (not (< before nxt))
      {:type :error
       :message "New block value must be strictly bigger than previous."
       :info {:befor before
              :next nxt}}

      :otherwise
      {:type :ok})))

(defn in-mem-chain
  []
  (InMemoryBlockchain. (atom (list (block/genesis)))
                       strictly-growing-number
                       ;;(constantly {:type :ok})
                       ))


(comment
  
  (do
    (def test-chain (in-mem-chain))
    (def b2 (block/sign (create-chain-block test-chain 1)))
    (add! test-chain b2))

  (clojure.pprint/pprint test-chain)
  
  )

(defn create-chain-block
  [blockchain body]
  (block/map->Block {:headers {}
                     :nonce 0
                     :previous (:hash (latest blockchain))
                     :hash ""
                     :body body}))
