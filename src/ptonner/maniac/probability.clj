(ns ptonner.maniac.probability
  (:require [clj-djl.ndarray :as nd]
            [clj-djl.engine :as eng]
            [clj-djl.nn :as nn]
            [clj-djl.training :as trn])
  (:import [ai.djl.nn AbstractBlock Parameter Parameter$Type]
           [ai.djl.training ParameterStore]
           [ai.djl.training.optimizer Adam Optimizer]))

(defprotocol DistributionProt
  (batch-shape [d])
  (event-shape [d])
  (log-prob [d x])
  (sample [d sample-shape]))

(defrecord Normal [loc scale]
  DistributionProt
  (event-shape [d] (nd/shape))
  (log-prob [d x]
    (-> )))

(comment

  
  (-> (.. (Parameter/builder)
          (setName "this")
          (setType Parameter$Type/OTHER)
          (optInitializer (nn/normal-initializer))
          (optShape (nd/shape 10 20))
          build)
      (doto (.initialize mgr (nd/datatype :float32)))
      .getArray)
  

  (let [p (proxy [java.io.InputStream] [] (readthis [] -1))]
    (println (.readthis p))
    ;; (println (.read p (byte-array 3) 0 3))
    )
  
  (let [p (proxy [AbstractBlock] [1]
            (forwardInternal [] nil))]
    (.forwardInternal p))
  (.forward )
  (def engine (eng/get-instance))
  (def mgr (nd/new-base-manager))
  (def store (trn/parameter-store mgr false))
  (nn/softplus x)
  (def x (doto (.arange mgr (float 4))
           (nd/set-requires-gradient true)))
  (nd/get-shape x)
  (nd/get-gradient x)
  (with-open [gc (eng/new-gradient-collector engine)]
    (let [y (-> x
                (nd/dot x)
                (nd/mul 2))]
      (trn/backward gc y)))
  (nd/get-gradient x)
  (with-open [gc (eng/new-gradient-collector engine)]
    (let [y (nd/sum x)]
      (trn/backward gc y)))
  (nd/get-gradient x))
