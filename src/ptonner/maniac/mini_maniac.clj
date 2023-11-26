(ns ptonner.maniac.mini-maniac
  (:import [ai.djl.nn AbstractBlock Parameter Parameter$Type])
  (:require [lambda-toolshed.papillon :as pap]
            [flatland.ordered.map :refer [ordered-map]]
            [clj-djl.ndarray :as nd]))

(defn context-init []
  {::interceptors (ordered-map)})

(def ^:dynamic context (atom (context-init)))

;; Interceptors

(defn trace "Trace interceptor to record primitive sites in order"
  ([] (trace ::default))
  ([name]
   {:leave (fn [{::keys [message] :as ctx}]
             (let [message-name (::name message)]
               (update-in ctx [::trace name]
                          (fnil assoc (ordered-map))
                          message-name message)))}))

(defn get-trace "Get the output of a trace"
  [ctx name]
  (get-in ctx [::trace name]))

(defn replay "Replay previous messages"
  [prev]
  {:enter (fn [{::keys [message] :as ctx}]
            (assoc-in ctx [::message ::value]
                      (get-in (get-trace ctx prev)
                              [(::name message) ::value])))})

(defmulti finalize-message
  "Finalize the current message, dispatching on its type"
  #(get-in % [::message ::type]))

(defmethod finalize-message ::sample
  [& {::keys [distribution]}])

(defn get-param "Get the current parameter value, if it exists"
  [ctx name]
  (get-in ctx [::parameters name]))

(defn- set-param "Set parameter value"
  [ctx name param]
  (assoc-in ctx [::parameters name] param))

(defn- make-param [name initial-tensor]
  (doto (.. (Parameter/builder)
            (setName (str name))
            (setType Parameter$Type/OTHER)
            build)
    (.setArray initial-tensor)))

(comment
  (-> (make-param ::tmp (nd/random-normal (nd/new-base-manager) [10 2]))
      (.getName)))

(defmethod finalize-message ::param
  [& {::keys [message] :as ctx}]
  ;; TODO: support for parameter constraints
  (if-let [param (get-param ctx (::name message))]
    (assoc-in ctx [::message ::value] param)
    (let [{::keys [initial-tensor name]} message
          param (make-param name initial-tensor)]
      (-> ctx
          (set-param name param)
          (assoc-in [::message ::value] param)))))

(defn message-value
  "Finalize a message if no value has been inserted

  In minipyro, this is a hard-coded interceptor applied after
  processing the message stack."
  []
  ;; TODO: this should actually be an `:exit`, no? and should always
  ;; be beginning of interceptor chain (so it's `:exit` always goes last)
  ;; NOTE: actually maybe not, at least in `minipyro` it's between the
  ;; `process` and `postprocess` steps
  {:enter
   (fn [ctx] (if (get-in ctx [::message ::value]) ctx
                 (finalize-message ctx)))
   ;; (fn [{::keys [message] :as ctx}]
   ;;   (assoc-in ctx [::message ::value]
   ;;             (or (::value message)
   ;;                 (apply
   ;;                  (get message ::fn identity)
   ;;                  (::args message)))))
   })

(defmacro with-context [& body]
  `(binding [context (atom (context-init))]
     ~@body))

(defmacro with-interceptor [ix & body]
  (let [name (keyword (gensym name))
        ignore (gensym "ignore")
        res (gensym "res")]
    `(let [~ignore (swap! context assoc-in
                          [::interceptors ~name] ~ix)
           ~res (do ~@body)
           ~ignore (swap! context dissoc ::interceptors ~name)]
       ~res)))

;; Probability

;; API

(defn- process-message
  "Process a message using provided interceptors"
  [ctx message]
  (pap/execute (-> ctx ::interceptors vals)
               (assoc ctx ::message message)))

(defn- send-message! [message]
  (-> context
      (swap! process-message message)
      (get ::message)))

(defn sample [name distribution]
  (-> {::type ::sample
       ::name name
       ::distribution distribution}
      send-message!
      ::value))

(defn param [name initial-tensor]
  (-> {::type ::param
       ::name name
       ::initial-tensor initial-tensor}
      send-message!
      ::value))

(comment
  (with-context
    (tap> @context)
    (with-interceptor (trace :guide)
      (with-interceptor (message-value)
        (-> context deref ::interceptors))))

  ;; param
  (let [mgr (nd/new-base-manager)]
    (with-context
      (with-interceptor (message-value)
        (let [p (param ::tmp (nd/zeros mgr [10 10]))]
          (= p (-> context deref (get-param ::tmp)))))))
  (let [mgr (nd/new-base-manager)]
    (with-context
      (with-interceptor (message-value)
        (let [p1 (param ::tmp (nd/zeros mgr [10 10]))
              p2 (param ::tmp (nd/ones mgr [10 10]))]
          [p1 p2]))))
  
  (with-context
    (tap> @context)
    (with-interceptor (trace :guide)
      (with-interceptor (message-value)
        (sample :a identity [0])))
    (tap> @context)
    (with-interceptor (replay :guide)
      (with-interceptor (message-value)
        (sample :a identity [1])))
    (tap> @context))
  
  (with-context
    (tap> @context)
    (with-interceptor (trace :guide)
      (with-interceptor (message-value)
        (sample :a identity [0])))
    (tap> @context)
    (with-interceptor (replay :guide)
      (with-interceptor (message-value)
        (sample :a identity [1])))
    (tap> @context)))



