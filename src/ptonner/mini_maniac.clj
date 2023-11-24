(ns ptonner.mini-maniac
  (:require [lambda-toolshed.papillon :as pap]
            [flatland.ordered.map :refer [ordered-map]]))

(def ^:dynamic context (atom {}))

(defn context-init []
  {::interceptors (ordered-map)})

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
            (let [guide (get-in ctx [::trace prev])
                  name (::name message)]
              (assoc-in ctx [::message ::value]
                        (get-in guide [name ::value]))))})

(defn message-value
  "Finalize a message if no value has been inserted"
  []
  {:enter (fn [{::keys [message] :as ctx}]
            (assoc-in ctx [::message ::value]
                      (or (::value message)
                          (apply
                           (get message ::fn identity)
                           (::args message)))))})

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

;; API

(defn- process-message "Process a message using provided interceptors"
  [ctx message]
  (pap/execute (-> ctx ::interceptors vals)
               (assoc ctx ::message message)))

(defn- send-message! [message]
  (-> context
      (swap! process-message message)
      (get ::message)))

(defn sample [name fun & {:as args}]
  (-> {::type ::sample
       ::name name
       ::fn fun
       ::args args}
      send-message!
      ::value))

(comment (with-context
           (tap> @context)
           (with-interceptor (trace :guide)
             (with-interceptor (message-value)
               (sample :a identity [0])))
           (tap> @context)
           (with-interceptor (replay :guide)
             (with-interceptor (message-value)
               (sample :a identity [1])))
           (tap> @context)))



