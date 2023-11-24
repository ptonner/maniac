(ns ptonner.mini-maniac
  (:require [lambda-toolshed.papillon :as pap]
            [flatland.ordered.map :refer [ordered-map]]))

(def ^:dynamic param-store nil)
(def ^:dynamic maniac-stack nil)
(def ^:dynamic maniac-ctx nil)

;; Interceptors

(defn trace "Trace interceptor to record primitive sites in order"
  [& {:keys [name] :or {name ::default}}]
  {:leave (fn [{::keys [message] :as ctx}]
            (let [message-name (::name message)]
              (update-in ctx [::trace name]
                         (fnil assoc (ordered-map))
                         message-name message)))})

(defn get-trace "Get the output of a trace"
  [ctx name]
  (get-in ctx [::trace name]))

(defn replay "Replay previous messages"
  [prev]
  {:enter (fn [{::keys [message] :as ctx}]
            (let [guide (get-in ctx [::trace prev])
                  name (:name message)]
              (assoc-in ctx [::message ::value]
                        (get guide name))))})

(defn message-value
  "Finalize a message if no value has been inserted"
  []
  {:enter (fn [{::keys [message] :as ctx}]
            (assoc-in ctx [::message ::value]
                      (or (::value message)
                          ((get message ::fn identity)
                           (::args message)))))})

;; API
(defmacro with-stack "Wrap execution with stack"
  [stack & body]
  `(binding [maniac-stack ~stack]
     ~@body))

(defmacro with-ctx [& body]
  `(binding [maniac-ctx (atom {})]
     ~@body))

(defn process-message "Process a message using provided interceptors"
  [ctx ixs message]
  (pap/execute ixs (assoc ctx ::message message)))

(comment
  (process-message {} [(trace) (message-value)] {::fn identity ::args 0 ::name :test}))

(defn sample [name fun & {:as args}]
  (let [message {::type ::sample
                 ::name name
                 ::fn fun
                 ::args args}
        ctx (swap! maniac-ctx process-message maniac-stack message)]
    (get-in ctx [::message ::value])))

(with-ctx
  (with-stack [(message-value)]
    (sample :test)))

(comment
  (defn- process-queue [ctx]
    (if-let [next (some-> ctx ::queue peek)]
      (let [enter (:enter next)
            ctx (if enter (enter ctx) ctx)]
        (recur
         (-> ctx (update ::queue pop)
             (update ::stack (fnil conj []) next))))
      ctx))

  (defn- process-stack [ctx]
    (if-let [next (some-> ctx ::stack peek)]
      (let [exit (:leave next)
            ctx (if exit (exit ctx) ctx)]
        (recur
         (update ctx ::stack pop)))
      ctx))

  (defn process
    "Basic interceptor processing"
    [ctx xtors]
    (-> ctx (assoc ::queue (apply list xtors)) process-queue process-stack (dissoc ::queue ::stack))))

(comment)
(pap/execute [{:enter #(update % :a (fnil inc 0)) :leave (fn [c] (-> c (assoc :b (:a c))))}
              {:enter #(update % :a (fnil * 0) 7) :leave #(dissoc % :c)}
              {:leave #(update % :a (fnil / 0) 5)}])

(comment
  (process {} [{:enter #(update % :a (fnil inc 0)) :leave (fn [c] (-> c (assoc :b (:a c))))}
               {:enter #(update % :a (fnil * 0) 7) :leave #(dissoc % :c)}
               {:leave #(update % :a (fnil / 0) 5) :name :c}]))
