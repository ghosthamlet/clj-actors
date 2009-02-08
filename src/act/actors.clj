(ns act.actors)

(defmacro !
  "Send a message to an actor, with arguments."
  [ag msg & args]
  `(let [ag# ~ag] (send-off ag# (:handler @ag#) ~msg ~@args)))

(defmacro arecur [st msg & args]
  "Within an actor, send a message to itself given its state."
  `(let [st# ~st] (send-off (:self st#) (:handler st#) ~msg ~@args) st#))

(defn- msecond [_ x & _] x)
(defn actor
  "Creates an actor given an initial state, a map.
Returns an agent over this state and additional fields:
  :handler - refers to the MultiFn extended by defhandle
  :self - refers to the agent itself.

Extend the actor with defhandle; send messages to it with
! ;use arecur within a handler to loop."
  [#^PersistentArrayMap st]
  (let [handler (new clojure.lang.MultiFn msecond :default)
        ag (agent (conj st {:handler handler}))]
    (send ag conj {:self ag})
    (await ag)
    ag))

(defmacro defhandle
  "Add a handler to an actor."
  [act msg arglst & body]
  (let [arglst2 (vec (concat [(first arglst) '_] (rest arglst)))]
    `(defmethod (:handler @~act) ~msg ~arglst2 ~@body)))
