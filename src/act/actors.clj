(ns act.actors)

(defn !
  "Send a message to an actor, with arguments."
  [act msg & args]
  (apply (:sender @act) act (:handler @act) msg args))

(defn arecur [st msg & args]
  "Within an actor, send a message to itself given its state."
  (apply (:sender st) (:self st) (:handler st) msg args)
  st)

(defn- msecond [_ x & _] x)
(defn actor
  "Creates an actor given an initial state, a map.
Returns an agent over this state and additional fields:

  :handler - refers to the MultiFn extended by defhandle
  :self - refers to the agent itself.
  :sender - sending fn; if not set in 'st, defaulted to send-off

Extend the actor with defhandle; send messages to it with
! ;use arecur within a handler to loop."
  [#^PersistentArrayMap st]
  (let [handler (new clojure.lang.MultiFn msecond :default)
        st (if (:sender st) st (conj st {:sender send-off}))
        ag (agent (conj st {:handler handler}))]
    (send ag conj {:self ag})
    ag))

(defmacro defhandle
  "Add a handler to an actor."
  [act msg arglst & body]
  (let [arglst2 (vec (concat [(first arglst) '_] (rest arglst)))]
    `(defmethod (:handler @~act) ~msg ~arglst2 ~@body)))

(defn watch
  "Wrapper for add-watcher, to send a message upon a change."
  [ref act msg]
  (add-watcher ref :send-off (:sender @act)
               (fn [_ refst]
                 (! act msg refst))))
