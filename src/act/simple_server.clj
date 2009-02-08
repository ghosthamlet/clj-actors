(ns act.simple-server
  (:use act.actors)
  (:use act.networking)
  (:import (java.nio ByteBuffer)))

(defn- default-loop [act]
  (defhandle act :loop [st]
    (-> st :server select)
    (loop [events (-> st :server selected)]
      (if-let [event (first events)]
        (cond (.isAcceptable event)
              (do (arecur st :connected event)
                  (recur (rest events)))
              (.isReadable event)
              (let [buf (ByteBuffer/allocate 0xFFF)
                    instr (socket-recv event buf)]
                (if (zero? (count instr))
                  (arecur st :dropped event)
                  (arecur st :received event instr))
                (recur (rest events)))
              :else (throw (Exception. "neither acceptable nor readable")))
        (arecur st :loop)))))

(defn- default-connected [act]
  (defhandle act :connected [st #^SelectionKey sock]
    (push-readable (:server st) sock)
    st))
    
(defn- default-dropped [act]
  (defhandle act :dropped [st #^SelectionKey sock]
    (pop-readable (:server st) sock)
    st))

(defn create-simple-server
  "Given a server from act.networking/create-server,
create the server as :server in a returned actor.  Defines a
:loop handler that expects these handlers:

  :connected [act #^Selectionkey sock]
  :received [act #^SelectionKey sock #^String instr]
  :dropped [act #^SelectionKey sock]

By default, :connected calls act.networking/push-readable
and :dropped calls act.networking/pop-readable
"
  [server]
  (doto (actor {:server server})
    (default-loop)
    (default-connected)
    (default-dropped)))
