(ns examples.echo-server
  (:use act.actors)
  (:use act.networking)
  (:use act.simple-server))

(def port (if-let [arg1 (second *command-line-args*)]
            (Integer/decode arg1)
            4000))

(def echo-server (create-simple-server (create-server port)))
(send echo-server assoc :sender send) ;; default is send-off

(defhandle echo-server :received [st sock instr]
  (socket-send sock instr)
  st)

(! echo-server :loop)

(println "Echo server started on port" port)
(clojure.main/repl)
