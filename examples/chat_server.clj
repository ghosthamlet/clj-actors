(ns examples.chat-server
  (:use act.actors)
  (:use act.networking)
  (:use act.simple-server))

(def clients (agent {}))
(def broadcast-message (agent ""))
(defn broadcast [s]
  (send broadcast-message #(do %1 %2) s))
(defn relayer [sock rf]
  (socket-send sock @rf)
  sock)

(let [n (ref 1)]
  (defn client [key]
    (let [nam (str "Anonymous-" (dosync (alter n inc)))
          watcher (agent key)
          act (actor {:name nam :sock key, :watcher watcher})]
      (add-watcher broadcast-message :send watcher relayer)
      (defhandle act :say [st msg]
        (broadcast (str "<" (:name st) "> " msg "\r\n"))
        st)
      (defhandle act :nick [st nam]
        (broadcast (str "*** " (:name st) " changes nick to " nam "\r\n"))
        (assoc st :name nam))
      (defhandle act :me [st msg]
        (broadcast (str "* " (:name st) " " msg "\r\n"))
        st)
      (defhandle act :quit [st msg]
        (broadcast (str "*** " (:name st) " quits (" msg ")\r\n"))
        (remove-watcher broadcast-message (:watcher st))
        (.close (:sock st))
        st)
      (defhandle act :invalid [st msg]
        (socket-send (:sock st) (str "Invalid command: " msg "\r\n"))
        st)
      act)))

(defn clean [s]
  (apply str (filter #(>= (int %) (int \space)) s)))
  
(defn parse [s]
  (if (= (first s) \/)
    (let [cmd-msg (seq (.split (clean s) " " 2))]
      [({"/say" :say 
         "/nick" :nick 
         "/me" :me 
         "/quit" :quit} (first cmd-msg) :invalid)
       (or (second cmd-msg) s)])
    [:say (clean s)]))

(def port (if-let [arg1 (second *command-line-args*)]
            (Integer/decode arg1)
            4001))

(def chat-server (create-simple-server (create-server port)))

(defhandle chat-server :received [st key instr]
  (if-let [cl (@clients key)]
    (let [[cmd msg] (parse instr)]
      (! cl cmd msg)
      st)
    (let [cl (client key) ;; new client
          [cmd msg] (parse instr)]
      (send clients assoc key cl)
      (! cl cmd msg)
      (broadcast (str "*** " (:name @cl) " has joined.\r\n"))
      st)))

(defhandle chat-server :dropped [st key]
  (if-let [cl (@clients key)]
    (do (when (:watcher @cl)
          (remove-watcher broadcast-message (:watcher @cl)))
        (send clients dissoc key)
        (broadcast (str "*** " (:name @cl) " dropped connection.\r\n"))
        st)
    st)) ;; ignore uncliented drops

(! chat-server :loop)

(println "Echo server started on port" port)
(clojure.main/repl)
