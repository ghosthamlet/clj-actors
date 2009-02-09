(ns examples.watcher
  (:use act.actors))

;; (def repeater (actor {}))
;; (defhandle repeater :broadcast [st msg]
;;   (println "repeater" msg)
;;   st)

(def reverser (actor {}))
(defhandle reverser :broadcast [st msg]
  (println "reverser:" (apply str (reverse msg)))
  st)

(def broadcast-message (agent ""))
(defn broadcast (msg)
  (send broadcast-message #(do %2) msg)
  msg)

;(watch broadcast-message repeater :broadcast)
;(watch broadcast-message reverser :broadcast)

;;(println "(broadcast MSG)")
;;(clojure.main/repl)
