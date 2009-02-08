(ns act.networking
  (:import (java.nio.channels.spi SelectorProvider)
           (java.nio.channels ServerSocketChannel SelectionKey SocketChannel)
           (java.net InetSocketAddress InetAddress)
           (java.nio ByteBuffer)))

(defn localhost []
  (InetAddress/getByAddress (into-array Byte/TYPE (map byte [127 0 0 1]))))

(defn create-server
  ([port] (create-server port (localhost)))
  ([port address]
    (let [sel (.openSelector (SelectorProvider/provider))
          chan (ServerSocketChannel/open)
          addr (InetSocketAddress. address port)]
      (.configureBlocking chan false)
      (.bind (.socket chan) addr)
      (let [accept-key (.register chan sel SelectionKey/OP_ACCEPT)]
        {:sel sel, :chan chan, :addr addr, :accept accept-key,
         :port port, :address address}))))

(defn close-server [server]
  (.close (:chan server))
  (.close (:sel server)))

(defn iterator-seq! [#^java.util.Iterator iter]
  (loop [acc ()]
    (if (.hasNext iter)
      (let [e (.next iter)]
        (.remove iter)
        (recur (conj acc e)))
      (reverse acc))))

(defn select [server] (-> server :accept .selector .select))
(defn selected [server] (-> server :sel .selectedKeys .iterator iterator-seq!))

(defn select-type [#^SelectionKey x]
  (cond (.isAcceptable x) :accept
        (.isReadable x) :read
        (.isWritable x) :write
        true (throw (Exception. (str "unknown accept-type: " x)))))

(defn push-readable [server #^SelectionKey x]
  (let [#^SocketChannel channel (.accept (.channel x))]
    (.configureBlocking channel false)
    (let [#^SelectionKey read-key
          (.register channel (:sel server) SelectionKey/OP_READ)]
      read-key)))

(defn pop-readable [server #^SelectionKey k]
  (.cancel k))

(defn socket-recv [#^SelectionKey key #^ByteBuffer buf]
  (apply str (map char (take (.read (.channel key) buf) (.array buf)))))
(defn socket-send [#^SelectionKey key #^String s]
  (.write (.channel key) (ByteBuffer/wrap (.getBytes s))))