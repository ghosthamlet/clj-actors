(ns act.networking
  (:import (java.nio.channels.spi SelectorProvider)
           (java.nio.channels ServerSocketChannel SelectionKey SocketChannel)
           (java.net InetSocketAddress InetAddress)
           (java.nio ByteBuffer)))

(defn address
    "Returns an InetAddress from [1 2 3 4] or \"1.2.3.4\""
    [adr]
    (let [v (if (string? adr)
              (map #(Integer/decode %) (.split adr "[.]"))
              adr)]
      (InetAddress/getByAddress (into-array Byte/TYPE (map byte adr)))))

(defn localhost
  "Returns the InetAddress of 127.0.0.1 rather than InetAddress/getLocalHost"
  []
  (address [127 0 0 1]))

(defn create-server
  "Binds a server socket and sets it up for nonblocking operation,
returning a map with :port , :address , and other infrastructural fields."
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

(defn close-server
  "Closes a server."
  [server]
  (.close (:chan server))
  (.close (:sel server)))

(defn- iterator-seq!
  "A variant of iterator-seq that leaves the iterator empty."
  [#^java.util.Iterator iter]
  (loop [acc ()]
    (if (.hasNext iter)
      (let [e (.next iter)]
        (.remove iter)
        (recur (conj acc e)))
      (reverse acc))))

(defn select
  "Waits for activity on a server."
  [server]
  (-> server :accept .selector .select))

(defn selected
  "Returns a list of SelectionKeys of server activity."
  [server]
  (-> server :sel .selectedKeys .iterator iterator-seq!))

(defn select-type
  "Maps SelectionKeys to one of (:accept :read :write)"
  [#^SelectionKey x]
  (cond (.isAcceptable x) :accept
        (.isReadable x) :read
        (.isWritable x) :write
        true (throw (Exception. (str "unknown accept-type: " x)))))

(defn push-readable
  "Adds a SelectionKey to a server, so that it will receive read events."
  [server #^SelectionKey x]
  (let [#^SocketChannel channel (.accept (.channel x))]
    (.configureBlocking channel false)
    (let [#^SelectionKey read-key
          (.register channel (:sel server) SelectionKey/OP_READ)]
      read-key)))

(defn pop-readable
  "Registers a SelectionKey for removal from a server."
  [server #^SelectionKey k]
  (.cancel k))

(defn socket-recv
  "Fill a ByteBuffer from a socket."
  [#^SelectionKey key #^ByteBuffer buf]
  (apply str (map char (take (.read (.channel key) buf) (.array buf)))))

(defn socket-send
  "Write a String to a socket."
  [#^SelectionKey key #^String s]
  (.write (.channel key) (ByteBuffer/wrap (.getBytes s))))
