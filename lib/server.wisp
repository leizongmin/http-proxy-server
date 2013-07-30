(ns proxy.server
  "Proxy Server"
  (:require [os :refer [network-interfaces]]
            [proxy.proxy :refer [create-server]]
            [proxy.utils :refer [debug log each-keys]]
            [commander :as program]))

(.version program "0.1")
(.option program "-p, --port [port]', 'Specify the port will listen on(8989 by default)" parse-int)
(.parse program (:argv process))

(def port (or (:port program) 8989))

(def server (create-server))
(.listen server port (fn []
  (log (str "Proxy server running at localhost:" port))
  (log "Network interfaces:")
  (each-keys (network-interfaces) (fn [k v]
    (log k)
    (.for-each v (fn [item]
      (log (str "  " (:address item) "\t" (:family item)))))))))
