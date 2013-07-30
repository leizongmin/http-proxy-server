(ns proxy.proxy
  "Proxy"
  (:require [net]
            [proxy.utils :refer [debug log dump dump-error modify-headers find-body parse-request]]))

(def **PROXY-ID** 0)

(defn- assign-proxy-id
  "分配一个代理ID"
  []
  (set! **PROXY-ID** (+ **PROXY-ID** 1))
  **PROXY-ID**)

(defn proxy
  "从http请求头部取得请求信息后，继续监听浏览器发送数据，同时连接目标服务器，并把目标服务器的数据传给浏览器"
  [^Object req ^Connection c ^Buffer b]
  
  ;; 如果请求不是CONNECT方法（GET, POST），那么替换掉头部的一些东西
  (if (== (:method req) "CONNECT") nil
    (let [info (modify-headers req b)]
      (set! req (:request info))
      (set! b (:buffer info))))

  (def id (assign-proxy-id))
  (def timestamp (.now Date))
  (def log2 (fn [s]
    (log (str "#" id ": +" (- (.now Date) timestamp) "ms " s))))
  (log2 (str (:method req) " http://" (:host req) ":" (:port req) (:path req)))

  ;; 建立到目标服务器的连接
  (def s (.create-connection net (:port req) (:host req)))
  ;; 交换服务器与浏览器的数据
  (.pipe c s)
  (.pipe s c)
  ;; 异常
  (.on c "error" (fn [err]
    (log2 (str err))))
  (.on s "error" (fn [err]
    (log2 (str err))))
  (.on s "data" (fn [b]
    (log2 (str "recieved " (:length b) "bytes"))))
  (.on s "close" (fn [b]
    (log2 "remote closed")))
  (.on c "close" (fn [b]
    (log2 "client closed")))

  (if (== (:method req) "CONNECT")
    (.write c "HTTP/1.1 200 Connection established\r\nConnection: close\r\n\r\n")
    (.write s b))
  nil)

(defn create-server
  "在本地创建一个server监听本地端口"
  []
  (.create-server net (fn [c]
    ;; 首先监听浏览器的数据发送事件，直到收到的数据包含完整的http请求头
    (def b (Buffer. 0))
    (.on c "data" (fn [data]
      (set! b (.concat Buffer [b data]))
      (if (== (find-body b) -1) nil
        (let [req (parse-request b)]
          (if req
            (do
              (.remove-all-listeners c "data")
              (proxy req c b))))))))))