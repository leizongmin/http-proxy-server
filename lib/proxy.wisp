(ns proxy.proxy
  "Proxy"
  (:require [net]
            [proxy.utils :refer [debug log modify-headers find-body parse-request]]))

(defn proxy
  "从http请求头部取得请求信息后，继续监听浏览器发送数据，同时连接目标服务器，并把目标服务器的数据传给浏览器"
  [^Object req ^Connection c ^Buffer b]
  (debug (str (:method req) " " (:host req) ":" (:port req)))
  
  ;; 如果请求不是CONNECT方法（GET, POST），那么替换掉头部的一些东西
  (if (== (:method req) "CONNECT") nil
    (set! b (modify-headers req b)))

  ;; 建立到目标服务器的连接
  (def s (.create-connection net (:port req) (:host req)))
  ;; 交换服务器与浏览器的数据
  (.pipe c s)
  (.pipe s c)
  ;; 异常
  (.on c "error" (fn [err]
    (log err)))
  (.on s "error" (fn [err]
    (log err)))

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
            (.remove-all-listeners c "data")
            (proxy req c b)))))))))