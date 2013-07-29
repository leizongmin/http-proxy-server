(ns proxy.proxy
  "Proxy"
  (:require [proxy.utils]
            [Debug]))

(def ^:private debug (Debug "proxy"))
(def ^:private log console.log)

(defn proxy
  "从http请求头部取得请求信息后，继续监听浏览器发送数据，同时连接目标服务器，并把目标服务器的数据传给浏览器"
  [^Object req ^Connection c ^Buffer b]
  (debug (str (.-method req) " " (.-host req) ":" (.-port req)))
  
  ;; 如果请求不是CONNECT方法（GET, POST），那么替换掉头部的一些东西
  (if (!= (.method req) "CONNECT")
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

  (if (!= (.method req) "CONNECT")
    (.write c "HTTP/1.1 200 Connection established\r\nConnection: close\r\n\r\n")
    (.write s b)))