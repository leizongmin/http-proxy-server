(ns proxy.proxy
  "Proxy"
  (:require [proxy.utils]
            [Debug]))

(def debug (Debug "proxy"))

(defn proxy
  "从http请求头部取得请求信息后，继续监听浏览器发送数据，同时连接目标服务器，并把目标服务器的数据传给浏览器"
  [^Object req ^Connection c ^Buffer b]
  (debug (str (.-method req) " " (.-host req) ":" (.-port req)))
  
  (if (!= (.method req) "CONNECT")
    ()