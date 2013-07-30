(ns proxy.utils
  "Proxy Utils")

(def ^:private **REQUEST-MATCH-1** #"^([A-Z]+)\s([^\:\s]+)\:(\d+)\sHTTP\/(\d\.\d)")
(def ^:private **REQUEST-MATCH-2** #"^([A-Z]+)\s([^\s]+)\sHTTP\/(\d\.\d)")
(def ^:private **REQUEST-MATCH-3** #"Host\:\s+([^\n\s\r]+)")
(def ^:private **REQUEST-MATCH-4** #"^([A-Z]+)\s")
(def ^:private **REQUEST-MATCH-5** #"http\:\/\/[^\/]+")

(defn- ^Boolean body-end?
  "判断是否为\r\n\r\n"
  [^Buffer b ^Number i]
  (and
    (== (get b i) 13)
    (== (get b (+ i 1)) 10)
    (== (get b (+ i 2)) 13)
    (== (get b (+ i 3)) 10)))

(defn ^Number find-body
  "从缓存中找到头部结束标记\r\n\r\n的位置
  未找到时返回-1"
  [^Buffer b]
  (def len (.-length b))
  (def i (loop [i 0]
    (if (< i len)
      (if (body-end? b i)
        i
        (recur (+ i 1))))))
  (if (> i 0) i -1))

(defn- ^Object parse-connect-request
  [^String s]
  (def arr (.match s **REQUEST-MATCH-1**))
  (if (>= 5 (.-length arr))
    {:method (get arr 1)
     :host (get arr 2)
     :port (get arr 3)
     :httpVersion (get arr 4)}
    false))

(defn- ^Object parse-other-request
  [^String s]
  (def arr (.match s **REQUEST-MATCH-2**))
  (if (>= (.-length arr) 4)
    (let [host (get (.match s **REQUEST-MATCH-3**) 1)]
      (if host
        (let [p (.split host ":")]
          {:method (get arr 1)
           :host (get p 0)
           :port (or (get p 1) 80)
           :path (get arr 2)
           :httpVersion (get arr 3)})
        false))
    false))

(defn ^Object parse-request
  "从请求头部取得请求详细信息
  如果是 CONNECT 方法，那么会返回 {method,host,port,httpVersion}
  如果是 GET/POST 方法，那么返回 {metod,host,port,path,httpVersion}"
  [^Buffer b]
  (def s (.toString b "utf8"))
  (def m (get (.match (get (.split s "\n") 0) **REQUEST-MATCH-4**) 1))
  (if (== m "CONNECT")
    (parse-connect-request s)
    (parse-other-request s)))

(defn- ^String modify-headers-connection
  "替换connection头"
  [^String h]
  (set! h (.replace h (RegExp "(proxy\\-)?connection\\:.+\\r\\n" "ig") ""))
  (set! h (.replace h (RegExp "Keep\\-Alive\\:.+\\r\\n" "i") ""))
  (.replace h "\r\n" "\r\nConnection: close\r\n"))

(defn- ^String modify-headers-path
  "替换网址格式(去掉域名部分)"
  [^Object req ^String header]
  (def url (.replace (:path req) **REQUEST-MATCH-5** ""))
  (if (!= (:path req) url)
    (.replace header (:path req) url)
    header))

(defn ^String modify-headers
  "如果请求不是CONNECT方法（GET, POST），那么替换掉头部的一些东西"
  [^Object req ^Buffer b]
  (def i (find-body b))
  (if (< i 0) (set! i (:length b)))
  (def header (.to-string (.slice b 0 i)))
  (set! header (modify-headers-connection header))
  (if (== (:httpVersion req) "1.1")
    (set! header (modify-headers-path req header)))
  (.concat Buffer [(Buffer. header "utf8") (.slice b i)]))
