(ns proxy.utils
  "Proxy Utils")

(defn body-end?
  "判断是否为\r\n\r\n"
  [^Buffer b ^Number i]
  (and
    (== (get b i) 13)
    (== (get b (+ i 1)) 10)
    (== (get b (+ i 2)) 13)
    (== (get b (+ i 3)) 10)))

(defn find-body
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

(defn- parse-connect-request
  [^String s]
  (def arr (.match s #"^([A-Z]+)\s([^\:\s]+)\:(\d+)\sHTTP\/(\d\.\d)"))
  (if (>= 5 (.-length arr))
    {:method (get arr 1)
     :host (get arr 2)
     :port (get arr 3)
     :httpVersion (get arr 4)}
    false))

(defn- parse-other-request
  [^String s]
  (def arr (.match s #"^([A-Z]+)\s([^\s]+)\sHTTP\/(\d\.\d)"))
  (if (>= (.-length arr) 4)
    (let [host (get (.match s #"Host\:\s+([^\n\s\r]+)") 1)]
      (if host
        (let [p (.split host ":")]
          {:method (get arr 1)
           :host (get p 0)
           :port (or (get p 1) 80)
           :path (get arr 2)
           :httpVersion (get arr 3)})
        false))
    false))

(defn parse-request
  "从请求头部取得请求详细信息
  如果是 CONNECT 方法，那么会返回 {method,host,port,httpVersion}
  如果是 GET/POST 方法，那么返回 {metod,host,port,path,httpVersion}"
  [^Buffer b]
  (def s (.toString b "utf8"))
  (def m (get (.match (get (.split s "\n") 0) #"^([A-Z]+)\s") 1))
  (if (== m "CONNECT")
    (parse-connect-request s)
    (parse-other-request s)))
