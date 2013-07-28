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
