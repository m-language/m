;;; http.m
;;;
;;; An HTTP client for M (temporary)

;; Send a request synchronously, returning a process
(extern http/send)

;; Supported HTTP methods
(def http/GET    (symbol GET))
(def http/HEAD   (symbol HEAD))
(def http/POST   (symbol POST))
(def http/PUT    (symbol PUT))
(def http/PATCH  (symbol PATCH))
(def http/DELETE (symbol DELETE))

;; Convenience methods for each HTTP method
(defn http/get url headers body    (http/send http/GET url headers body))
(defn http/head url headers body   (http/send http/HEAD url headers body))
(defn http/post url headers body   (http/send http/POST url headers body))
(defn http/put url headers body    (http/send http/PUT url headers body))
(defn http/patch url headers body  (http/send http/PATCH url headers body))
(defn http/delete url headers body (http/send http/DELETE url headers body))