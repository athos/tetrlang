(ns parser
  (:refer-clojure :exclude [char])
  )

(declare >>=)
  
(defn >> [m k]
  (>>= m (fn [_] k)))

(defmacro doM [expr & exprs]
  (cond (empty? exprs) expr
	(not (list? expr)) `(>> ~expr (doM ~@exprs))
	:else
	(let [[a1 a2 a3 & rest] expr]
	  (cond (= a1 'let) `(let [~a2 ~a3] (doM ~@exprs))
		(= a2 '<-) `(>>= ~a3 (fn [~a1] (doM ~@exprs)))
		:else `(>> ~expr (doM ~@exprs))))))

(defn return [a]
  (fn [s] [a s]))

(defn >>= [m f]
  (fn [[index _ :as s0]]
    (let [ret (m s0)]
      (if (nil? ret)
	nil
	(let [[a s1] ret]
	  ((f a) s1))))))


(def *input*)

(defn input-nrows []
  (count *input*))

(defn input-ncolumns []
  (count (get *input* 0)))

(defn valid-pos? [[y x]]
  (and (< -1 y (input-nrows))
       (< -1 x (input-ncolumns))))

(defn at [[y0 x0] y x]
  (let [x* (+ x0 x)
	q (quot x* (input-ncolumns))]
    [(+ y0 y (if (>= x* 0) q (- q 1)))
     (mod x* (input-ncolumns))]))

(defn get-pos [[y x]]
  (get (get *input* y) x))

(defn value
  ([] (value 0))
  ([x] (value 0 x))
  ([y x]
   (fn [[pos _ :as s]]
     (let [pos* (at pos y x)]
       (if (valid-pos? pos*)
	 [(get-pos pos*) s]
	 nil)))))

(defn visit
  ([] (visit 0))
  ([x] (visit 0 x))
  ([y x]
   (fn [[pos visited]]
     [nil [pos (conj visited (at pos y x))]])))

(defn advance
  ([] (advance 1))
  ([x] (advance 0 x))
  ([y x]
   (fn [[pos visited]]
     [nil [(at pos y x) visited]])))

(defn ensure-visited
  ([] (ensure-visited 0))
  ([x] (ensure-visited 0 0))
  ([y x]
   (fn [[pos visited :as s]]
     (let [pos* (at pos y x)]
       (if (visited pos*)
	 [nil s]
	 nil)))))

(defn ensure-not-visited
  ([] (ensure-not-visited 0))
  ([x] (ensure-not-visited 0 0))
  ([y x]
   (fn [[pos visited :as s]]
     (let [pos* (at pos y x)]
       (if (visited pos*)
	 nil
	 [nil s])))))

(defn get-value
  ([] (get-value 0))
  ([x] (get-value 0 x))
  ([y x]
   (doM (ensure-not-visited y x)
	(v <- (value y x))
	(visit y x)
	(return v))))

(defn visited []
  (doM (ensure-visited)
       (v <- (value))
       (advance)
       (return v)))

(defn fail []
  (fn [s] nil))

(defmacro <|>
  ([p] p)
  ([p & ps]
   `(fn [s#]
     (let [ret# (~p s#)]
       (if (nil? ret#)
	 ((<|> ~@ps) s#)
	 ret#)))))

(declare many1)

(defn many [p]
  (<|> (many1 p)
       (return nil)))

(defn many1 [p]
  (doM (x <- p)
       (xs <- (many p))
       (return (cons x xs))))

(defn ensure-value
  ([value] (ensure-value 0 value))
  ([x value] (ensure-value 0 x value))
  ([y x value]
   (doM (c <- (get-value y x))
	(if (= c value)
	  (return c)
	  (fail)))))

(defn any-char []
  (doM (v <- (get-value))
       (advance)
       (return v)))

(defn satisfy [pred]
  (doM (v <- (any-char))
       (if (pred v)
	 (return v)
	 (fail))))

(defn run-parse [input parser]
  (binding [*input* input]
    (let [[ret _] (parser [[0 0] #{}])]
      ret)))

;; for debug
(defn ptest
  ([code parser] (ptest code (count code) parser))
  ([code n parser]
   (binding [*input* (vec (map vec (partition n code)))]
     (parser [[0 0] #{}]))))

