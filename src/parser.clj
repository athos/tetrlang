(ns parser
  (:use tmino)
  )

(def whitespace-char \u3000)

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

(defn skip-ignorable []
  (doM (many (<|> (satisfy #(= whitespace-char %))
		  (visited)))
       (return nil)))


(defn transform [rows]
  (apply concat
	 (for [i (range (count rows)),
	       :let [row (get rows i)],
	       j (range (count row))
	       :let [c (get row j)]]
	   (if (not= c \space)
	     [[[i j] c]]
	     nil))))

(defn normalize [[origin & _ :as coords]]
  (let [[[y0 x0] _] origin]
    (for [[[y x] c] coords]
      [[(- y y0) (- x x0)] c])))

(defn minos->parser [minos value]
  (letfn [(rec [[[[y x] c] & rest :as minos]]
	    (if (empty? minos)
	      (doM (advance)
		   (return value))
	      (doM (ensure-value y x c)
		   (rec rest))))]
    (doM (skip-ignorable)
	 (rec minos))))

(defn fold-with-<|> [[p & ps]]
  (if (empty? ps)
    p
    (<|> p (fold-with-<|> ps))))

(defn tmino->parser [tmino value]
  (fold-with-<|> (map #(minos->parser (normalize (transform %)) value)
		      tmino)))
;;  (map #(normalize (transform %)) tmino))

(defmacro def-tmino-parser [& names]
  `(do ~@(for [[name value] names]
	   (let [parser-name (symbol (str name '-parser))
		 tmino-name (symbol (str 'tmino- name))]
	     `(defn ~parser-name []
		(tmino->parser ~tmino-name '~value))))))

(def-tmino-parser
  (T +)
  (O -)
  (S >)
  (Z <)
  (IH I)
  (IV O)
  (L L)
  (J J))

(defn parser []
  (many (<|> (T-parser)
	     (O-parser)
	     (S-parser)
	     (Z-parser)
	     (IV-parser)
	     (IH-parser)
	     (doM (L-parser)
		  (x <- (parser))
		  (J-parser)
		  (return x)))))

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

(defn parse [input]
  (run-parse input (parser)))
