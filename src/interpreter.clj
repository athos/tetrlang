(ns interpreter)

(def *insn-table* (atom {}))

(defn execute-insn [insn memory pointer k]
  (let [[memory* pointer*] ((@*insn-table* insn) memory pointer)]
    #(k memory* pointer*)))

(declare execute-toplevel execute-insns execute-loop)

(defn execute [code memory pointer]
  (trampoline (execute-toplevel code memory pointer
				;; (fn [m p] [m p])
				(fn [_ _] nil)
				)))

(defn execute-toplevel [[insn & next :as code] memory pointer k]
  (if (empty? code)
    #(k memory pointer)
    #(execute-insns insn memory pointer
		    (fn [m p] (execute-toplevel next m p k)))))

(defn execute-insns [insn memory pointer k]
  (if (coll? insn)
    #(execute-loop insn memory pointer k)
    #(execute-insn insn memory pointer k)))

(defn execute-loop [code0 memory pointer k]
  (letfn [(rec [[insn & next :as code] memory pointer]
	    (if (empty? code)
	      #(execute-loop code0 memory pointer k)
	      #(execute-insns insn memory pointer
			      (fn [m p] (rec next m p)))))]
    (if (= (memory pointer) 0)
      #(k memory pointer)
      #(rec code0 memory pointer))))

(defmacro def-insn [insn & body]
  `(letfn [(proc# [~'memory ~'pointer]
	     ;; (printf "%s: %s(%d)\n" '~insn ~'memory ~'pointer)
	     ~@body)]
     (swap! *insn-table*
	    conj
	    ['~insn proc#])))

(def-insn +
  [(assoc memory pointer (inc (memory pointer)))
   pointer])

(def-insn -
  [(assoc memory pointer (dec (memory pointer)))
   pointer])

(def-insn >
  [memory (inc pointer)])

(def-insn <
  [memory (dec pointer)])

(def-insn O
  (print (char (memory pointer)))
  (flush)
  [memory pointer])

(def-insn I
  (let [c (.read *in*)]
    [(assoc memory pointer c)
     pointer]))
