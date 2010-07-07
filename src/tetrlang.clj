(ns tetrlang
  (:use [clojure.contrib.duck-streams :only (read-lines)]
	[clojure.contrib.command-line :only (with-command-line)]
	[parser :only (parse whitespace-chars)]
	[interpreter :only (execute)])
  (:gen-class))

(def *program-width*)

(defn trim-lines [lines]
  (let [width *program-width*]
    (for [line lines]
      (let [length (count line)]
	(cond (> length width)
	      ,(apply str (take width line))
	      (< length width)
	      ,(apply str line (repeat (- width length)
				       (first whitespace-chars)))
	      :else line)))))

(defn read-file [filename]
  (vec (trim-lines (read-lines filename))))

(defn -main [& args]
  (with-command-line args
    "tetr -- tetrlang interpreter"
    [[file f "use file as input source code"]
     [parse-only? p? "only print result of parsing, without executing code"]
     [size s "specify interpreter memory size" 100]
     [width w "specify program width" 20]]
    (let [filename (or file *in*)
	  memory-size (if (integer? size) size (Integer/parseInt size))
 	  program-width (if (integer? width) width (Integer/parseInt width))
	  memory (vec (repeat 100 0))]
      (binding [*program-width* program-width]
	(let [insns (parse (read-file filename))]
	  (if parse-only?
	    (println insns)
	    (execute insns memory 0))
	  (flush))))))
