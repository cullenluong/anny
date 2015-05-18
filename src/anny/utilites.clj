(ns anny.utility
	(:require [clojure.core.matrix as m])
	(:use [clojure.core.matrix.operators]))

(defn sigmoid 
	"sigmoid activation function"
	[x]
	(/ 1 (+ 1 (exp (- x)))))