(ns anny.forwardprop
	(:require [clojure.core.matrix :as m]
			  [anny.utility :as u]
			  [anny.network :as n])
	(:use [clojure.core.matrix.operators]))

(defn sum-weighted-values
	"computes the propagation for one layer given the
	 input and weights for that layer this is often used
	 as a part of forward propagiton or classifying"
	 [input weight-layer]
	 (m/inner-product (m/transpose input) weight-layer))

(defn prop-function
	"sums the weighted inputs for a node and applies
	 the activation function"
	 [activation-function]
	 (comp 
	 	(fn [x] (mapv activation-function x))
	 	sum-weighted-values))

(defn forward-prop
	"foward propagiotn to finde output of network"
	[net input]
	(let [weights (:weights net)
		  act (:activation-function net)
		  f (prop-function act)]
		  (reduce #(f %1 %2) input weights)))

