(ns anny.network
	(:require [clojure.core.matrix :as m]
			  [anny.utility :as u])
	(:use [clojure.core.matrix.operators]))

(m/set-current-implementation :vectorz)

(defrecord Network [weights activation-function bias?])


(defmulti network 
	"constructer function for the Network type will take a generator parameter
	TODO: add custom connections so this type can consturct several types of networks
		  more declaritive type checking"
	(fn [& {:keys [init]}]
			(cond 
				(keyword? init) init
				(sequential? init) :pre-made
				:else :default)))

(defn- generate-layer
	"helper function for generate weights"
	[f from to]
		(vec (repeatedly from  #(vec (repeatedly to f)))))


(defn generate-weights
	"generates weights given initialziation function
	 TODO: be able to take in more elaborate init funcitons"
	([f [from & xs]]
	(let [to (first xs)
		  layer (generate-layer f from to)]
		    (generate-weights f xs [layer])))

	([f [from & xs] weights]
		(if (nil? xs)
			weights
			(let [to (first xs)
		          layer (generate-layer f from to)]
		          (recur f xs (conj weights layer))))))

;multimethod constructor functions for creating network
(defmethod network :rand
	[& {:keys [init size activation-function bias?]
					 :or {size []
					 	  activation-function u/sigmoid
					 	  bias? true}}]
	(let [f #(- (rand 2) 1)
		  weights (generate-weights f size)]
	(Network. weights activation-function bias?)))


(defmethod network :pre-made
	[& {:keys [init activation-function bias?]
					 :or {activation-function u/sigmoid
					 	  bias? true}}]
	(Network. init activation-function bias?))


(defmethod network :default
	[& {:keys [init]}]
	(throw (IllegalArgumentException. "invalid initialization parameter")))