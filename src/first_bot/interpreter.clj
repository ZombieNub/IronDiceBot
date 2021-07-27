(ns first-bot.interpreter
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [instaparse.core :as insta :refer [defparser]])
  (:gen-class))

(defparser msg-command-parser (clojure.java.io/resource "msg-command-parser.bnf"))

(declare convert-type reduce-type)

(defmulti convert-type
  (fn [type] (first type)))

(defmethod convert-type :command
  [type]
  {:command (reduce-type (rest type))})

(defmethod convert-type :args
  [type]
  (let [temp-unconverted-args (rest type)
        is-kwarg? (fn [type] (= (first type) :keyword))
        pargs (map convert-type (filter (complement is-kwarg?) temp-unconverted-args))
        kwargs (reduce-type (filter is-kwarg? temp-unconverted-args))]
    {:pargs pargs :kwargs kwargs}))

(defmethod convert-type :demand
  [type]
  {:demand (keyword (convert-type (second type)))})

(defmethod convert-type :statement-pair
  [type]
  (list (convert-type (nth type 1))
        (convert-type (nth type 3))))

(defmethod convert-type :float
  [type]
  (Float/parseFloat (apply str (rest type))))

(defmethod convert-type :int
  [type]
  (let [val (second type)]
    (if (< -2147483648 (bigint val) 2147483647)
      (Integer/parseInt val)
      (bigint val))))

(defmethod convert-type :keyword
  [type]
  (let [keyword (keyword (convert-type (second type)))
        statement (convert-type (nth type 3))]
    {keyword statement}))

(defmethod convert-type :string
  [type]
  (apply str (map convert-type (rest type))))

(defmethod convert-type :word
  [type]
  (second type))

(defmethod convert-type :whitespace
  [type]
  (second type))

(defmethod convert-type :default
  [type]
  {(first type) (rest type)})

(defn reduce-type
  [type]
  (reduce (fn [kmap type]
            (let [converted-type (convert-type type)]
              (merge kmap converted-type)))
          {}
          type))

(defn interpret
  [string]
  (let [coll (msg-command-parser string)]
    (convert-type coll)))
