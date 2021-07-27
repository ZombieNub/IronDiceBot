(ns first-bot.dice
  (:require [first-bot.interpreter :as interpreter]
            [clojure.string :as str])
  (:gen-class))

(defn roll-generic-dice
  [num]
  (inc (rand-int num)))

(defmulti parse-roll
  (fn [demand pargs kwargs] (-> pargs
                        first
                        keyword)))

(defmethod parse-roll :dice
  [demand pargs kwargs]
  (let [name (if (:name kwargs)
              (:name kwargs)
              "Roll")]
    (str name ": **" (roll-generic-dice (second pargs)) "**")))

(defn roll-list
  [odds-pairs]
  (let [adjusted-odds (reductions + (map second odds-pairs))
        adjusted-pairs (map #(list (first %1) %2) odds-pairs adjusted-odds)
        maximum (last adjusted-odds)
        roll (rand maximum)]
    (first (drop-while #(< (second %) roll) adjusted-pairs))))

(defmethod parse-roll :list
  [demand pargs kwargs]
  (let [selection (roll-list (next pargs))
        name (if (:name kwargs)
               (:name kwargs)
               "List")]
    (str name ": **" (print-str (first selection)) "**")))

(defmethod parse-roll :default
  [demand pargs kwargs]
  (print-str "Unrecognized command. Details: demand:" demand "pargs:" pargs "kwargs:" kwargs))

(defmulti parse-command
  (fn [demand pargs kwargs] demand))

(defmethod parse-command :r
  [demand pargs kwargs]
  (parse-roll demand pargs kwargs))

(defmethod parse-command :default
  [demand pargs kwargs]
  (print-str "Unrecognized command. Details: demand:" demand "pargs:" pargs "kwargs:" kwargs))

(defn grab-command-features
  [command]
  (let [demand (-> command
                   :command
                   :demand)
        pargs (-> command
                  :command
                  :pargs)
        kwargs (-> command
                   :command
                   :kwargs)]
    (list demand pargs kwargs)))

(defn process-command
  [string]
  (try
    (-> string
        interpreter/interpret
        grab-command-features
        (#(apply parse-command %)))
    (catch Exception e (str "Exception: " (.getMessage e)))))
