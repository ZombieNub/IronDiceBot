(ns first-bot.dice
  (:require [clojure.string :as str]))

(defn roll-generic-dice
  [dice-amt]
  (inc (rand-int dice-amt)))

(defmulti roll-handler
  (fn [command out-func] (str/starts-with? (nth (:parameters command) 0) "[")))

(defmethod roll-handler false
  [command out-func]
  (let [matcher (re-matcher #"^d*(\d+)" (nth (:parameters command) 0))]
    (re-find matcher)
    (out-func (str "**" (nth (:parameters command) 0) ":** " (roll-generic-dice (Integer/parseInt (get (re-groups matcher) 1)))))))

(defn array-parameter-divider
  [entry] ;; a:17
  (let [key-and-num (str/split entry #":")]
    {:name (nth key-and-num 0) :num (Integer/parseInt (nth key-and-num 1))}))

(defn array-parameter-to-seq
  [map-entry]
  (take (:num map-entry) (repeat (:name map-entry))))

(defmethod roll-handler true
  [command out-func]
  (-> command
      :parameters
      str/join
      (str/replace #"[\s\[\]]" "")
      (str/split #",")
      (#(map array-parameter-divider %))
      (#(map array-parameter-to-seq %))
      flatten
      rand-nth
      (#(str "**" (str/join (:parameters command)) ":** " %))
      out-func))

(defmulti command-handler
  (fn [command out-func] (:command command)))

(defmethod command-handler "!r"
  [command out-func]
  (roll-handler command out-func))

(defmethod command-handler :default
  [command out-func]
  (println (str "Unrecognized command: " (:command command) ". Details: " command)))

(defn parameterize
  [command]
  {:command (get command 1) :parameters (rest (rest command))})

(defn string-to-command
  [string out-func]
  (try
    (-> string
        str/trim
        (str/split #"\s+")
        parameterize
        (command-handler out-func))
    (catch Exception e (str "Exception: " (.getMessage e)))))
