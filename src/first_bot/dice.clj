(ns first-bot.dice
  (:require [clojure.string :as str]))

(defn roll-generic-dice
  [dice-amt]
  (inc (rand-int dice-amt)))

(defmulti roll-handler
  (fn [command] (nth (:parameters command) 0)))

(defmethod roll-handler "dice"
  [command]
  (let [input (nth (:parameters command) 1)
        output (roll-generic-dice (Integer/parseInt input))]
    (str "**d" input " ->** " output)))

(defn array-parameter-divider
  [entry] ;; a:17
  (let [key-and-num (str/split entry #":")]
    {:name (nth key-and-num 0) :num (Integer/parseInt (nth key-and-num 1))}))

(defn array-parameter-to-seq
  [map-entry]
  (take (:num map-entry) (repeat (:name map-entry))))

(defmethod roll-handler "list"
  [command]
  (-> command
      :parameters
      str/join
      (str/replace #"[\s\[\]]" "")
      (str/split #",")
      (#(map array-parameter-divider %))
      (#(map array-parameter-to-seq %))
      flatten
      rand-nth
      (#(str "**" (str/join (:parameters command)) ":** " %))))

(defmulti command-handler
  (fn [command] (:command command)))

(defmethod command-handler "!r"
  [command]
  (roll-handler command))

(defmethod command-handler :default
  [command]
  (println (str "Unrecognized command: " (:command command) ". Details: " command)))

(defn parameterize
  [command]
  {:command (get command 1) :parameters (rest (rest command))})

(defn string-to-command
  [string]
  (try
    (-> string
        str/trim
        (str/split #"\s+")
        parameterize
        command-handler)
    (catch Exception e (str "Exception: " (.getMessage e)))))
