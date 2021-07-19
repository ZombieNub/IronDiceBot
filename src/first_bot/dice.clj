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
    (str "d" input " -> **" output "**")))

(defn string-to-odds-pair
  [entry] ;; a:17
  (let [key-and-num (str/split entry #":")]
    {:name (nth key-and-num 0) :num (Float/parseFloat (nth key-and-num 1))}))

(defn accumulate-odds-pairs
  [odds-pair-seq]
  (reduce + (map :num odds-pair-seq)))

(defn pick-random-entry
  [odds-pair-seq]
  (println odds-pair-seq)
  (let [accu (accumulate-odds-pairs odds-pair-seq)
        rand-val (rand accu)]
    (loop [iter 0
           accu-odds 0]
      (if (< rand-val (+ (:num (nth odds-pair-seq iter)) accu-odds))
        (nth odds-pair-seq iter)
        (recur (inc iter) (+ accu-odds (:num (nth odds-pair-seq iter))))))))

(defmethod roll-handler "list"
  [command]
  (let [input (rest (:parameters command))
        _ (println input)
        output (-> input
                   (#(map string-to-odds-pair %))
                   pick-random-entry)
        output-string (str (:name output) ":" (:num output))
        output-name (:name output)]
    (str (str/join " " input) " -> " output-string " -> **" output-name "**")))

(defmulti command-handler
  (fn [command] (:command command)))

(defmethod command-handler "!r"
  [command]
  (roll-handler command))

(defmethod command-handler :default
  [command]
  (str "Unrecognized command: " (:command command) ". Details: " command))

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
