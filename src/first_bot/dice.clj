(ns first-bot.dice
  (:require [clojure.string :as str]))

(defn roll-generic-dice
  "Utility function to make generic dice rolls easier."
  [dice-amt]
  (inc (rand-int dice-amt)))

(defmulti roll-handler
  "The !r command has two different ways of being used, and as such has its own handler.
  !r dice N rolls a number between 1-N
  !r list COLL picks a random item out of a list depending on weighted odds"
  (fn [command] (nth (:parameters command) 0)))

(defmethod roll-handler "dice"
  [command]
  (let [input (nth (:parameters command) 1)
        output (roll-generic-dice (Integer/parseInt input))]
    (str "d" input " -> **" output "**")))

(defn string-to-odds-pair
  "Takes a string of name:num and returns a map of {:name name :num num}."
  [entry] ;; a:17
  (let [[name num] (str/split entry #":")]
    {:name name :num (Float/parseFloat num)}))

(defn accumulate-odds-pairs
  "Utility function for pick-random-entry"
  [odds-pair-seq]
  (reduce + (map :num odds-pair-seq)))

(defn pick-random-entry
  "Picks an option out of a list, depending on weighted odds. Could be simplified using reductions and drop-while, but I have no idea how to do so."
  [odds-pair-seq]
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
        output (-> input
                   (#(map string-to-odds-pair %))
                   pick-random-entry)
        output-string (str (:name output) ":" (:num output))
        output-name (:name output)]
    (str (str/join " " input) " -> " output-string " -> **" output-name "**")))

(defmulti command-handler
  "Selects which command to be used."
  (fn [command] (:command command)))

(defmethod command-handler "!r"
  [command]
  (roll-handler command))

(defmethod command-handler :default
  [command]
  (str "Unrecognized command: " (:command command) ". Details: " command))

(defn parameterize
  "This splits the inputted raw command into a 'command' and 'parameters' map, while dropping the @IronDice mention. This is done so command-handler can more easily select which multimethod should be used, while allowing the methods to handle the parameters however they are needed."
  [raw-command]
  {:command (get raw-command 1) :parameters (rest (rest raw-command))})

(defn string-to-command
  "Takes the command sent by the users (usually with an @ mention), modifies it, and send it to the command handler. Tries to return a string, but returns an exception if something goes wrong, informing the user."
  [string]
  (try
    (-> string
        str/trim
        (str/split #"\s+")
        parameterize
        command-handler)
    (catch Exception e (str "Exception: " (.getMessage e)))))
