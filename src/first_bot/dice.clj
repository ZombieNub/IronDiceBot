(ns first-bot.dice
  (:require [first-bot.interpreter :as interpreter]
            [clojure.string :as str])
  (:gen-class))

(defn process-command
  "Takes the command sent by the users (usually with an @ mention), modifies it, and send it to the command handler. Tries to return a string, but returns an exception if something goes wrong, informing the user."
  [string]
  (try
    (-> string
        interpreter/interpret
        prn-str)
    (catch Exception e (str "Exception: " (.getMessage e)))))
