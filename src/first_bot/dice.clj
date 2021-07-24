(ns first-bot.dice
  (:require [clojure.string :as str]
            [instaparse.core :as insta :refer [defparser]])
  (:gen-class))

(defparser msg-command-parser (clojure.java.io/resource "msg-command-parser.bnf"))

(defn string-to-command
  "Takes the command sent by the users (usually with an @ mention), modifies it, and send it to the command handler. Tries to return a string, but returns an exception if something goes wrong, informing the user."
  [string]
  (try
    (msg-command-parser string)
    (catch Exception e (str "Exception: " (.getMessage e)))))
