(ns first-bot.core
  (:require [first-bot.dice :as dice]
            [clojure.edn :as edn]
            [clojure.core.async :refer [chan close!]]
            [clojure.string :as str]
            [discljord.messaging :as discord-rest]
            [discljord.connections :as discord-ws]
            [discljord.formatting :refer [mention-user]]
            [discljord.events :refer [message-pump!]])
  (:gen-class))

(declare start-bot! stop-bot!)

(def state (atom nil))

(def bot-id (atom nil))

(def config (edn/read-string (slurp (clojure.java.io/resource "config.edn"))))

(defmulti handle-event (fn [type _data] type))

(defn random-response [user]
  (str (rand-nth (:responses config)) ", " (mention-user user) \!))

(defmethod handle-event :message-create
  [_ {:keys [channel-id author mentions content] :as _data}]
  (when (some #{@bot-id} (map :id mentions))
    (if (= content "<@!865428758940745728> !kill")
      (discord-ws/disconnect-bot! (:gateway @state))
      (discord-rest/create-message! (:rest @state) channel-id :content (dice/string-to-command content)))))

;; (when (some #{@bot-id} (map :id mentions))
;;    (discord-rest/create-message! (:rest @state) channel-id :content (random-response author)))

(defmethod handle-event :ready
  [_ _]
  (discord-ws/status-update! (:gateway @state) :activity (discord-ws/create-activity :name (:playing config))))

(defmethod handle-event :default [_ _])

(defn start-bot! [token & intents]
  (let [event-channel (chan 100)
        gateway-connection (discord-ws/connect-bot! token event-channel :intents (set intents))
        rest-connection (discord-rest/start-connection! token)]
    {:events  event-channel
     :gateway gateway-connection
     :rest    rest-connection}))

(defn stop-bot! [{:keys [rest gateway events] :as _state}]
  (discord-rest/stop-connection! rest)
  (close! events))

(defn -main [& args]
   (reset! state (start-bot! (:token config) :guild-messages))
   (reset! bot-id (:id @(discord-rest/get-current-user! (:rest @state))))
   (try
     (message-pump! (:events @state) handle-event)
     (finally (stop-bot! @state))))
