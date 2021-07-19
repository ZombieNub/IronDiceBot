(ns first-bot.dice-test
  (:require [first-bot.dice :as sut]
            [clojure.test :as t]
            [clojure.string :as str]))

(t/deftest r-dice-test
  (t/testing "The !r dice command"
    (t/is (str/starts-with? (sut/string-to-command "@example !r dice 20") "**d20 ->** "))
    (t/is (= (sut/string-to-command "@example !r dice 1") "**d1 ->** 1"))))
