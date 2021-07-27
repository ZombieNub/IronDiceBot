(defproject first-bot "0.1.1-SNAPSHOT"
  :description "A discord dice bot for me an my friends."
  :url "https://github.com/ZombieNub/IronDiceBot"
  :license {:name "EPL-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.suskalo/discljord "1.1.1"]
                 [instaparse "1.4.10"]]
  :repl-options {:init-ns first-bot.core}
  :main first-bot.core
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
