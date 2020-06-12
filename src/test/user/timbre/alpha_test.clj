(ns user.timbre.alpha-test
  (:require
   [clojure.string :as str]
   [clojure.test :as test :refer [deftest is are testing]]
   [io.aviso.ansi :as ansi]
   [taoensso.timbre :as timbre]
   [user.timbre.alpha :refer :all]
   ))


(init! {})


(deftest main
  (timbre/info (ident ::a))
  (timbre/info (ansi/bold-green "asdf"))
  (timbre/info (fn []))
  (timbre/info (ident-kw-transform ::a))
  (binding [*ansi-enabled* true]
    (timbre/info (ident-kw-transform ::a)))


  (require 'user.timbre.extensions.default)
  (timbre/info "Default extension:" (fn []))

  (require 'user.timbre.extensions.integrant)
  (timbre/info "Integrant extension:" (ident [::a ::b])))


(deftest ns-level-filter
  (is
    (some
      (fn [[prefix :as entry]] (when (str/starts-with? "org.eclipse.jetty.util.thread.strategy.ExecuteProduce" prefix) entry))
      {"org.eclipse.jetty." :info}))


  (is (identical?
        (try (ns-level-filter-middleware {"org.eclipse.jetty." :??}) (catch java.lang.AssertionError _ java.lang.AssertionError))
        java.lang.AssertionError))
  (is (map? (time ((ns-level-filter-middleware {"org.eclipse.jetty." :info}) {:?ns-str "org.eclipse.jetty.util.thread.strategy.ExecuteProduce" :level :info}))))
  (is (nil? (time ((ns-level-filter-middleware {"org.eclipse.jetty." :info}) {:?ns-str "org.eclipse.jetty.util.thread.strategy.ExecuteProduce" :level :debug}))))
  (is (map? (time ((ns-level-filter-middleware {"org.eclipse.jetty." :info}) {:?ns-str "org.eclipse.jetty.util.thread.strategy.ExecuteProduce" :level :info}))))
  (is (nil? (time ((ns-level-filter-middleware {"org.eclipse.jetty." :info}) {:?ns-str "org.eclipse.jetty.util.thread.strategy.ExecuteProduce" :level :debug}))))
  (is (map? (time ((ns-level-filter-middleware {"org.eclipse.jetty." :info}) {:?ns-str "boot.user" :level :debug}))))
  )


(comment
  {:pattern  "HH:mm:ss.SSS"
   :locale   :jvm-default
   :timezone utc-9}

  {:pattern  "dd HH:mm:ss.SSS"
   :locale   :jvm-default
   :timezone utc-9}

  {:timestamp-opts
   {:pattern  "u HH:mm:ss.SSS"
    :locale   :jvm-default
    :timezone utc-9}
   :output-fn :inherit}

  (. date-format-yyyy-MM-dd format (java.util.Date.))

  (timbre/fatal :fatal)
  (timbre/error :error)
  (timbre/warn  :warn)
  (timbre/info  :info)
  (timbre/debug :debug)
  (timbre/trace :trace)

  (defn- atomic-java-console-println [x]
    (binding [*out* (.writer (System/console))]
      (print x \n)
      (flush)))

  (timbre/merge-config! {:appenders {:stdout {:enabled? false}}})

  )
