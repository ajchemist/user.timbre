(ns user.timbre.alpha
  (:require
   [clojure.string :as str]
   [clojure.java.io :as jio]
   [taoensso.encore :as encore]
   [taoensso.timbre :as timbre]
   [io.aviso.ansi :as ansi]
   [user.clojure.core.patch.alpha :refer [fast-memoize]]
   [user.string.namespace]
   [user.timbre.alpha.keyword :as kw]
   [user.timbre.patch.alpha :as timbre.patch]
   )
  (:import
   java.util.TimeZone
   ))


(set! *warn-on-reflection* true)


(def ^TimeZone utc (TimeZone/getTimeZone "UTC"))
(def ^TimeZone utc-9 (TimeZone/getTimeZone "Asia/Seoul"))


(def ^{:private true
       :tag     java.text.SimpleDateFormat}
  date-format-yyyy-MM-dd
  (taoensso.encore/simple-date-format* "yyyy-MM-dd" :jvm-default utc-9))


;; * Repl


(comment
  (for [sym (into []
              (comp
               (filter #(= (the-ns 'user.default.shim.taoensso.timbre) (:ns (meta (val %)))) )
               (map key))
              (ns-map *ns*))]
    (ns-unmap *ns* sym))
  )


;; * Implementation


;; * public api


(defn success [x] (ansi/bold-green x))
(defn failure [x] (ansi/bold-red x))


(defn plus-marker [] (ansi/bold-green "+"))
(defn mius-marker [] (ansi/bold-red "-"))
(defn init-prefix [] (ansi/bold-green "+"))
(defn halt-prefix [] (ansi/bold-red "-"))
(defn fan-out [] (ansi/bold-white "<"))
(defn fan-in  [] (ansi/bold-white ">"))


(defmacro info-init [& xs] `(timbre/info (init-prefix) ~@xs))
(defmacro debug-init [& xs] `(timbre/debug (init-prefix) ~@xs))
(defmacro info-halt [& xs] `(timbre/info (halt-prefix) ~@xs))
(defmacro debug-halt [& xs] `(timbre/debug (halt-prefix) ~@xs))


(defn ident [x] (kw/->Ident x))


;; * ns-level-filter


(defn- -compile-ns-level-pred
  "return memoized predicate which returns a boolean"
  [pattern]
  {:pre [(every? (fn [[_ level]] (#{:trace :debug :info :warn :error :fatal :report} level)) pattern)]}
  (fast-memoize
    (fn pass-ns-str
      [^String ns-str level]
      (if-let [[_prefix limit] (some (fn [[prefix :as entry]] (when (str/starts-with? ns-str prefix) entry)) pattern)]
        (timbre/level>= level limit)
        true))))


(defn- ns-level-filter
  "[ [ prefix-str level-kw ] ... ]"
  [pattern]
  (let [-pred (-compile-ns-level-pred pattern)]
    (fn [{:keys [?ns-str level] :as data}]
      (try
        (when (-pred (str ?ns-str) level)
          data)
        (catch Throwable e
          (timbre/error e)
          data)))))


(defn ns-level-filter-middleware
  [pattern]
  (ns-level-filter pattern))


;; * file-appender


(defn create-file-appender
  {:style/indent [0]}
  ([]
   (create-file-appender {}))
  ([{:keys [log-dir
            log-section-name
            logfile-basename
            logfile-extension
            date-separator
            date-format
            retry-limit
            append?]
     :or   {log-dir           "log"
            log-section-name  "default"
            logfile-extension ".log"
            date-separator    \-
            date-format       date-format-yyyy-MM-dd
            retry-limit       2
            append?           true}
     :as   _opts}]
   (timbre.patch/create-file-appender
     {:log-dir           log-dir
      :log-section-name  log-section-name
      :logfile-basename  logfile-basename
      :logfile-extension logfile-extension
      :date-separator    date-separator
      :date-format       date-format
      :retry-limit       retry-limit
      :append?           append?})))


;; * timbre output-fn


(def ^:const ns-str-shortener-threshold-const 16)


(comment
  (fn [{:keys [msg_]}] (force msg_))
  )


(defn make-output-fn
  "Refer to `timbre/default-output-fn`."
  [{:keys
    [stacktrace-opts
     level-transform
     timestamp-transform
     thread-name-transform
     ns-str-transform
     message-transform]
    :or
    {level-transform       (fn [level]
                             ({:report "R"
                               :fatal  "F"
                               :error  "E"
                               :warn   "W"
                               :info   "I"
                               :debug  "D"
                               :trace  "T"} level level))
     timestamp-transform   identity
     thread-name-transform identity
     ns-str-transform      #(user.string.namespace/shorten-ns-str % ns-str-shortener-threshold-const)
     message-transform     (fn [{:keys [vargs ?msg-fmt]}]
                             (if (string? ?msg-fmt)
                               (encore/format* ?msg-fmt vargs)
                               (encore/str-join
                                 " "
                                 (comp
                                   (map encore/nil->str) ; coerce nil value -> str
                                   (map
                                     (fn [o]
                                       (if (satisfies? kw/KeywordRender o)
                                         (kw/-render o)
                                         o)))
                                   (map
                                     (fn [x]
                                       (cond
                                         (record? x) (pr-str x)
                                         :else       x))))
                                 vargs)))
     }}]
  (fn
    [{:keys [level timestamp_ ?err ?ns-str ?file ?line ?msg-fmt]
      :as   data}]
    (let [thread-name (.getName (Thread/currentThread))]
      (str
        (level-transform level) " "
        (timestamp-transform (force timestamp_)) " "
        (thread-name-transform thread-name) " "
        "[" (ns-str-transform (or ?ns-str ?file "?")) (when ?line (str ":" ?line)) "]" " "
        (message-transform data)
        (when-not (:no-stacktrace? stacktrace-opts)
          (when-let [err ?err]
            (str "\n" (timbre/stacktrace err stacktrace-opts))))))))



(defn make-println-output-fn
  [{:as options}]
  (make-output-fn
    (merge
      {:level-transform   (fn [level]
                            ({:report (ansi/bold-white "R")
                              :fatal  (ansi/bold-red "F")
                              :error  (ansi/bold-red "E")
                              :warn   (ansi/bold-magenta "W")
                              :info   (ansi/bold-cyan "I")
                              :debug  (ansi/bold-green "D")
                              :trace  (ansi/bold-yellow "T")} level level))
       :ns-str-transform  #(-> % (user.string.namespace/shorten-ns-str ns-str-shortener-threshold-const) (ansi/bold-green))
       :message-transform (fn [{:keys [vargs ?msg-fmt]}]
                            (if (string? ?msg-fmt)
                              (encore/format* ?msg-fmt vargs)
                              (encore/str-join
                                " "
                                (comp
                                  (map encore/nil->str) ; coerce nil value -> str
                                  (map
                                    (fn [o]
                                      (if (satisfies? kw/KeywordRender o)
                                        (binding [kw/*ansi-enabled* true]
                                          (kw/-render o))
                                        o)))
                                  (map
                                    (fn [x]
                                      (cond
                                        (record? x) (pr-str x)
                                        :else       x))))
                                vargs)))}
      options)))


;; * wrap-up


(defn init!
  ([]
   (init! {}))
  ([{:keys
     [::ctl-use-timbre
      ::ns-level-filter-pattern
      ::ns-str-shortener-threshold
      ::file-appender-options]
     :or
     {ctl-use-timbre             true
      ns-str-shortener-threshold ns-str-shortener-threshold-const
      file-appender-options      {}}
     :as _options}]
   {:pre [(int? ns-str-shortener-threshold)
          (map? file-appender-options)]}


   (when ctl-use-timbre
     (try
       (require 'clojure.tools.logging)
       (require 'taoensso.timbre.tools.logging)
       ((resolve 'taoensso.timbre.tools.logging/use-timbre))
       (catch Throwable _)
       (finally (timbre/trace ::ctl-use-timbre))))


   (when ns-level-filter-pattern
     (timbre/swap-config!
       update
       :middleware
       (fnil conj [])
       (ns-level-filter-middleware ns-level-filter-pattern)))


   (timbre/merge-config!
     {:level          :debug
      :timestamp-opts {:pattern  "EEE HH:mm:ss.SSS"
                       :locale   :jvm-default
                       :timezone utc-9}


      :appenders
      {:stdout
       (merge
         (timbre/println-appender {:stream :std-out})
         {:enabled?       false ; default is false
          :timestamp-opts {:pattern  "EEE HH:mm:ss.SSS"
                           :locale   :jvm-default
                           :timezone utc-9}})


       :file
       {:enabled?       true
        :async?         false
        :min-level      nil
        :rate-limit     nil
        :output-fn      :inherit
        :fn             (create-file-appender file-appender-options)
        :timestamp-opts {:pattern  "HH:mm:ss.SSS"
                         :locale   :jvm-default
                         :timezone utc-9}}}})


   (timbre/swap-config!
     (fn [config]
       (let [base-ns-str-transform #(-> % (user.string.namespace/shorten-ns-str ns-str-shortener-threshold))]
         (-> config
           (assoc
             :output-fn
             (make-output-fn
               {:ns-str-transform base-ns-str-transform}))
           (assoc-in
             [:appenders :println :output-fn]
             (make-println-output-fn
               {:ns-str-transform #(-> % base-ns-str-transform (ansi/bold-green))}))
           (assoc-in
             [:appenders :stdout :output-fn]
             (make-println-output-fn
               {:ns-str-transform #(-> % base-ns-str-transform (ansi/bold-green))}))))))
   nil))


(set! *warn-on-reflection* false)


(comment
  (timbre/info :ang)
  (timbre/error :ang)
  )
