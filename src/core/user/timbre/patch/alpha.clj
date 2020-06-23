(ns user.timbre.patch.alpha
  (:require
   [clojure.java.io :as jio]
   [taoensso.timbre :as timbre]
   [taoensso.encore :as encore]
   [user.java.io.alpha :as u.jio]
   )
  (:import
   java.io.File
   java.util.Date
   java.text.SimpleDateFormat
   ))


(declare with-appenders)


;; * Generators


(defn- list-
  "use only at compile time"
  [& xs]
  (->> xs
    (into () (remove nil?))
    (reverse)))


(defn- gen-output-fn
  [{:keys [style stacktrace]
    :as   recipe}]
  (let [{style-level       :level
         style-thread-name :thread-name
         style-timestamp   :timestamp
         style-ns-str      :ns-str
         :or
         {style-level {:fatal "F"
                       :error "E"
                       :warn  "W"
                       :info  "I"
                       :debug "D"
                       :trace "T"}}} style
        bindings-over-lambda
        (into
          []
          (mapcat
            (fn [[sym f]]
              (when f
                [sym `(let [f# ~f]
                        (assert (ifn? f#)) ; assertion for debugging
                        f#)])))
          [['style-level style-level]
           ['style-timestamp style-timestamp]
           ['style-thread-name style-thread-name]
           ['style-ns-str style-ns-str]])]
    (list
      'clojure.core/let
      bindings-over-lambda
      (list
        'fn '[{:keys [level timestamp_ ?err ?ns-str ?line msg_] :as data}]
        (list
          'clojure.core/let
          ['thread-name '(.getName (Thread/currentThread))]
          (list
            'clojure.core/str
            (if style-level
              '(style-level level)
              'level)
            " "
            (if style-timestamp
              '(style-timestamp (force timestamp_))
              '(force timestamp_))
            " "
            (if style-thread-name
              '(style-thread-name thread-name)
              'thread-name)
            " ["
            (if style-ns-str
              '(or (style-ns-str ?ns-str) "?")
              '(or ?ns-str "?"))
            '(when ?line (str ":" ?line))
            "] "
            '(force msg_)
            (when (:stacktrace? stacktrace true)
              `(when-let [~'err ~'?err]
                 (str "\n" (timbre/stacktrace ~'err ~stacktrace))))))))))


(defn- gen-appender
  "date-format is an instance of java.text.SimpleDateFormat"
  {:style/indent [:defn]}
  [{:keys [file-path
           file-basename
           file-ext
           date-separator
           date-format
           append?]
    :or {file-ext "log"
         date-separator \-}
    :as opts}]
  (let [file (list-
              'clojure.java.io/file (when file-path 'path)
              (reverse
               (into
                 ()
                 (comp
                   cat
                   (remove nil?))
                 ['[str]
                  (when file-basename '[basename])
                  (when date-format '[date-separator (. date-format format (Date.))])
                  (when file-ext '[\. ext])])))
        bindings-over-lambda
        (into
          ['path file-path
           'basename file-basename
           'ext file-ext]
          (mapcat (fn [[sym o]] (when o [sym o])))
          [['date-format date-format]])]
    (list-
     'clojure.core/let
     bindings-over-lambda
     (when file-path
       '(when (string? path)
          (. (clojure.java.io/file path) mkdirs)))
     '(assert (and (string? basename) (re-matches #"[^/]*" basename)))
     '(assert (and (string? ext) (re-matches #"[^/]*" ext)))
     (when date-format
       '(assert (instance? java.text.SimpleDateFormat date-format)))
     (list
      'fn 'self '[{:keys [output_] :as data}]
      (list
       'try
       `(spit ~file (str (force ~'output_) "\n") :append ~(or append? true))
       '(catch java.io.IOException e
          (if (:__spit-appender/retry? data)
            (with-appenders [:println]
              (timbre/error e))
            (self (assoc data :__spit-appender/retry? true)))))))))


(defn create-file-appender
  "date-format is an instance of java.text.SimpleDateFormat"
  {:style/indent [0]}
  [{:keys [log-dir
           log-section-name
           logfile-basename
           logfile-extension
           date-separator
           ^SimpleDateFormat date-format
           retry-limit
           append?
           on-retry-limit]
    :or   {on-retry-limit (fn [t] (with-appenders [:println] (timbre/error t)))}
    :as   opts}]
  {:pre [(some? log-dir)
         (string? log-section-name)
         (or (nil? logfile-basename)
             (and (string? logfile-basename) (re-matches #"[^/]*" logfile-basename)))
         (and (string? logfile-extension) (re-matches #"\.[^/]+" logfile-extension))
         (instance? java.text.SimpleDateFormat date-format)
         (number? retry-limit)
         (boolean? append?)
         (fn? on-retry-limit)]}
  (let [log-dir (u.jio/path log-dir)]
    ;; make sure parent existence
    (. (jio/file log-dir log-section-name) mkdirs)
    ;;
    (let [-appender-target-file
          (if (and (string? logfile-basename) (re-matches #"[^/]*" logfile-basename))
            #(jio/file log-dir log-section-name (str logfile-basename date-separator (. date-format format (Date.)) logfile-extension))
            #(jio/file log-dir log-section-name (str (. date-format format (Date.)) logfile-extension)))]
      (fn self
        [{:keys [output_] :as data}]
        (try
          (spit (-appender-target-file)
                (str (force output_) "\n") :append append?)
          (catch Throwable t
            (let [{retry?      :__spit-appender/retry?
                   retry-count :__spit-appender/retry-count} data]
              (if (and retry? (< retry-count retry-limit))
                (on-retry-limit t)
                (self
                  (assoc data
                    :__spit-appender/retry? true
                    :__spit-appender/retry-count
                    (if (number? retry-count)
                      (unchecked-inc-int retry-count)
                      0)))))))))))


(defmacro output-fn*
  [recipe]
  (gen-output-fn recipe))


;; * Appenders


(defmacro with-appenders
  {:style/indent [1]}
  [appenders & body]
  `(binding [timbre/*config* (update timbre/*config* :appenders select-keys ~appenders)]
     ~@body))


(defmacro appender*
  {:style/indent [:defn]}
  [recipe]
  (gen-appender recipe))


(comment
  (with-appenders [:stdout :println]
    (timbre/info :stdout))
  )
