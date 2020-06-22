(ns user.timbre.alpha
  (:require-macros
   user.timbre.alpha)
  (:require
   [taoensso.timbre :as timbre]
   [user.clojurescript.env.alpha :as u.cljs.env]
   [user.string.namespace]
   [user.timbre.alpha.ident :as id]
   ))


(def ^:const ns-str-shortener-threshold-const 16)


;;


(defn plus-marker [] "+")
(defn mius-marker [] "-")
(defn init-prefix [] "+")
(defn halt-prefix [] "-")
(defn fan-out [] "<")
(defn fan-in  [] ">")


(defn ident [x] (id/->Ident x))


;;


(defn make-output-fn
  "Refer to `timbre/default-output-fn`."
  [{:keys
    [stacktrace-opts
     level-transform
     ns-str-transform
     message-transform]
    :or
    {level-transform   (fn [level]
                         ({:report "R"
                           :fatal  "F"
                           :error  "E"
                           :warn   "W"
                           :info   "I"
                           :debug  "D"
                           :trace  "T"} level level))
     ns-str-transform  #(user.string.namespace/shorten-ns-str % ns-str-shortener-threshold-const)
     message-transform (fn [{:keys [msg_]}] (force msg_))
     }}]
  (fn
    [{:keys [level timestamp_ ?err ?ns-str ?file ?line ?msg-fmt]
      :as   data}]
    (str
      (level-transform level) " "
      "[" (ns-str-transform (or ?ns-str ?file "?")) (when ?line (str ":" ?line)) "] "
      (message-transform data)
      (when-not (:no-stacktrace? stacktrace-opts)
        (when-let [err ?err]
          (str "\n" (timbre/stacktrace err stacktrace-opts)))))))


(defn init!
  ([]
   (init! {}))
  ([{:keys [::enable-raw-console?
            ::ns-str-shortener-threshold]
     :or   {enable-raw-console?        (u.cljs.env/if-nodejs-target false true)
            ns-str-shortener-threshold ns-str-shortener-threshold-const}
     :as   options}]
   {:pre [(boolean? enable-raw-console?)
          (int? ns-str-shortener-threshold-const)]}
   (when enable-raw-console?
     (timbre/swap-config!
       update :middleware conj
       (fn [data]
         (-> data (update :raw-console? #(if (some? %) % true))))))


   (timbre/swap-config!
     (fn [config]
       (let [base-ns-str-transform #(-> % (user.string.namespace/shorten-ns-str ns-str-shortener-threshold))]
         (-> config
           (assoc :output-fn (make-output-fn {:ns-str-transform base-ns-str-transform}))))))))


(comment
  (init!)
  (timbre/swap-config! assoc :middleware [])
  (timbre/info "asdf")
  (timbre/info ::a)
  )
