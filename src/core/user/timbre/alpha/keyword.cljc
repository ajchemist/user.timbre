(ns user.timbre.alpha.keyword
  (:require
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   #?(:clj
      [io.aviso.ansi :as ansi]
      )
   ))


(def ^:dynamic *ansi-enabled* false)


(defprotocol KeywordRender
  (-render [o]))


(defn- render-kw
  [kw]
  {:pre [(keyword? kw)]}
  (let [ns (namespace kw)]
    (cond
      (string? ns)
      (let [ns'   ns
            name  (name kw)
            name' (if *ansi-enabled* #?(:clj (ansi/bold-green name) :cljs name) name)]
        (if (str/blank? ns')
          name'
          (str ns' "/" name')))

      :else
      (str kw))))


(extend-protocol KeywordRender
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core/Keyword)
  (-render [x] (render-kw x)))


(deftype Ident [ident]
  Object
  (toString [_] (str ident)))


(extend-type Ident
  KeywordRender
  (-render [x] (-render (.-ident x))))


#?(:clj
   (defmethod print-method Ident
     [x ^java.io.Writer w]
     (.write w (str (.-ident x))))
   :cljs
   (extend-protocol IPrintWithWriter
     Ident
     (-pr-writer [x w _]
       (write-all w (str (.-ident x))))))
