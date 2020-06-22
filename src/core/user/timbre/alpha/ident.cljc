(ns user.timbre.alpha.ident
  (:require
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   #?(:clj
      [io.aviso.ansi :as ansi]
      )
   ))


(def ^:dynamic *color-enabled* false)


(defprotocol IdentRender
  (-render [o]))


(defn- render-kw
  [kw]
  {:pre [(keyword? kw)]}
  (let [ns (namespace kw)]
    (cond
      (string? ns)
      (let [ns'   ns
            name  (name kw)
            name' (if *color-enabled* #?(:clj (ansi/bold-green name) :cljs name) name)]
        (if (str/blank? ns')
          name'
          (str ns' "/" name')))

      :else
      (str kw))))


(deftype Ident [ident]
  Object
  (toString [_] (str ident))
  IdentRender
  (-render [x] (render-kw (.-ident x))))


#?(:clj
   (defmethod print-method Ident
     [x ^java.io.Writer w]
     (.write w (str (.-ident x))))
   :cljs
   (extend-protocol IPrintWithWriter
     Ident
     (-pr-writer [x w _]
       (write-all w (str (.-ident x))))))
