(ns user.timbre.extensions.default
  (:require
   [clojure.string :as str]
   [clojure.main :refer [demunge]]
   [taoensso.timbre :as timbre]
   [io.aviso.ansi :as ansi]
   [user.timbre.alpha.keyword :as kw]
   ))


(extend clojure.lang.AFunction
  kw/KeywordRender
  {:-render
   (fn [^clojure.lang.AFunction f]
     (let [^String demunged (demunge (.. f getClass getName))]
       (if kw/*ansi-enabled*
         (let [last-index (str/last-index-of demunged "/")]
           (if (int? last-index)
             (str ansi/yellow-font (subs demunged 0 last-index) "/"
                  ansi/bold-yellow-font (subs demunged (unchecked-inc-int last-index)) ansi/reset-font)
             demunged))
         demunged)))})
