(ns user.timbre.extensions.integrant
  (:require
   [integrant.core :as ig]
   [user.timbre.alpha.keyword :as kw]
   #?(:clj [user.timbre.extensions.default])
   ))


(defn normalize-key
  "Ref `ig/normalize-key`"
  [k]
  (if (vector? k) (ig/composite-keyword k) k))


(extend-type user.timbre.alpha.keyword.Ident
  kw/KeywordRender
  (-render [ident]
    (kw/-render (normalize-key (.-ident ident)))))
