(ns user.timbre.extensions.integrant
  (:require
   [taoensso.timbre :as timbre]
   [integrant.core :as ig]
   [user.timbre.alpha :as u.timbre]
   [user.timbre.extensions.default]
   ))


(def normalize-key @(resolve 'ig/normalize-key))


(extend user.timbre.alpha.Ident
  u.timbre/TimbreTransform
  {:-transform
   (fn [^user.timbre.alpha.Ident ident]
     (u.timbre/ident-kw-transform (normalize-key (.-ident ident))))})
