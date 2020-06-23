(ns user.timbre.extensions.integrant-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [taoensso.timbre :as timbre]
   [user.timbre.alpha :as u.timbre]
   [user.timbre.extensions.integrant]
   ))


(deftest main
  (timbre/info (u.timbre/ident [:group/a ::x])))
