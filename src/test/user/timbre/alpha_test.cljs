(ns user.timbre.alpha-test
  (:require
   [clojure.test :as test :refer [deftest is are testing]]
   [taoensso.timbre :as timbre]
   [user.timbre.alpha :as u.timbre]
   ))


(u.timbre/init!)


(deftest main
  (timbre/info 1)
  (timbre/info true)
  (timbre/info :a)
  (timbre/info 'a)
  (u.timbre/info-init :init)
  (u.timbre/info-halt :halt)
  (timbre/info (u.timbre/ident :kw)))
