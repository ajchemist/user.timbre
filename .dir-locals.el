((clojure-mode
  ;; (cider-clojure-cli-global-options . "-A:provided:test")
  (cider-clojure-cli-parameters . "-A:provided:test -m nrepl.cmdline --middleware '%s'")
  (clojure-local-source-path . "src/core")
  (clojure-local-source-test-path . "src/test")))
