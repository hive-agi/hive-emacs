;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((clojure-mode
  . ((cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-aliases . ":dev")
     (cider-jack-in-nrepl-middlewares . ("cider.nrepl/cider-middleware"
                                         "clojure-elisp.nrepl/wrap-cljel"))))
 (clojure-elisp-mode
  . ((cider-preferred-build-tool . clojure-cli)
     (cider-clojure-cli-aliases . ":dev")
     (cider-jack-in-nrepl-middlewares . ("cider.nrepl/cider-middleware"
                                         "clojure-elisp.nrepl/wrap-cljel")))))
