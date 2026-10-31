;; -*- mode: eask; lexical-binding: t -*-

(package "ek9"
         "0.1.0" ; x-cicada-version
         "Kubernetes client similar to k9s")

(website-url "https://github.com/Boenan/ek9")
(keywords "tools")

(package-file "ek9.el")
(files "ek9.el"
       "ek9-action.el"
       "ek9-context.el"
       "ek9-describe.el"
       "ek9-edit.el"
       "ek9-exec.el"
       "ek9-list.el"
       "ek9-logs.el"
       "ek9-metrics.el"
       "ek9-portfwd.el"
       "ek9-process.el"
       "ek9-proxy.el"
       "ek9-resource.el"
       "ek9-saved-view.el"
       "ek9-transient.el"
       "ek9-types.el")

(script "test" "eask test ert test/*-test.el")
(script "integration" "test/integration/run.sh")

(source "gnu")
(source "melpa")

(depends-on "emacs" "30.2")

;; Eask runs Emacs with `user-emacs-directory' in .eask.  The level
;; high makes the Network Security Manager save the fingerprint of each
;; new host.  Keep this file in .eask too, so that Eask does not need
;; ~/.emacs.d (it is missing in CI) and does not change the personal file.
(setq network-security-level 'high
      nsm-settings-file (expand-file-name "network-security.data"
                                          user-emacs-directory))
