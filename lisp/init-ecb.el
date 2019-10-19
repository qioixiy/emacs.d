;;;

(maybe-require-package 'ecb)

(require 'ecb)

;; ecb depend cedet plugin
(require 'semantic/analyze)
(provide 'semantic-analyze)
(provide 'semantic-ctxt)
(provide 'semanticdb)
(provide 'semanticdb-find)
(provide 'semanticdb-mode)
(provide 'semantic-load)

(setq stack-trace-on-error t)
;;(setq ecb-auto-activate t)
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)
(setq ecb-source-path '("."
                        "~"
                        "/"))
(setq ecb-layout-name "right1")
(global-set-key (kbd "C-c e a") 'ecb-activate)
(global-set-key (kbd "C-c e d") 'ecb-deactivate)

(provide 'init-ecb)
