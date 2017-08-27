;;;

(maybe-require-package 'sr-speedbar)

(require 'sr-speedbar)

(sr-speedbar-refresh-turn-on)
(sr-speedbar-handle-auto-refresh t)
(setq speedbar-use-images nil)

(provide 'init-sr-speedbar)

;;;
