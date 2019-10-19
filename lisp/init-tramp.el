;;; init-tramp.el --- tramp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'tramp)

;; C-x C-f /plink:usename@hostname:~/work
(setq tramp-default-method "plink")

;; C-x C-f /ssh:usename@hostname:~/work
;;(setq tramp-default-method "ssh")

(provide 'init-tramp)

;;; init-tramp.el ends here
