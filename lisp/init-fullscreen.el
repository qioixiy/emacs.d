;; fullscreen

;; (defadvice handle-switch-frame (after maximize-frame activate)
;;   "Maximize frame every time it receives focus, i.e. a switch-frame event."
;;   (w32-send-sys-command #xf030))

(defvar my-fullscreen-p t "Check if fullscreen is on or off")
(defun my-non-fullscreen-w32 ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728)
    (progn (set-frame-parameter nil 'width 82)
           (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen-w32 ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488)
    (set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen-w32 ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen-w32)
    (my-fullscreen-w32)))

(defun minimize-frame-w32 ()
  (interactive)
  (w32-send-sys-command #xf020))

(defun fullscreen ()
  "Fullscreen."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         ;; if first parameter is '1', can't toggle fullscreen status
                         '(1 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun fullscreen-toggle ()
  "Toggle fullscreen status."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         ;; if first parameter is '2', can toggle fullscreen status
                         '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(defun maximize ()
  "Maximize window."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

(defun jbr-init ()
    "Called from term-setup-hook after the default
terminal setup is
done or directly from startup if term-setup-hook not
used.  The value
0xF030 is the command for maximizing a window."
  (interactive)
  (w32-send-sys-command #xf030)
  (ecb-redraw-layout)
  (calendar)
  )

;;(if (window-system)
;;    (maximize))

;;(setq term-setup-hook 'jbr-init)
;;(setq window-setup-hook 'jbr-init)

(provide 'init-fullscreen)
