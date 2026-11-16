;; -*- lexical-binding: t; -*-
;; Surface org-agenda deadlines/scheduled items as real desktop notifications.

(require 'appt)
(require 'org-agenda)

(setq appt-display-format 'window
      appt-display-interval 5
      appt-message-warning-time 15)

(defun my/appt-notify (min-to-app new-time appt-msg)
  "Send APPT-MSG as a desktop notification, due in MIN-TO-APP minutes."
  (call-process "notify-send" nil 0 nil
                "-u" "normal" "-a" "Org Agenda"
                (format "In %s minutes" min-to-app)
                appt-msg))

(setq appt-disp-window-function #'my/appt-notify
      appt-delete-window-function #'ignore)

(appt-activate 1)
(org-agenda-to-appt t)
(run-with-timer 0 (* 10 60) (lambda () (org-agenda-to-appt t)))

(provide 'init.appt)
