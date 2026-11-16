;; -*- lexical-binding: t; -*-

(require 'json)
(require 'seq)
(require 'subr-x)
(require 'xdg)

(defun my/hypr--instance ()
  "Directory of the newest live Hyprland instance in $XDG_RUNTIME_DIR/hypr, or nil."
  (when-let* ((runtime (getenv "XDG_RUNTIME_DIR"))
              (root (expand-file-name "hypr" runtime))
              ((file-directory-p root)))
    (car (sort (seq-filter (lambda (dir)
                             (file-exists-p (expand-file-name "hyprland.lock" dir)))
                           (directory-files root t "\\`[^.]"))
               (lambda (a b)
                 (time-less-p (file-attribute-modification-time (file-attributes b))
                              (file-attribute-modification-time (file-attributes a))))))))

(defun my/hypr--call (&rest args)
  "Run `hyprctl ARGS...' against the live instance and return its trimmed output."
  (let* ((instance (my/hypr--instance))
         (process-environment
          (if instance
              (cons (concat "HYPRLAND_INSTANCE_SIGNATURE=" (file-name-nondirectory instance))
                    process-environment)
            process-environment)))
    (with-temp-buffer
      (let ((status (apply #'call-process "hyprctl" nil t nil args)))
        (unless (eq status 0)
          (user-error "hyprctl %s: exit %s: %s"
                      (string-join args " ") status (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun my/hypr--json (&rest args)
  "Run `hyprctl -j ARGS...' and parse the result into an alist."
  (json-parse-string (apply #'my/hypr--call "-j" args)
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun my/hypr--lua-string (string)
  "Quote STRING as a Lua long string."
  (when (string-match-p "\\]==\\]" string)
    (user-error "Cannot quote %S for Lua" string))
  (concat "[==[" string "]==]"))

(defun my/hypr-dispatch (dispatcher)
  "Run the Lua dispatcher expression DISPATCHER, e.g. \"hl.dsp.exit()\"."
  (let ((out (my/hypr--call "dispatch" dispatcher)))
    (unless (member out '("" "ok"))
      (message "hyprctl dispatch: %s" out))
    out))

(defconst my/hypr--popup-title-regexp
  "\\`emacs-\\(launcher\\|windows\\|capture\\|power\\)\\'")

(defun my/hypr--clients ()
  "Visible windows, most recently focused first, without our own popups."
  (let ((clients (seq-filter
                  (lambda (c)
                    (and (alist-get 'mapped c)
                         (not (alist-get 'hidden c))
                         (not (string-match-p my/hypr--popup-title-regexp
                                              (or (alist-get 'initialTitle c) "")))))
                  (my/hypr--json "clients"))))
    (sort clients
          (lambda (a b)
            (< (or (alist-get 'focusHistoryID a) most-positive-fixnum)
               (or (alist-get 'focusHistoryID b) most-positive-fixnum))))))

(defun my/hypr--window-candidates ()
  "Alist (LABEL . ADDRESS) of `my/hypr--clients'."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar (lambda (c)
              (let* ((label (format "%-6s %-20s %s"
                                    (or (alist-get 'name (alist-get 'workspace c)) "?")
                                    (truncate-string-to-width
                                     (or (alist-get 'class c) "") 20)
                                    (or (alist-get 'title c) "")))
                     (n (puthash label (1+ (gethash label seen 0)) seen)))
                (cons (if (> n 1) (format "%s <%d>" label n) label)
                      (alist-get 'address c))))
            (my/hypr--clients))))

(defun my/hypr--completing-read-ordered (prompt candidates)
  "`completing-read' over the strings CANDIDATES, keeping their order."
  (completing-read prompt
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity)
                                    (cycle-sort-function . identity))
                       (complete-with-action action candidates string pred)))
                   nil t))

(defun my/hypr-switch-window ()
  "Focus a Hyprland window chosen with completion."
  (interactive)
  (let ((candidates (my/hypr--window-candidates)))
    (unless candidates (user-error "No windows"))
    (let ((address (cdr (assoc (my/hypr--completing-read-ordered
                                "Window: " (mapcar #'car candidates))
                               candidates))))
      (unless (and address (string-match-p "\\`0x[0-9a-f]+\\'" address))
        (user-error "Unexpected window address %S" address))
      (my/hypr-dispatch
       (format "hl.dsp.focus({ window = %s })"
               (my/hypr--lua-string (concat "address:" address)))))))

(global-set-key (kbd "C-c h w") #'my/hypr-switch-window)

(defvar my/launcher-extra-data-dirs
  (list "/run/current-system/sw/share"
        (format "/etc/profiles/per-user/%s/share" (user-login-name))
        (expand-file-name "~/.nix-profile/share")
        "/var/lib/flatpak/exports/share"
        (expand-file-name "~/.local/share/flatpak/exports/share"))
  "Data directories searched for applications besides `xdg-data-dirs'.")

(defun my/launcher--entries ()
  "Alist (NAME . DESKTOP-FILE) of launchable applications, sorted by name."
  (let ((seen (make-hash-table :test #'equal))
        (names (make-hash-table :test #'equal))
        entries)
    (dolist (dir (delete-dups (append (list (xdg-data-home))
                                      (xdg-data-dirs)
                                      my/launcher-extra-data-dirs)))
      (let ((appdir (expand-file-name "applications" dir)))
        (when (file-directory-p appdir)
          (dolist (file (directory-files-recursively appdir "\\.desktop\\'"))
            (let ((id (subst-char-in-string ?/ ?- (file-relative-name file appdir))))
              (unless (gethash id seen)
                (puthash id t seen)
                (let* ((desk (ignore-errors (xdg-desktop-read-file file)))
                       (name (and desk (gethash "Name" desk))))
                  (when (and name
                             (equal (gethash "Type" desk) "Application")
                             (gethash "Exec" desk)
                             (not (equal (gethash "NoDisplay" desk) "true"))
                             (not (equal (gethash "Hidden" desk) "true"))
                             (let ((try (gethash "TryExec" desk)))
                               (or (null try) (executable-find try))))
                    (let ((n (puthash name (1+ (gethash name names 0)) names)))
                      (push (cons (if (> n 1) (format "%s (%s)" name id) name) file)
                            entries))))))))))
    (sort entries (lambda (a b) (string-collate-lessp (car a) (car b) nil t)))))

(defun my/launcher--launch (file)
  "Start the application described by the .desktop FILE, through Hyprland."
  (unless (executable-find "gio")
    (user-error "gio (glib) not found"))
  (my/hypr-dispatch
   (format "hl.dsp.exec_cmd(%s)"
           (my/hypr--lua-string (concat "gio launch " (shell-quote-argument file))))))

(defun my/popup--notify (message)
  (when (executable-find "notify-send")
    (call-process "notify-send" nil 0 nil "Emacs popup" message)))

(defmacro my/popup--with-frame (&rest body)
  "Run BODY, then delete the frame that was selected when it started."
  (declare (indent 0))
  (let ((frame (make-symbol "frame")))
    `(let ((,frame (selected-frame)))
       (unwind-protect
           (condition-case err
               (progn ,@body)
             (quit nil)
             (error (my/popup--notify (error-message-string err))))
         (when (and (frame-live-p ,frame) (cdr (frame-list)))
           (delete-frame ,frame))))))

(defun my/popup-launcher ()
  "Pick an application and start it."
  (my/popup--with-frame
    (let* ((entries (my/launcher--entries))
           (choice (my/hypr--completing-read-ordered "Run: " (mapcar #'car entries))))
      (my/launcher--launch (cdr (assoc choice entries))))))

(defun my/popup-windows ()
  "Focus a Hyprland window."
  (my/popup--with-frame
    (my/hypr-switch-window)))

(defun my/popup-power ()
  "Lock, log out, suspend, reboot or power off."
  (my/popup--with-frame
    (pcase (completing-read "Power: " '("lock" "logout" "suspend" "reboot" "poweroff") nil t)
      ("lock"     (call-process "loginctl" nil 0 nil "lock-session"))
      ("suspend"  (call-process "systemctl" nil 0 nil "suspend"))
      ("logout"   (when (y-or-n-p "Log out of Hyprland? ")
                    (my/hypr-dispatch "hl.dsp.exit()")))
      ("reboot"   (when (y-or-n-p "Reboot? ")
                    (call-process "systemctl" nil 0 nil "reboot")))
      ("poweroff" (when (y-or-n-p "Power off? ")
                    (call-process "systemctl" nil 0 nil "poweroff"))))))

(defun my/popup-capture ()
  "Run `org-capture' in the popup frame and close it when finished."
  (require 'org-capture)
  (let ((frame (selected-frame)))
    (letrec ((cleanup (lambda ()
                        (remove-hook 'org-capture-after-finalize-hook cleanup)
                        (when (and (frame-live-p frame) (cdr (frame-list)))
                          (delete-frame frame)))))
      (add-hook 'org-capture-after-finalize-hook cleanup)
      (condition-case nil
          (progn (org-capture) (delete-other-windows))
        (quit (funcall cleanup))))))

(defun my/popup-dired ()
  "Open ~ in dirvish, or dired if dirvish is not loaded."
  (let ((dir (expand-file-name "~/")))
    (if (fboundp 'dirvish) (dirvish dir) (dired dir))))

(provide 'init.hyprland)
