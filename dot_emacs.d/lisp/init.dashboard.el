;; -*- lexical-binding: t; -*-
;; Modern dashboard with project workflow integration

(defgroup my/dashboard nil
  "Dashboard settings."
  :group 'convenience)

(defvar my/dashboard-buffer-name "*Dashboard*"
  "Name of the dashboard buffer.")

;; ============================================================
;; Faces
;; ============================================================
(defface my/dashboard-banner
  '((t :inherit font-lock-keyword-face :height 1.3))
  "Face for dashboard banner.")

(defface my/dashboard-subtitle
  '((t :inherit shadow :height 1.0))
  "Face for dashboard subtitle.")

(defface my/dashboard-heading
  '((t :inherit font-lock-type-face :height 1.1 :weight bold))
  "Face for dashboard section headings.")

(defface my/dashboard-separator
  '((t :inherit shadow))
  "Face for visual separators.")

(defface my/dashboard-item
  '((t :inherit default))
  "Face for dashboard items.")

(defface my/dashboard-shortcut
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for dashboard shortcuts.")

(defface my/dashboard-path
  '((t :inherit shadow :height 0.85))
  "Face for file paths.")

(defface my/dashboard-badge
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face for project badges.")

(defface my/dashboard-dim
  '((t :inherit shadow))
  "Face for de-emphasized text.")

(defface my/dashboard-celestial
  '((t :inherit font-lock-string-face))
  "Face for celestial/astrological info.")

(defface my/dashboard-celestial-dim
  '((t :inherit shadow :slant italic))
  "Face for secondary celestial info.")

(defvar my/dashboard-items
  '((projects . 6)
    (recents . 6)
    (agenda . 4))
  "Items to show on dashboard with counts.")

;; ============================================================
;; Helper functions
;; ============================================================

(defun my/dashboard--insert-centered (text)
  "Insert TEXT centered in the window."
  (let* ((width (window-width))
         (visible-len (string-width text))
         (pad (max 0 (/ (- width visible-len) 4))))
    (insert (make-string pad ?\s))
    (insert text)))

(defun my/dashboard--insert-separator ()
  "Insert a subtle separator line."
  (let* ((width (min 64 (- (window-width) 8)))
         (pad (max 0 (/ (- (window-width) width) 2))))
    (insert (make-string pad ?\s))
    (insert (propertize (make-string width ?─) 'face 'my/dashboard-separator))
    (insert "\n")))

(defun my/dashboard--section-icon (section)
  "Return icon for SECTION if nerd-icons available."
  (if (fboundp 'nerd-icons-mdicon)
      (pcase section
        ('recents (nerd-icons-mdicon "nf-md-file_document_multiple"))
        ('projects (nerd-icons-mdicon "nf-md-folder_multiple"))
        ('agenda (nerd-icons-mdicon "nf-md-calendar_check"))
        ('bookmarks (nerd-icons-mdicon "nf-md-bookmark_multiple"))
        (_ ""))
    ""))

(defun my/dashboard--file-icon (file)
  "Return icon for FILE if nerd-icons available."
  (if (fboundp 'nerd-icons-icon-for-file)
      (concat (nerd-icons-icon-for-file file) " ")
    ""))

;; ============================================================
;; Section renderers
;; ============================================================

(defun my/dashboard--insert-recents (count)
  "Insert COUNT recent files."
  (require 'recentf)
  (recentf-mode 1)
  (when recentf-list
    (let ((files (seq-take (seq-filter #'file-exists-p recentf-list) count))
          (idx 0))
      (dolist (file files)
        (let* ((short-name (abbreviate-file-name file))
               (dir (propertize (file-name-directory short-name) 'face 'my/dashboard-path))
               (name (file-name-nondirectory short-name))
               (key (if (< idx 9) (number-to-string (1+ idx)) nil)))
          (insert "      ")
          (when key
            (insert (propertize (format "%s " key) 'face 'my/dashboard-shortcut))
            (local-set-key (kbd key)
                           (let ((f file)) (lambda () (interactive) (find-file f)))))
          (insert (my/dashboard--file-icon file))
          (insert-text-button name
                              'action (lambda (_) (find-file file))
                              'follow-link t
                              'face 'my/dashboard-item
                              'help-echo file)
          (insert "  ")
          (insert dir)
          (insert "\n")
          (setq idx (1+ idx)))))))

(defun my/dashboard--insert-projects (count)
  "Insert COUNT recent projects with quick actions."
  (let ((projects (cond
                   ((and (fboundp 'projectile-relevant-known-projects)
                         (bound-and-true-p projectile-mode))
                    (seq-take (projectile-relevant-known-projects) count))
                   ((fboundp 'project-known-project-roots)
                    (seq-take (project-known-project-roots) count))
                   (t nil))))
    (if (not projects)
        (progn
          (insert "      ")
          (insert (propertize "No projects yet. Press 'n' to create one." 'face 'shadow))
          (insert "\n"))
      (dolist (proj projects)
        (let* ((name (file-name-nondirectory (directory-file-name proj)))
               (short-path (abbreviate-file-name proj))
               (has-envrc (file-exists-p (expand-file-name ".envrc" proj)))
               (has-flake (file-exists-p (expand-file-name "flake.nix" proj)))
               (has-git (file-exists-p (expand-file-name ".git" proj))))
          (insert "      ")
          (insert (if (fboundp 'nerd-icons-mdicon)
                      (concat (cond
                               (has-flake (nerd-icons-mdicon "nf-md-nix"))
                               (has-git (nerd-icons-mdicon "nf-md-git"))
                               (t (nerd-icons-mdicon "nf-md-folder")))
                              " ")
                    ""))
          (insert-text-button name
                              'action (lambda (_)
                                        (let ((default-directory proj))
                                          (if (fboundp 'projectile-switch-project-by-name)
                                              (projectile-switch-project-by-name proj)
                                            (project-find-file))))
                              'follow-link t
                              'face 'my/dashboard-item
                              'help-echo proj)
          (when has-envrc
            (insert (propertize " env" 'face 'my/dashboard-badge)))
          (when has-flake
            (insert (propertize " nix" 'face 'my/dashboard-badge)))
          (insert "  ")
          (insert (propertize short-path 'face 'my/dashboard-path))
          (insert "\n"))))))

(defun my/dashboard--insert-agenda (count)
  "Insert COUNT agenda items."
  (when (and (fboundp 'org-agenda-get-day-entries)
             (boundp 'org-agenda-files)
             org-agenda-files)
    (require 'org-agenda)
    (let* ((today (calendar-current-date))
           (entries (ignore-errors
                      (org-agenda-get-day-entries
                       (org-agenda-files nil 'ifmode)
                       today))))
      (if entries
          (let ((items (seq-take entries count)))
            (dolist (entry items)
              (let ((txt (get-text-property 0 'txt entry)))
                (insert "      ")
                (insert (if (fboundp 'nerd-icons-mdicon)
                            (concat (nerd-icons-mdicon "nf-md-checkbox_blank_outline") " ")
                          "- "))
                (insert (propertize (or txt "Unknown") 'face 'my/dashboard-item))
                (insert "\n"))))
        (insert "      ")
        (insert (propertize "No agenda items for today" 'face 'shadow))
        (insert "\n")))))

;; ============================================================
;; Celestial / planetary section
;; ============================================================

;; Location: Weilheim in Oberbayern
(setq calendar-latitude 47.84
      calendar-longitude 11.14
      calendar-location-name "Weilheim, DE")

(defvar my/zodiac-signs
  '((0 . "Aries") (30 . "Taurus") (60 . "Gemini") (90 . "Cancer")
    (120 . "Leo") (150 . "Virgo") (180 . "Libra") (210 . "Scorpio")
    (240 . "Sagittarius") (270 . "Capricorn") (300 . "Aquarius") (330 . "Pisces"))
  "Zodiac signs by ecliptic longitude degree.")

(defvar my/zodiac-symbols
  '("Aries" "Taurus" "Gemini" "Cancer" "Leo" "Virgo"
    "Libra" "Scorpio" "Sagittarius" "Capricorn" "Aquarius" "Pisces")
  "Zodiac sign names in order.")

(defvar my/planetary-days
  '((0 . ("Sun" "Sol"))
    (1 . ("Moon" "Luna"))
    (2 . ("Mars" "Tyr"))
    (3 . ("Mercury" "Woden"))
    (4 . ("Jupiter" "Thor"))
    (5 . ("Venus" "Freya"))
    (6 . ("Saturn" "Saturn")))
  "Day of week to ruling planet.")

(defvar my/planetary-hour-sequence
  '("Saturn" "Jupiter" "Mars" "Sun" "Venus" "Mercury" "Moon")
  "Chaldean order of planetary hours.")

(defun my/celestial--sun-sign ()
  "Return current sun zodiac sign."
  (require 'solar)
  (let* ((exact-time (decode-time))
         (day (nth 3 exact-time))
         (month (nth 4 exact-time))
         (year (nth 5 exact-time))
         (lng (ignore-errors
                (solar-longitude
                 (calendar-astro-from-absolute
                  (calendar-absolute-from-gregorian
                   (list month day year)))))))
    (if lng
        (let ((sign "Pisces"))
          (dolist (entry my/zodiac-signs sign)
            (when (>= lng (car entry))
              (setq sign (cdr entry)))))
      "unknown")))

(defun my/celestial--moon-phase-info ()
  "Return (PHASE-NAME DAYS-TO-NEXT NEXT-TYPE) for current moon."
  (require 'lunar)
  (let* ((now (decode-time))
         (month (nth 4 now))
         (year (nth 5 now))
         (today (calendar-absolute-from-gregorian
                 (list month (nth 3 now) year)))
         (phases (lunar-phase-list month year))
         (next-phase nil)
         (prev-phase nil))
    ;; Find surrounding phases
    (dolist (phase phases)
      (let ((phase-abs (calendar-absolute-from-gregorian (car phase))))
        (if (<= phase-abs today)
            (setq prev-phase phase)
          (unless next-phase
            (setq next-phase phase)))))
    ;; If no next phase found, check next month
    (unless next-phase
      (let* ((next-month (if (= month 12) 1 (1+ month)))
             (next-year (if (= month 12) (1+ year) year)))
        (dolist (phase (lunar-phase-list next-month next-year))
          (let ((phase-abs (calendar-absolute-from-gregorian (car phase))))
            (when (and (> phase-abs today) (not next-phase))
              (setq next-phase phase))))))
    (let* ((next-abs (when next-phase
                       (calendar-absolute-from-gregorian (car next-phase))))
           (days-to (when next-abs (- next-abs today)))
           (next-type (when next-phase (nth 2 next-phase)))
           ;; 0=new 1=first-quarter 2=full 3=last-quarter
           (prev-type (if prev-phase (nth 2 prev-phase) 0))
           (phase-name (pcase prev-type
                         (0 "New Moon (waxing)")
                         (1 "First Quarter (waxing)")
                         (2 "Full Moon (waning)")
                         (3 "Last Quarter (waning)")
                         (_ "Unknown")))
           (moon-icon (pcase prev-type
                        (0 "nf-md-moon_new")
                        (1 "nf-md-moon_first_quarter")
                        (2 "nf-md-moon_full")
                        (3 "nf-md-moon_last_quarter")
                        (_ "nf-md-moon_waning_crescent")))
           (next-name (pcase next-type
                        (0 "New Moon")
                        (1 "First Quarter")
                        (2 "Full Moon")
                        (3 "Last Quarter")
                        (_ "?"))))
      (list phase-name days-to next-name moon-icon))))

(defun my/celestial--planetary-hour ()
  "Return (PLANET HOUR-NUM) for current planetary hour."
  (require 'solar)
  (let* ((now (decode-time))
         (month (nth 4 now))
         (day (nth 3 now))
         (year (nth 5 now))
         (dow (nth 6 now))
         (current-minutes (+ (* (nth 2 now) 60) (nth 1 now)))
         (date (list month day year))
         (solar-data (ignore-errors (solar-sunrise-sunset date)))
         (sunrise-time (when solar-data (car (car solar-data))))
         (sunset-time (when solar-data (car (cadr solar-data)))))
    (if (and sunrise-time sunset-time)
        (let* ((sunrise-min (round (* sunrise-time 60)))
               (sunset-min (round (* sunset-time 60)))
               (day-length (- sunset-min sunrise-min))
               (night-length (- 1440 day-length))
               (day-hour-len (/ (float day-length) 12))
               (night-hour-len (/ (float night-length) 12))
               ;; Starting planet index in Chaldean sequence for each day
               ;; Sun=0 Mon=1 Tue=2 etc -> Chaldean start indices
               (day-starts '(3 6 2 5 1 4 0)) ;; Sun Mon Tue Wed Thu Fri Sat
               (start-idx (nth dow day-starts))
               (is-day (and (>= current-minutes sunrise-min)
                            (< current-minutes sunset-min)))
               (hour-num (if is-day
                             (floor (/ (- current-minutes sunrise-min) day-hour-len))
                           (if (>= current-minutes sunset-min)
                               (+ 12 (floor (/ (- current-minutes sunset-min) night-hour-len)))
                             (+ 12 (floor (/ (+ (- 1440 sunset-min) current-minutes)
                                             night-hour-len))))))
               (planet-idx (mod (+ start-idx hour-num) 7))
               (planet (nth planet-idx my/planetary-hour-sequence)))
          (list planet (1+ (mod hour-num 12)) is-day))
      (list "unknown" 0 t))))

(defun my/celestial--sunrise-sunset ()
  "Return (SUNRISE SUNSET) as formatted strings."
  (require 'solar)
  (let* ((now (decode-time))
         (date (list (nth 4 now) (nth 3 now) (nth 5 now)))
         (solar-data (ignore-errors (solar-sunrise-sunset date)))
         (sunrise (when solar-data (car (car solar-data))))
         (sunset (when solar-data (car (cadr solar-data)))))
    (if (and sunrise sunset)
        (list (format "%d:%02d" (floor sunrise) (round (* (mod sunrise 1) 60)))
              (format "%d:%02d" (floor sunset) (round (* (mod sunset 1) 60))))
      (list "?" "?"))))

;; ============================================================
;; Main dashboard
;; ============================================================

(defvar my/moon-sigils
  '((0 . "●")    ; new moon
    (1 . "◑")    ; first quarter
    (2 . "○")    ; full moon
    (3 . "◐"))   ; last quarter
  "Moon phase sigils.")

(defvar my/zodiac-glyphs
  '(("Aries" . "♈") ("Taurus" . "♉") ("Gemini" . "♊") ("Cancer" . "♋")
    ("Leo" . "♌") ("Virgo" . "♍") ("Libra" . "♎") ("Scorpio" . "♏")
    ("Sagittarius" . "♐") ("Capricorn" . "♑") ("Aquarius" . "♒") ("Pisces" . "♓"))
  "Unicode zodiac glyphs.")

(defvar my/planet-glyphs
  '(("Sun" . "☉") ("Moon" . "☽") ("Mars" . "♂") ("Mercury" . "☿")
    ("Jupiter" . "♃") ("Venus" . "♀") ("Saturn" . "♄"))
  "Unicode planet glyphs.")

(defvar my/discordian-seasons
  '("Chaos" "Discord" "Confusion" "Bureaucracy" "The Aftermath")
  "Discordian season names.")

(defvar my/discordian-days
  '("Sweetmorn" "Boomtime" "Pungenday" "Prickle-Prickle" "Setting Orange")
  "Discordian day names.")

(defun my/discordian-date ()
  "Return the current Discordian date as a string."
  (let* ((now (decode-time))
         (day (nth 3 now))
         (month (nth 4 now))
         (year (nth 5 now))
         (yday (1- (time-to-day-in-year (encode-time now))))
         (dyear (+ year 1166))
         (leap (date-leap-year-p year)))
    ;; St. Tib's Day
    (if (and leap (= month 2) (= day 29))
        (format "St. Tib's Day, YOLD %d" dyear)
      (let* ((adj-yday (if (and leap (> yday 59)) (1- yday) yday))
             (season (/ adj-yday 73))
             (sday (1+ (mod adj-yday 73)))
             (wday (mod adj-yday 5)))
        (format "%s, %s %d, %s YOLD %d"
                (nth wday my/discordian-days)
                (nth season my/discordian-seasons)
                sday
                (cond ((= sday 5) "| Apostle: Mungday")
                      ((= sday 50) "| Holyday: Chaoflux")
                      (t ""))
                dyear)))))

(defun my/zodiac-glyph (name)
  "Get zodiac glyph for NAME."
  (or (cdr (assoc name my/zodiac-glyphs)) "?"))

(defun my/planet-glyph (name)
  "Get planet glyph for NAME."
  (or (cdr (assoc name my/planet-glyphs)) "?"))

(defun my/celestial--current-house ()
  "Return the current astrological house (1-12) based on time of day.
Uses the Placidus-like approximation: house 1 starts at sunrise."
  (require 'solar)
  (let* ((now (decode-time))
         (date (list (nth 4 now) (nth 3 now) (nth 5 now)))
         (current-minutes (+ (* (nth 2 now) 60) (nth 1 now)))
         (solar-data (ignore-errors (solar-sunrise-sunset date)))
         (sunrise (when solar-data (round (* (car (car solar-data)) 60))))
         (sunset (when solar-data (round (* (car (cadr solar-data)) 60)))))
    (if (and sunrise sunset)
        (let* ((day-len (- sunset sunrise))
               (night-len (- 1440 day-len))
               (day-house-len (/ (float day-len) 6))
               (night-house-len (/ (float night-len) 6)))
          (cond
           ((and (>= current-minutes sunrise) (< current-minutes sunset))
            (1+ (floor (/ (- current-minutes sunrise) day-house-len))))
           ((>= current-minutes sunset)
            (+ 7 (floor (/ (- current-minutes sunset) night-house-len))))
           (t
            (+ 7 (floor (/ (+ (- 1440 sunset) current-minutes) night-house-len))))))
      1)))

(defun my/dashboard--insert-occult-header ()
  "Insert compact occult header with celestial data."
  (let* ((moon-info (ignore-errors (my/celestial--moon-phase-info)))
         (phase-idx (if moon-info
                        (pcase (nth 0 moon-info)
                          ((pred (string-prefix-p "New")) 0)
                          ((pred (string-prefix-p "First")) 1)
                          ((pred (string-prefix-p "Full")) 2)
                          ((pred (string-prefix-p "Last")) 3)
                          (_ 0))
                      0))
         (moon-sigil (cdr (assq phase-idx my/moon-sigils)))
         (sun-sign (ignore-errors (my/celestial--sun-sign)))
         (p-hour (ignore-errors (my/celestial--planetary-hour)))
         (sun-times (ignore-errors (my/celestial--sunrise-sunset)))
         (dow (nth 6 (decode-time)))
         (day-planet (car (cdr (assq dow my/planetary-days))))
         (house (ignore-errors (my/celestial--current-house)))
         (ddate (ignore-errors (my/discordian-date)))
         (sun-glyph (my/zodiac-glyph (or sun-sign "Aries")))
         (day-glyph (my/planet-glyph (or day-planet "Sun")))
         (hour-glyph (when p-hour (my/planet-glyph (nth 0 p-hour)))))

    ;; Line 1: Main celestial status
    (insert "    ")
    (insert (propertize (format "%s %s in %s"
                                moon-sigil
                                (or (nth 0 moon-info) "Moon")
                                sun-sign)
                        'face 'my/dashboard-celestial))
    (when (nth 1 moon-info)
      (insert (propertize (format "  %dd to %s" (nth 1 moon-info) (nth 2 moon-info))
                          'face 'my/dashboard-celestial-dim)))
    (insert "\n")

    ;; Line 2: Planetary hours + house
    (insert "    ")
    (insert (propertize (format "%s %s  %s %s  House %s"
                                sun-glyph (or sun-sign "?")
                                day-glyph (or day-planet "?")
                                (if house (number-to-string house) "?"))
                        'face 'my/dashboard-celestial))
    (when p-hour
      (insert (propertize (format "  %s h%d %s"
                                  hour-glyph
                                  (nth 1 p-hour)
                                  (if (nth 2 p-hour) "day" "night"))
                          'face 'my/dashboard-celestial-dim)))
    (insert "\n")

    ;; Line 3: Sun times + Discordian date
    (insert "    ")
    (when sun-times
      (insert (propertize (format "rise %s  set %s" (nth 0 sun-times) (nth 1 sun-times))
                          'face 'my/dashboard-celestial-dim)))
    (when ddate
      (insert (propertize (format "  %s" ddate) 'face 'my/dashboard-celestial-dim)))
    (insert "\n")))

(defun my/dashboard--render ()
  "Render the dashboard content."
  (let ((inhibit-read-only t))
    (erase-buffer)

    (insert "\n")

    ;; Occult celestial header
    (my/dashboard--insert-occult-header)
    (insert "\n")

    ;; Init stats
    (insert "    ")
    (insert (propertize (format "%.2fs  %d packages  %s"
                                (float-time (time-subtract after-init-time before-init-time))
                                (length package-activated-list)
                                (format "Emacs %s" emacs-version))
                        'face 'my/dashboard-dim))
    (insert "\n\n")

    ;; Sections
    (dolist (item my/dashboard-items)
      (let ((section (car item))
            (count (cdr item)))
        (insert "    ")
        (insert (my/dashboard--section-icon section))
        (insert " ")
        (insert (propertize (capitalize (symbol-name section)) 'face 'my/dashboard-heading))
        (insert "\n\n")
        (pcase section
          ('recents (my/dashboard--insert-recents count))
          ('projects (my/dashboard--insert-projects count))
          ('agenda (my/dashboard--insert-agenda count))
          ('bookmarks (my/dashboard--insert-bookmarks count)))
        (insert "\n")))

    (my/dashboard--insert-separator)
    (insert "\n")

    ;; Quick actions in two lines
    (insert "    ")
    (insert (propertize "[f]" 'face 'my/dashboard-shortcut))
    (insert (propertize " find  " 'face 'my/dashboard-dim))
    (insert (propertize "[p]" 'face 'my/dashboard-shortcut))
    (insert (propertize " project  " 'face 'my/dashboard-dim))
    (insert (propertize "[r]" 'face 'my/dashboard-shortcut))
    (insert (propertize " recent  " 'face 'my/dashboard-dim))
    (insert (propertize "[a]" 'face 'my/dashboard-shortcut))
    (insert (propertize " agenda  " 'face 'my/dashboard-dim))
    (insert (propertize "[m]" 'face 'my/dashboard-shortcut))
    (insert (propertize " magit" 'face 'my/dashboard-dim))
    (insert "\n")

    (insert "    ")
    (insert (propertize "[t]" 'face 'my/dashboard-shortcut))
    (insert (propertize " term  " 'face 'my/dashboard-dim))
    (insert (propertize "[c]" 'face 'my/dashboard-shortcut))
    (insert (propertize " capture  " 'face 'my/dashboard-dim))
    (insert (propertize "[s]" 'face 'my/dashboard-shortcut))
    (insert (propertize " config  " 'face 'my/dashboard-dim))
    (insert (propertize "[g]" 'face 'my/dashboard-shortcut))
    (insert (propertize " refresh  " 'face 'my/dashboard-dim))
    (insert (propertize "[q]" 'face 'my/dashboard-shortcut))
    (insert (propertize " quit" 'face 'my/dashboard-dim))
    (insert "\n")

    ;; Move to first button
    (goto-char (point-min))
    (ignore-errors (forward-button 1 nil nil t))))

(defun my/dashboard--setup-keys ()
  "Set up dashboard keybindings."
  (local-set-key (kbd "f") #'find-file)
  (local-set-key (kbd "r") #'consult-recent-file)

  (local-set-key (kbd "p") (lambda () (interactive)
                             (if (fboundp 'projectile-switch-project)
                                 (projectile-switch-project)
                               (project-switch-project))))
  (local-set-key (kbd "n") (lambda () (interactive)
                             (if (fboundp 'projectile-create-project)
                                 (call-interactively 'projectile-create-project)
                               (message "projectile-create-project not available"))))

  (local-set-key (kbd "a") #'org-agenda)
  (local-set-key (kbd "c") #'org-capture)
  (local-set-key (kbd "t") #'eshell)
  (local-set-key (kbd "m") #'magit-status)

  (local-set-key (kbd "s") (lambda () (interactive)
                             (find-file (expand-file-name "~/.emacs.d/init.el"))))
  (local-set-key (kbd "g") #'my/dashboard-refresh)
  (local-set-key (kbd "q") #'save-buffers-kill-terminal)

  ;; Navigation
  (local-set-key (kbd "j") #'forward-button)
  (local-set-key (kbd "k") #'backward-button)
  (local-set-key (kbd "RET") #'push-button)
  (local-set-key (kbd "TAB") #'forward-button)
  (local-set-key (kbd "<backtab>") #'backward-button)
  (local-set-key (kbd "h") #'backward-char)
  (local-set-key (kbd "l") #'forward-char))

(define-derived-mode my/dashboard-mode special-mode "Dashboard"
  "Major mode for the dashboard."
  (setq buffer-read-only t
        cursor-type nil
        truncate-lines t)
  (my/dashboard--setup-keys)
  (when (fboundp 'meow-motion-mode) (meow-motion-mode 1)))

(defun my/dashboard-refresh ()
  "Refresh the dashboard."
  (interactive)
  (when (get-buffer my/dashboard-buffer-name)
    (with-current-buffer my/dashboard-buffer-name
      (my/dashboard--render))))

(defun my/create-dashboard ()
  "Create and return the dashboard buffer."
  (let ((buffer (get-buffer-create my/dashboard-buffer-name)))
    (with-current-buffer buffer
      (my/dashboard-mode)
      (my/dashboard--render))
    buffer))

(defun my/show-dashboard ()
  "Show the dashboard."
  (interactive)
  (switch-to-buffer (my/create-dashboard)))

;; ============================================================
;; Startup integration
;; ============================================================

(defvar my/dashboard-shown nil
  "Track if dashboard has been shown.")

(defun my/dashboard-init ()
  "Initialize dashboard on startup."
  (when (and (not my/dashboard-shown)
             (< (length command-line-args) 2))
    (setq my/dashboard-shown t)
    (my/show-dashboard)))

(add-hook 'emacs-startup-hook #'my/dashboard-init)

(global-set-key (kbd "C-c d") #'my/show-dashboard)

(provide 'init.dashboard)
