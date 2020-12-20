;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-theme 'doom-monokai-pro
      display-line-numbers-type t

      org-directory "~/mega/org/"
      org-agenda-dir "~/mega/org/agenda/"
      calendar-week-start-day 1

      mode-line-default-help-echo nil
      show-help-function nil)

;;
;;; UI

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))

;; Make fonts bigger on laptop
(when (string-match "Toby-XPS" (system-name))
  (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 20)
        doom-variable-pitch-font (font-spec :family "ETBembo" :size 24)))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;
;;; Keybindings

;; global
(map! "C-'" #'better-comment-dwim
      :nv "g D" #'xref-find-definitions-other-window
      :mnv "$" #'evil-end-of-line
      :mnv "g $" #'evil-end-of-visual-line
      :v "DEL" #'evil-delete-char
      "<f8>" #'toby/toggle-org-agenda)

;; after SPC
(map! :leader
      :nv "t w" (lambda () (interactive) (visual-fill-column-mode 'toggle))
      :nv "w x" (lambda () (interactive) (save-buffer) (doom-kill-buffer-and-windows (current-buffer)))
      :nv "p O" #'projectile-find-other-file-other-window)

;; in org-mode
(map! :map org-mode-map
      :nv "SPC m h" #'toby/org-toggle-headings
      :ei "M-SPC m h" #'toby/org-toggle-headings)

;; The built-in calendar mode and org-journal-search mappings conflict with evil bindings
(map! :map calendar-mode-map
      :n "o" #'org-journal-display-entry
      :n "p" #'org-journal-previous-entry
      :n "n" #'org-journal-next-entry
      :n "O" #'org-journal-new-date-entry)
(map! :map calendar-mode-map :localleader
      "w" #'org-journal-search-calendar-week
      "m" #'org-journal-search-calendar-month
      "y" #'org-journal-search-calendar-year)
(map! :map org-journal-search-mode-map
      :n "j" #'org-journal--search-next
      :n "k" #'org-journal--search-prev
      :n "q" #'kill-this-buffer)

;;
;;; Packages

;; Define paragraphs in text-mode to include lists
(defun toby/text-mode-hook ()
  (setq paragraph-start "\f\\|[ \t]*$")
  (setq paragraph-separate "^[ \t\f]*$"))

(add-hook 'text-mode-hook #'toby/text-mode-hook)

;; Enable visual-line-mode in programming modes
(add-hook 'prog-mode-hook #'turn-on-visual-line-mode)

;; Enable visual-fill-column in text mode
(use-package! visual-fill-column
  :hook (text-mode . visual-fill-column-mode)
  :config
  (setq visual-fill-column-width 90)
  (turn-on-visual-line-mode))

;; Sort filenames better
(after! ivy
  (add-to-list
   'ivy-sort-functions-alist
   '(read-file-name-internal . toby/sort-filenames)))

;; Set up focus
(use-package! focus
  :config
  ;; Initialise focus-mode to be off
  (defvar focus-mode nil)
  ;; Make focus mode work with paragraphs
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph))))

;; Make writeroom-mode expand to take up the whole window
(after! writeroom-mode
  (setq writeroom-windows nil
        need-to-restore? nil)

  (defun toby/writeroom-mode-hook ()
    (display-line-numbers-mode 'toggle)
    (hl-line-mode 'toggle)
    (company-mode 'toggle)
    (focus-mode 'toggle)
    (if writeroom-mode
        (when (not (one-window-p))
          (setq need-to-restore? 't)
          (setq writeroom-windows (current-window-configuration))
          (delete-other-windows))
      (when need-to-restore?
        (set-window-configuration writeroom-windows)
        (setq need-to-restore? nil))))

  (add-hook 'writeroom-mode-hook #'toby/writeroom-mode-hook))

(after! mixed-pitch
  ;; Mixed-pitch cursor doesn't work with evil
  (setq mixed-pitch-variable-pitch-cursor nil
        mixed-pitch-set-height 144)

  ;; Make org-ellipsis the right size
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-ellipsis)
  ;; Faces to stop being mixed pitch
  (cl-delete-if (lambda (x) (memq x '(font-lock-comment-face))) mixed-pitch-fixed-pitch-faces))

(after! projectile
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    (setq projectile-indexing-method 'native)))

(after! lookup
  ;; Add cppreference to +lookup/online
  (add-to-list '+lookup-provider-url-alist '("C++ Reference" "https://en.cppreference.com/mwiki/index.php?search=%s")))

(after! magit
  (keychain-refresh-environment))

(after! org
  (setq org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-catch-invisible-edits 't
        org-ellipsis " ▼ "
        org-hide-emphasis-markers 't
        org-log-into-drawer 't)

  (setq org-agenda-files (cons (concat org-directory "calendar.org") (directory-files org-agenda-dir t "\\.org$")))

  (setq org-todo-keyword-faces '(("[-]"  . +org-todo-project)
                                 ("STRT" . +org-todo-project)
                                 ("[?]"  . +org-todo-onhold)
                                 ("WAIT" . +org-todo-onhold)
                                 ("HOLD" . +org-todo-onhold)
                                 ("PROJ" . +org-todo-active)))

  (setq org-tag-alist '(("@megasorber" . ?m)
                        ("@errand" . ?e)
                        ("@home" . ?h)
                        ))

  (setq org-refile-allow-creating-parent-nodes 't
        org-refile-targets '(("next.org" :level . 0)
                             ("someday.org" :level . 1)
                             ("reading.org" :level . 2)
                             ("writing.org" :level . 1)
                             ("projects.org" :maxlevel . 1)
                             ("megasorber.org" :level . 0)
                             ("uni.org" :level . 0)))

  (setq org-inbox-file "~/mega/org/agenda/inbox.org")

  (setq org-capture-templates `(("i" "Inbox" entry (file ,org-inbox-file) "* TODO %?")
                                ("l" "Link" entry (file ,org-inbox-file) "* TODO %(org-cliplink-capture)" :immediate-finish t)
                                ("r" "Read" entry (file ,org-inbox-file) "* TODO %^{Title}\nAuthor: %^{Author}\n%?")
                                ("p" "Project" entry (file ,(concat org-agenda-dir "projects.org")) "* PROJ %?")
                                ("w" "Weekly review" entry (file+olp+datetree ,(concat org-agenda-dir "progress.org"))
                                 (file ,(concat org-directory "templates/weekly_review.org")))))

  (add-hook! 'org-capture-after-finalize-hook (org-agenda-maybe-redo))

  ;; Increase the number of lines that can be fontified
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)

  ;; Org latex previews
  (when (string-match "-[Mm]icrosoft" operating-system-release)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75)))

  (plist-put org-format-latex-options :justify 'center)

  (defun org-justify-fragment-overlay (beg end image imagetype)
    "Adjust the justification of a LaTeX fragment.
The justification is set by :justify in
`org-format-latex-options'. Only equations at the beginning of a
line are justified."
    (require 'ov)
    (cond
     ;; Centered justification
     ((and (eq 'center (plist-get org-format-latex-options :justify))
           (= beg (line-beginning-position)))
      (let* ((img (create-image image (intern imagetype)))
             (width (car (image-size img)))
             (offset (floor (- (/ 140 2) (/ width 2)))))
        (overlay-put (ov-at) 'before-string (make-string offset ?\s))))
     ;; Right justification
     ((and (eq 'right (plist-get org-format-latex-options :justify))
           (= beg (line-beginning-position)))
      (let* ((width (car (image-display-size (overlay-get (ov-at) 'display))))
             (offset (floor (- (window-text-width) width (- (line-end-position) end)))))
        (overlay-put (ov-at) 'before-string (make-string offset ?\s))))))

  (advice-add 'org--make-preview-overlay :after 'org-justify-fragment-overlay)

;; Make org-toggle-headings nicer
(defun toby/org-toggle-headings ()
  (interactive)
  (org-toggle-heading (org-current-level)))

(defun toby/archive-done-tasks ()
    "Archive all done tasks."
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
     "/DONE" 'agenda))

(defun toby/clean-org-latex-cache ()
  (interactive)
  (shell-command "rm ~/.emacs.d/.local/cache/org-latex/*")))

(after! org-superstar
  (setq org-superstar-leading-bullet ?\s))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⚑" "⚑" "⚑")))

(after! org-journal
  (setq org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-dir "~/mega/org/journal/"
        org-journal-file-header "#+TITLE: %B %Y\n#+STARTUP: overview\n\n"))

(use-package! org-gcal
  :config
  (setq org-gcal-file-alist '(("michlaw23@gmail.com" . "~/mega/org/calendar.org")))

  (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
  (add-hook 'org-capture-after-finalize-hook 'org-gcal-fetch)

  (defun org-gcal--notify (title message)
    (ignore message)
    (message (concat "Org-gcal: " (downcase title))))

  (defun toby/get-org-gcal-credentials ()
    (setq org-gcal-client-id (password-store-get "org-gcal/id")
          org-gcal-client-secret (password-store-get "org-gcal/secret"))
    (advice-remove 'org-gcal-request-token 'toby/get-org-gcal-credentials)
    (advice-remove 'org-gcal--refresh-token 'toby/get-org-gcal-credentials))

  (advice-add 'org-gcal-request-token :before 'toby/get-org-gcal-credentials)
  (advice-add 'org-gcal--refresh-token :before 'toby/get-org-gcal-credentials))

(after! org-agenda
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode 't

        org-clocktable-defaults (plist-put org-clocktable-defaults :fileskip0 't)

        org-agenda-restore-windows-after-quit 't

        org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

  (setq org-agenda-custom-commands `((" " "Agenda"
                                      ((agenda "" ((org-agenda-span 'day)
                                                   (org-agenda-start-day nil)
                                                   (org-deadline-warning-days 14)))
                                      (todo "TODO" ((org-agenda-overriding-header "To refile")
                                                    (org-agenda-files '(,(concat org-agenda-dir "inbox.org")))))
                                      (todo "STRT" ((org-agenda-overriding-header "In progress")
                                                    (org-agenda-files '(,(concat org-agenda-dir "someday.org")
                                                                        ,(concat org-agenda-dir "reading.org")
                                                                        ,(concat org-agenda-dir "writing.org")
                                                                        ,(concat org-agenda-dir "uni.org")
                                                                        ,(concat org-agenda-dir "megasorber.org")
                                                                        ,(concat org-agenda-dir "projects.org")
                                                                        ,(concat org-agenda-dir "next.org")))))
                                      (todo "PROJ" ((org-agenda-overriding-header "Projects")
                                                    (org-agenda-files '(,(concat org-agenda-dir "uni.org")))
                                                    (org-agenda-files '(,(concat org-agenda-dir "megasorber.org")))
                                                    (org-agenda-files '(,(concat org-agenda-dir "projects.org")))
                                                    ))
                                      (todo "TODO" ((org-agenda-overriding-header "Tasks")
                                                    (org-agenda-files '(,(concat org-agenda-dir "uni.org")
                                                                        ,(concat org-agenda-dir "megasorber.org")
                                                                        ,(concat org-agenda-dir "next.org")))
                                                    (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                                      ))))

  (defun toby/process-inbox ()
    (interactive)
    (org-agenda-bulk-mark-regexp "inbox:")
    (toby/bulk-process-entries))

  (defun toby/bulk-process-entries ()
    (if (not (null org-agenda-bulk-marked-entries))
        (let ((entries (reverse org-agenda-bulk-marked-entries))
              (processed 0)
              (skipped 0))
          (dolist (e entries)
            (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
              (if (not pos)
                  (progn (message "Skipping removed entry at %s" e)
                         (cl-incf skipped))
                (goto-char pos)
                (let (org-loop-over-headlines-in-active-region) (funcall 'toby/process-item))
                ;; `post-command-hook' is not run yet.  We make sure any
                ;; pending log note is processed.
                (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                          (memq 'org-add-log-note post-command-hook))
                  (org-add-log-note))
                (cl-incf processed))))
          (org-agenda-redo)
          (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
          (message "Acted on %d entries%s%s"
                   processed
                   (if (= skipped 0)
                       ""
                     (format ", skipped %d (disappeared before their turn)"
                             skipped))
                   (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

  (defun toby/process-item ()
    (interactive)
    (org-with-wide-buffer
     (org-agenda-set-tags)
     (org-agenda-priority)
     (call-interactively 'toby/org-set-effort)
     (org-agenda-refile nil nil t)))

  (setq org-agenda-bulk-custom-functions '((?j toby/process-item)))

  (defvar toby/org-current-effort "1:00")

  (defun toby/org-set-effort (effort)
    (interactive
     (list (read-string (format "Effort [%s]: " toby/org-current-effort) nil nil toby/org-current-effort)))
    (setq toby/org-current-effort effort)
    (org-agenda-check-no-diary)
    (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                         (org-agenda-error)))
           (buffer (marker-buffer hdmarker))
           (pos (marker-position hdmarker))
           (inhibit-read-only t)
           newhead)
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (widen)
          (goto-char pos)
          (org-show-context 'agenda)
          (funcall-interactively 'org-set-effort nil toby/org-current-effort)
          (end-of-line 1)
          (setq newhead (org-get-heading)))
        (org-agenda-change-all-lines newhead hdmarker))))

  (add-hook! 'org-clock-in-hook :append (org-todo "STRT"))
  (advice-add 'org-agenda-exit :before 'org-save-all-org-buffers)
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers))

(defun toby/toggle-org-agenda ()
  (interactive)
  (require 'evil-org-agenda)
  (if evil-org-agenda-mode (org-agenda-quit)
    (org-agenda nil " ")))

(after! (:and solaire-mode org)
  (add-hook! 'org-mode-hook
    (face-remap-add-relative 'solaire-default-face :inherit 'variable-pitch)
    (display-line-numbers-mode)
    (hl-line-mode)
    (writeroom-mode))

  (defun toby/fix-org-latex-preview-background-colour (&rest _)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute 'solaire-default-face :background nil t))))

  (add-hook 'solaire-mode-hook #'toby/fix-org-latex-preview-background-colour))

;;; DWIM functions
;; Better commenting
(defun better-comment-dwim ()
  "Like 'comment-dwim', but toggle comment if cursor is not at end of line."
  (interactive)
  (if (region-active-p)
      (comment-dwim nil)
    (let (($lbp (line-beginning-position))
          ($lep (line-end-position)))
      (if (eq $lbp $lep)
          (progn
            (comment-dwim nil))
        (if (eq (point) $lep)
            (progn
              (comment-dwim nil))
          (progn
            (comment-or-uncomment-region $lbp $lep)
            (forward-line )))))))

;; Better filename sorting
(defun toby/sort-filenames (x y)
  "Compare two files. Prioritise directories. Returns true if x < y."
  (if (string-match "/$" x)
      (if (string-match "/$" y)
          (dict< x y)
        t)
    (if (string-match "/$" y)
        nil
      (dict< x y))))

(defun dict< (str1 str2)
  "Return t if STR1 is < STR2 when doing a dictionary compare
(splitting the string at numbers and doing numeric compare with them)"
  (let ((str1-components (dict-split str1))
        (str2-components (dict-split str2)))
    (dict-lessp str1-components str2-components)))

(defun dict-lessp (slist1 slist2)
  "Compare the two lists of strings & numbers"
  (cond ((null slist1)
         (not (null slist2)))
        ((null slist2)
         nil)
        ((and (numberp (car slist1))
              (stringp (car slist2)))
         t)
        ((and (numberp (car slist2))
              (stringp (car slist1)))
         nil)
        ((and (numberp (car slist1))
              (numberp (car slist2)))
         (or (< (car slist1) (car slist2))
             (and (= (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))
        (t
         (or (string-lessp (car slist1) (car slist2))
             (and (string-equal (car slist1) (car slist2))
                  (dict-lessp (cdr slist1) (cdr slist2)))))))

(defun dict-split (str)
  "Split a string into a list of number and non-number components"
  (save-match-data
    (let ((res nil))
      (while (and str (not (string-equal "" str)))
        (let ((p (string-match "[0-9]*\\.?[0-9]+" str)))
          (cond ((null p)
                 (setq res (cons str res))
                 (setq str nil))
                ((= p 0)
                 (setq res (cons (string-to-number (match-string 0 str)) res))
                 (setq str (substring str (match-end 0))))
                (t
                 (setq res (cons (substring str 0 (match-beginning 0)) res))
                 (setq str (substring str (match-beginning 0)))))))
      (reverse res))))

(defun toby/light-theme ()
  (interactive)
  (setq doom-theme 'doom-solarized-light)
  (doom/reload-theme)
  (set-face-attribute 'font-lock-comment-face nil :slant 'unspecified))

(defun toby/dark-theme ()
  (interactive)
  (setq doom-theme 'doom-monokai-pro)
  (doom/reload-theme))
