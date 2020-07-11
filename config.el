;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-theme 'doom-monokai-pro
      display-line-numbers-type t

      org-directory "~/mega/org/"
      calendar-week-start-day 1

      mode-line-default-help-echo nil
      show-help-function nil)

;;
;;; UI

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
      doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))

;; Make fonts bigger on Windows
(when (string-match "-[Mm]icrosoft" operating-system-release)
  (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 20)
        doom-variable-pitch-font (font-spec :family "ETBembo" :size 24)))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;;
;;; Keybinds

;; global
(map! "C-'" 'better-comment-dwim
      :mnv "g D" 'xref-find-definitions-other-window
      :mnv "$" 'evil-end-of-line
      :mnv "g $" 'evil-end-of-visual-line
      :v "DEL" 'evil-delete-char)

;; after SPC
(map! :leader
      "t w" (lambda () (interactive) (visual-fill-column-mode 'toggle))

      :mnv "p O" 'projectile-find-other-file-other-window)

;; in org-mode
(map! :map org-mode-map
      :mnv "SPC m h" 'toby/org-toggle-headings
      :ei "M-SPC m h" 'toby/org-toggle-headings)

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
      :n "j" #'org-journal-search-next
      :n "k" #'org-journal-search-prev
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

(after! org
  (setq org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-ellipsis " ▼ "
        org-hide-emphasis-markers t)

  (setq org-capture-templates '(("i" "Inbox" entry (file "~/mega/org/inbox.org") "* TODO %?" :empty-lines 1)
                                ("l" "Link" entry (file "~/mega/org/inbox.org") "* TODO %(org-cliplink-capture)" :immediate-finish t :empty-lines 1)))

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
             (offset (floor (- (/ 160 2) (/ width 2)))))
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

(defun toby/clean-org-latex-cache ()
  (interactive)
  (shell-command "rm ~/.emacs.d/.local/cache/org-latex/*")))

(after! org-superstar
  (setq org-superstar-leading-bullet ?\s))

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

  (defun toby/get-org-gcal-credentials ()
    (setq org-gcal-client-id (password-store-get "org-gcal/id")
          org-gcal-client-secret (password-store-get "org-gcal/secret")))

  (advice-add 'org-gcal-request-token :before 'toby/get-org-gcal-credentials))

(after! (:and solaire-mode org)
  ;; HACK: solaire org has the wrong colour at start up (but is fine after swapping themes)
  ;;(set-face-attribute 'solaire-org-hide-face nil :foreground "#2D2A2E")
  (add-hook! 'org-mode-hook
    (face-remap-add-relative 'solaire-default-face :inherit 'variable-pitch)
    (writeroom-mode))

  (defun toby/fix-org-latex-preview-background-colour (&rest _)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute 'solaire-default-face :background nil t))))

  (add-hook 'solaire-mode-hook #'toby/fix-org-latex-preview-background-colour))

;; Shortcut to insert images
;; TODO: see how org-download works, replace this with that?
(defvar img-d "~/mega/misc/img")
(defun toby/img-complete-link ()
  "Create an image link using completion."
  (interactive)
  (org-insert-link nil (concat "file:" (abbreviate-file-name (expand-file-name (read-file-name "Image: " img-d)))) nil))

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
