;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-theme 'doom-monokai-pro
      display-line-numbers-type t

      org-directory "~/mega/org/"

      mode-line-default-help-echo nil
      show-help-function nil)

(use-package! focus
  :config
  ;; Initialise focus-mode to be off
  (defvar focus-mode nil)
;; Make focus mode work with paragraphs
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph))))

;;
;;; UI

(setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14))
(setq doom-variable-pitch-font (font-spec :family "ETBembo" :size 16))

;;
;;; Keybinds

(map! "C-'" 'better-comment-dwim
      :mnv "g D" 'xref-find-definitions-other-window
      :mnv "$" 'evil-end-of-line
      :mnv "g $" 'evil-end-of-visual-line
      :v "DEL" 'evil-delete-char)

(map! :leader
      "t w" (lambda () (interactive) (visual-fill-column-mode 'toggle))

      :mnv "p O" 'projectile-find-other-file-other-window)

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
;;; Modules

;; Sort filenames better
;; This isn't getting called
(after! ivy
  (add-to-list
   'ivy-sort-functions-alist
   '(read-file-name-internal . toby/sort-filenames)))

;; Define paragraphs in text-mode to include lists
(defun toby/text-mode-hook ()
  (setq paragraph-start "\f\\|[ \t]*$")
  (setq paragraph-separate "^[ \t\f]*$"))
(add-hook! 'text-mode-hook 'toby/text-mode-hook)

;; Enable visual-line-mode in programming modes
(add-hook! 'prog-mode-hook 'turn-on-visual-line-mode)

;; Enable visual-fill-column in text modes
(setq visual-fill-column-width 90)
(require 'visual-fill-column)
(add-hook! 'text-mode-hook 'turn-on-visual-line-mode 'turn-on-visual-fill-column-mode)

;;; zen-mode
;; Clean up zen-mode
(setq writeroom-windows nil)
(setq need-to-restore? nil)
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
(add-hook 'writeroom-mode-hook #'toby/writeroom-mode-hook)

;; Fix up mixed-pitch
(setq mixed-pitch-variable-pitch-cursor nil)
(setq mixed-pitch-set-height 144)
(after! mixed-pitch
  (add-to-list 'mixed-pitch-fixed-pitch-faces 'org-ellipsis)
  ;; Faces to stop being mixed pitch
  (cl-delete-if (lambda (x) (memq x '(font-lock-comment-face))) mixed-pitch-fixed-pitch-faces))

;;; projectile
(when (string-match "-[Mm]icrosoft" operating-system-release)
  (setq projectile-indexing-method 'native))

;;; lookup
;; Add cppreference to +lookup/online
(add-to-list '+lookup-provider-url-alist '("C++ Reference" "https://en.cppreference.com/mwiki/index.php?search=%s"))

;;; windows
;; Switch to new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;;; org
(after! org
  (setq org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-superstar-leading-bullet ?\s

        org-ellipsis " ▼ "
        org-hide-emphasis-markers t

        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-dir "~/mega/org/journal/"
        org-journal-file-header "#+TITLE: %B %Y\n#+STARTUP: overview\n\n")

  ;; HACK: solaire org has the wrong colour at start up (but is fine after swapping themes)
  (set-face-attribute 'solaire-org-hide-face nil :foreground "#2D2A2E")

  ;; Increase the number of lines that can be fontified
  (setcar (nthcdr 4 org-emphasis-regexp-components) 10)

  (setq org-capture-templates '(("i" "Inbox" entry (file "~/mega/org/inbox.org") "* TODO %?" :empty-lines 1)
                                ("l" "Link" entry (file "~/mega/org/inbox.org") "* TODO %(org-cliplink-capture)" :immediate-finish t :empty-lines 1)))

  (add-hook! 'org-mode-hook
    (face-remap-add-relative 'solaire-default-face :inherit 'variable-pitch)
    (writeroom-mode))

(defun toby/fix-org-latex-preview-background-colour (&rest _)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute 'solaire-default-face :background nil t))))
(add-hook 'solaire-mode-hook #'toby/fix-org-latex-preview-background-colour)

(defun toby/fix-image-background-color (args)
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    (append (list file type data-p)
            (list :background (face-attribute 'solaire-default-face :background nil t))
            props)))
(advice-add 'create-image :filter-args #'toby/fix-image-with-background-color))

;; Make org-toggle-headings nicer
(defun toby/org-toggle-headings ()
  (interactive)
  (org-toggle-heading (org-current-level)))

(defun toby/clean-org-latex-cache ()
  (interactive)
  (shell-command "rm ~/.emacs.d/.local/cache/org-latex/*"))

;; Allow pdf-tools to create images
(after! pdf-view
  (advice-remove 'create-image #'toby/fix-image-with-background-color))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2D2A2E" "#CC6666" "#A9DC76" "#FFD866" "#78DCE8" "#FF6188" "#78DCE8" "#FCFCFA"])
 '(custom-safe-themes
   '("425cf02839fa7c5ebd6cb11f8074f6b8463ae6ed3eeb4cf5a2b18ffc33383b0b" default))
 '(fci-rule-color "#4C4A4D")
 '(jdee-db-active-breakpoint-face-colors (cons "#19181A" "#FCFCFA"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#19181A" "#A9DC76"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#19181A" "#727072"))
 '(objed-cursor-color "#CC6666")
 '(pdf-view-midnight-colors (cons "#FCFCFA" "#2D2A2E"))
 '(rustic-ansi-faces
   ["#2D2A2E" "#CC6666" "#A9DC76" "#FFD866" "#78DCE8" "#FF6188" "#78DCE8" "#FCFCFA"])
 '(vc-annotate-background "#2D2A2E")
 '(vc-annotate-color-map
   (list
    (cons 20 "#A9DC76")
    (cons 40 "#c5da70")
    (cons 60 "#e2d96b")
    (cons 80 "#FFD866")
    (cons 100 "#fec266")
    (cons 120 "#fdad66")
    (cons 140 "#FC9867")
    (cons 160 "#fd8572")
    (cons 180 "#fe737d")
    (cons 200 "#FF6188")
    (cons 220 "#ee627c")
    (cons 240 "#dd6471")
    (cons 260 "#CC6666")
    (cons 280 "#b56869")
    (cons 300 "#9f6b6c")
    (cons 320 "#886d6f")
    (cons 340 "#4C4A4D")
    (cons 360 "#4C4A4D")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
