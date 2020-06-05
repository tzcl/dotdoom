;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-theme 'doom-monokai-pro
      display-line-numbers-type t

      org-directory "~/gdrive/org/"

      mode-line-default-help-echo nil
      show-help-function nil)

(use-package! focus
  :config
  ;; Initialise focus-mode to be off
  (defvar focus-mode nil)
;; Make focus mode work with paragraphs
  (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph))))

;; Define paragraphs in text-mode to include lists
(defun toby/text-mode-hook ()
  (setq paragraph-start "\f\\|[ \t]*$")
  (setq paragraph-separate "^[ \t\f]*$"))
(add-hook! 'text-mode-hook 'toby/text-mode-hook)

;; Enable visual-line-mode in programming modes
(add-hook! 'prog-mode-hook 'turn-on-visual-line-mode)

;; Enable visual-fill-column in text modes
(require 'visual-fill-column)
(add-hook! 'text-mode-hook 'turn-on-visual-fill-column-mode)

;;
;;; UI

(setq doom-font (font-spec :family "monospace" :size 14))

;;
;;; Keybinds

(map! "C-'" 'better-comment-dwim
      :mnv "g D" 'xref-find-definitions-other-window
      :mnv "$" 'evil-end-of-line
      :mnv "g $" 'evil-end-of-visual-line
      :v "DEL" 'evil-delete-char)

(map! :leader
      "f i" 'toby/find-index

      :mnv "p O" 'projectile-find-other-file-other-window)

(map! :map org-mode-map
      :mnv "SPC m h" 'toby/org-toggle-headings
      :ei "M-SPC m h" 'toby/org-toggle-headings)

(map! :leader (:prefix ("j" . "journal") ;; org-journal bindings
        :desc "Create new journal entry" "j" #'org-journal-new-entry
        :desc "Open previous entry" "p" #'org-journal-open-previous-entry
        :desc "Open next entry" "n" #'org-journal-open-next-entry
        :desc "Search journal" "s" #'org-journal-search-forever))

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

;; Set transparency in Emacs so writeroom can restore it
(add-to-list 'default-frame-alist '(alpha 95 95))

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
        org-ellipsis " ▼ "
        org-hide-emphasis-markers t
        org-pretty-entities t

        org-journal-file-format "%Y-%m-%d"
        org-journal-file-type 'monthly
        org-journal-dir "~/gdrive/org/journal/"
        org-journal-file-header "#+TITLE: %B %Y\n\n"

        org-file-apps (butlast org-file-apps)
        org-file-apps (append org-file-apps '(("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")
                                              ("\\.png\\'" . "sxiv %s")
                                              ("\\.jpg\\'" . "sxiv %s")
                                              ("\\.jpeg\\'" . "sxiv %s")
                                              ("\\.gif\\'" . "sxiv %s"))))

  (setq org-capture-templates '(("i" "Inbox" entry (file "~/gdrive/org/inbox.org") "* TODO %?" :empty-lines 1)
                               ("l" "Link" entry (file "~/gdrive/org/inbox.org") "* TODO %(org-cliplink-capture)" :immediate-finish t :empty-lines 1)))

  (defun +org-update-latex-preview-background-color (&rest _)
    (setq-default
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))))
  (add-hook 'solaire-mode-hook #'+org-update-latex-preview-background-color)
  )
;; Turn leading stars into spaces
(after! org-superstar
  (setq org-superstar-leading-bullet ?\s))

;; Make org-toggle-headings nicer
(defun toby/org-toggle-headings ()
  (interactive)
  (org-toggle-heading (org-current-level)))

(defun toby/create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                                  'default)
                                              :background nil t))
            props)))
(advice-add 'create-image :filter-args #'toby/create-image-with-background-color)

(defun toby/clean-org-latex-cache ()
  (interactive)
  (shell-command "rm ~/.emacs.d/.local/cache/org-latex/*"))

;; Shortcut to insert images
;; TODO: see how org-download works, replace this with that?
(defvar img-d "~/gdrive/misc/img")
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
