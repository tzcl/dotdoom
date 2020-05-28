;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; TODO

;; TODO: set up key bindings for LSP mode, learn to use more effectively
;; TODO: set up bookmarks to access common files/websites/folders quickly
;; TODO: set up org-roam (see workflows)

;;; Org
;; TODO: set up org-capture templates (writing ideas, video ideas, ??)
;; TODO: set up org agenda? lots of functionality but have other equivalent
;; tools, like using Todoist because it's cross-platform (mobile)
;; TODO: look into how org-download works (what's the best way to insert images into org-mode)

(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      doom-theme 'doom-monokai-pro
      display-line-numbers-type t

      org-directory "~/gdrive/sci/org"

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

;;
;;; Modules

;;; zen-mode
;; Clean up zen-mode
(setq writeroom-windows nil)
(defun store-writeroom-windows ()
  (setq writeroom-windows (current-window-configuration)))
(defun restore-writeroom-windows ()
  (set-window-configuration writeroom-windows))
;; TODO clean this up
(defun toby/writeroom-mode-hook ()
  (if writeroom-mode
      (progn
        (store-writeroom-windows)
        (delete-other-windows)
        (display-line-numbers-mode 'toggle)
        (hl-line-mode 'toggle)
        (company-mode 'toggle)
        (focus-mode 'toggle))
    (progn
      (restore-writeroom-windows)
      (display-line-numbers-mode 'toggle)
      (hl-line-mode 'toggle)
      (company-mode 'toggle)
      (focus-mode 'toggle))))
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
;; TODO fix up org preview latex
(after! org
  (setq org-hide-leading-stars nil
        org-indent-mode-turns-on-hiding-stars nil
        org-ellipsis " ▼ "

        org-journal-file-type 'weekly
        org-journal-dir "~/gdrive/sci/org/journal/"

        org-file-apps (butlast org-file-apps)
        org-file-apps (append org-file-apps '(("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")
                                              ("\\.png\\'" . "sxiv %s")
                                              ("\\.jpg\\'" . "sxiv %s")
                                              ("\\.jpeg\\'" . "sxiv %s")
                                              ("\\.gif\\'" . "sxiv %s")))))
;; Turn leading stars into spaces
(after! org-superstar
  (setq org-superstar-leading-bullet ?\s))

;; Make org-toggle-headings nicer
(defun toby/org-toggle-headings ()
  (interactive)
  (org-toggle-heading (org-current-level)))

;; Make image backgrounds match the background colour
(defun toby/create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
         (type (cadr args))
         (data-p (caddr args))
         (props (cdddr args)))
    ;; get this return result style from `create-image'
    (append (list file type data-p)
            (list :background (face-background 'default))
            props)))
(advice-add 'create-image :filter-args #'toby/create-image-with-background-color)

;; Automatically preview latex equations
(defun toby/auto-toggle-equation ()
  (when (looking-back (rx "$ "))
    (save-excursion
      (backward-char 1)
      (org-toggle-latex-fragment))))
(add-hook 'org-mode-hook (lambda () (add-hook 'post-self-insert-hook #'toby/auto-toggle-equation 'append 'local)))

;; Shortcut to insert images
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
