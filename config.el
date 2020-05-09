;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; TODO: set up key bindings for LSP mode, learn to use more effectively
;; TODO: set up org-capture templates (writing ideas, video ideas, ??)
;; TODO: set up org agenda? lots of functionality but have other equivalent
;; tools, like using Todoist because it's cross-platform (mobile)
;; TODO: set up bookmarks to access common files/websites/folders quickly
;; TODO: in focus mode, clicking causes hl-line-mode to reappear

;; Make focus mode work with paragraphs
(setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph)))

;; Define paragraphs in text-mode to include lists
(defun toby/text-mode-hook ()
  (setq paragraph-start "^\n")
  (setq paragraph-separate "\n\n"))
(add-hook 'text-mode-hook #'toby/text-mode-hook)

(defun toby/toggle-minor-mode (mode)
  (if (symbol-value mode) (funcall (symbol-function mode) 0) (funcall (symbol-function mode) 1)))

;; Make flyspell check buffer on turning it on
;; Disable by default
(require 'flyspell)
(remove-hook! '(org-mode-hook
                markdown-mode-hook
                TeX-mode-hook
                rst-mode-hook
                mu4e-compose-mode-hook
                message-mode-hook)
  #'flyspell-mode)
(defun toby/flyspell-mode ()
  (interactive)
  (toby/toggle-minor-mode 'flyspell-mode)
  (if flyspell-mode (flyspell-buffer)))

;; Disable line numbers in zen-mode
(require 'focus)
(defun toby/writeroom-mode-hook ()
  (toby/toggle-minor-mode 'display-line-numbers-mode)
  (toby/toggle-minor-mode 'hl-line-mode)
  (toby/toggle-minor-mode 'visual-line-mode)
  (toby/toggle-minor-mode 'company-mode)
  (toby/toggle-minor-mode 'focus-mode))
(add-hook 'writeroom-mode-hook #'toby/writeroom-mode-hook)

;; Make org-toggle-headings nicer
(require 'org)
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

  (setq org-file-apps (butlast org-file-apps))
  (setq org-file-apps (append org-file-apps '(("\\.pdf::\\([0-9]+\\)\\'" . "zathura -P %1 %s")
                                              ("\\.png\\'" . "sxiv %s")
                                              ("\\.jpg\\'" . "sxiv %s")
                                              ("\\.jpeg\\'" . "sxiv %s")
                                              ("\\.gif\\'" . "sxiv %s"))))

(defun toby/find-index ()
  (interactive)
  (find-file "/home/toby/gdrive/index.org"))
(add-to-list '+doom-dashboard-menu-sections '("Open index"
                                              :icon (all-the-icons-octicon "repo" :face 'doom-dashboard-menu-title)
                                              :action toby/find-index) t)

;; Add cppreference to +lookup/online
(add-to-list '+lookup-provider-url-alist '("C++ Reference" "https://en.cppreference.com/mwiki/index.php?search=%s"))

;; Define keys
(map! "C-'" 'better-comment-dwim
      "C-x n" 'narrow-or-widen-dwim

      :mnv "g D" 'xref-find-definitions-other-window

      :v "DEL" 'evil-delete-char)

(map! :leader
      "t s" 'toby/flyspell-mode
      "t o" 'org-tree-slide-mode
      "f i" 'toby/find-index)

(map! :map org-mode-map
      :mnv "SPC m h" 'toby/org-toggle-headings
      :ei "M-SPC m h" 'toby/org-toggle-headings
      :mnv "SPC m d" 'toby/img-complete-link
      :ei "M-SPC m d" 'toby/img-complete-link

      :mnv "SPC m D" 'org-deadline
      :ei "M-SPC m D" 'org-deadline)

;; DWIM functions
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

;; Better narrowing and widening
(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'. With prefix P, don't widen, just
narrow even if buffer is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Toby Law"
      user-mail-address "toby@tzcl.me"

      doom-scratch-initial-major-mode 'org-mode
      doom-scratch-buffer-major-mode 'org-mode

      mode-line-default-help-echo nil
      show-help-function nil)

;; Initialise focus-mode to be off
(defvar focus-mode nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monospace" :size 14))
(if (string= (system-name) "xps") (setq doom-font (font-spec :family "monospace" :size 16)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
