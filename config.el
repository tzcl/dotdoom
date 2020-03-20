;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Make focus mode work with paragraphs
(setq focus-mode-to-thing '((prog-mode . defun) (text-mode . paragraph)))

;; Define paragraphs in text-mode to include lists
(defun toby/text-mode-hook ()
  (setq paragraph-start "^\n")
  (setq paragraph-separate "\n\n"))
(add-hook 'text-mode-hook #'toby/text-mode-hook)

(defun toby/toggle-minor-mode (mode)
  (if (symbol-value mode) (funcall (symbol-function mode) 0) (funcall (symbol-function mode) 1)))

;; Disable line numbers in zen-mode
(require 'focus)
(defun toby/writeroom-mode-hook ()
  (toby/toggle-minor-mode 'display-line-numbers-mode)
  (toby/toggle-minor-mode 'hl-line-mode)
  (toby/toggle-minor-mode 'focus-mode))
(add-hook 'writeroom-mode-hook #'toby/writeroom-mode-hook)

;; Define keys
(general-define-key
 :keymaps 'override
 "C-'" 'better-comment-dwim
 "C-x n" 'narrow-or-widen-dwim)

(general-define-key
 :states 'insert
 "RET" '+default/newline
 "C-j" 'newline-and-indent)

(general-define-key
 :states 'visual
 "DEL" 'evil-delete-char)

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

;; Initialised focus-mode to be off
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
