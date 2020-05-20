;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;
;;; TODO

;; TODO: set up key bindings for LSP mode, learn to use more effectively
;; TODO: set up bookmarks to access common files/websites/folders quickly
;; TODO: writeroom mode doesn't work with multiple buffers (delete the others
;; and restore)? Look into persp

;;; Org
;; TODO: set up org-capture templates (writing ideas, video ideas, ??)
;; TODO: set up org agenda? lots of functionality but have other equivalent
;; tools, like using Todoist because it's cross-platform (mobile)
;; TODO: look into how org-download works
;; TODO: focus-mode not working properly with lists, look at paragraph definition

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

;; Define paragraphs in text-mode to include lists (and make sure auto-fill is enabled)
(defun toby/text-mode-hook ()
  (setq paragraph-start "^\n")
  (setq paragraph-separate "\n\n"))
(add-hook 'text-mode-hook #'toby/text-mode-hook)

;; Enable auto-fill-mode everywhere
(auto-fill-mode 1)

;;
;;; UI

(setq doom-font (font-spec :family "monospace" :size 14))

;;
;;; Keybinds

(map! "C-'" 'better-comment-dwim
      "C-x n" 'narrow-or-widen-dwim

      :mnv "g D" 'xref-find-definitions-other-window

      :v "DEL" 'evil-delete-char)

(map! :leader
      "t s" 'toby/flyspell-mode
      "f i" 'toby/find-index)

(map! :map org-mode-map
      :mnv "SPC m h" 'toby/org-toggle-headings
      :ei "M-SPC m h" 'toby/org-toggle-headings
      ;; :mnv "SPC m d" 'toby/img-complete-link breaks org-capture keybindings
      ;; :ei "M-SPC m d" 'toby/img-complete-link

      ;; :mnv "SPC m D" 'org-deadline
      ;; :ei "M-SPC m D" 'org-deadline)
      )

;;
;;; Modules

;;; flyspell
;; Disable by default
(remove-hook! '(org-mode-hook
                markdown-mode-hook
                TeX-mode-hook
                rst-mode-hook
                mu4e-compose-mode-hook
                message-mode-hook)
  #'flyspell-mode)
(defun toby/flyspell-mode ()
  (interactive)
  (flyspell-mode 'toggle)
  (if flyspell-mode (flyspell-buffer)))

;;; zen-mode
;; Clean up zen-mode
(defun toby/writeroom-mode-hook ()
  (display-line-numbers-mode 'toggle)
  (hl-line-mode 'toggle)
  (visual-line-mode 'toggle)
  (company-mode 'toggle)
  (focus-mode 'toggle))
(add-hook 'writeroom-mode-hook #'toby/writeroom-mode-hook)

;; Set transparency in Emacs so writeroom can restore it
(add-to-list 'default-frame-alist '(alpha 95 80))

;;; doom-dashboard
;; Add index.org
(defun toby/find-index ()
  (interactive)
  (find-file "/home/toby/gdrive/index.org"))
(add-to-list '+doom-dashboard-menu-sections '("Open index"
                                              :icon (all-the-icons-octicon "repo" :face 'doom-dashboard-menu-title)
                                              :action toby/find-index) t)

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

        org-journal-file-type 'weekly

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
