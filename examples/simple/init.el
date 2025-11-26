;;; init.el --- Example init. -*- lexical-binding: t -*-

;; This is a simple example that implements a basic VIM like key-map
;; using built-in commands.

;; Run with:
;; emacs --init-dir ./examples/simple

;; Only to support running this within a git repository.
(add-to-list 'load-path (file-name-concat (file-name-directory user-init-file) ".." ".."))

;; No need for startup screen.
(setq inhibit-startup-screen t)

(defun my-define-keys (map &rest keybinds)
  (declare (indent 1))
  (pcase-dolist (`(,key . ,def) keybinds)
    (define-key map (kbd key) def)))

(use-package bray
  :commands (bray-mode)

  :config

  (defvar my-bray-state-normal-map (make-sparse-keymap))
  (defvar my-bray-state-insert-map (make-sparse-keymap))

  (setq bray-state-default 'normal)
  (setq bray-state-definitions
        (list
         (list
          :id 'normal
          ;; Define.
          :cursor-type 'hollow
          :lighter "<N>"
          :keymaps (list '(t . my-bray-state-normal-map)))

         (list
          :id 'insert
          ;; Define.
          :cursor-type 'bar
          :lighter "<I>"
          :keymaps (list '(t . my-bray-state-insert-map))

          ;; Optional.
          :is-input t)))

  (my-define-keys my-bray-state-normal-map

    ;; Left Hand: Row 1.
    '("q" . ignore)
    '("Q" . ignore)
    '("w" . forward-word)
    '("W" . ignore)
    '("e" . ignore)
    '("E" . ignore)
    '("r" . ignore)
    '("R" . ignore)
    '("t" . ignore)
    '("T" . ignore)

    ;; Left Hand: Row 2.
    '("a" . ignore)
    '("A" . ignore)
    '("s" . ignore)
    '("S" . ignore)
    '("d" . kill-region)
    '("D" . ignore)
    '("f" . ignore)
    '("F" . ignore)
    '("g u" . downcase-region)
    '("g U" . upcase-region)
    '("G" . ignore)

    ;; Left Hand: Row 3.
    '("z" . ignore)
    '("Z" . ignore)
    '("x" . delete-char)
    '("X" . delete-backward-char)
    '("c" . ignore)
    '("C" . ignore)
    '("v" . set-mark-command)
    '("V" . ignore)
    '("b" . backward-word)
    '("B" . ignore)

    ;; Right Hand: Row 1.
    '("y" . kill-ring-save)
    '("Y" . ignore)
    '("u" . undo-only)
    '("U" . undo-redo)
    '("i" .
      (lambda ()
        (interactive)
        (bray-state-stack-push 'insert)))
    '("I" . ignore)
    '("o" . ignore)
    '("O" . ignore)
    '("p" . yank)
    '("P" . ignore)
    '("\\" . ignore)
    '("|" . ignore)

    ;; Right Hand: Row 2.
    '("h" . backward-char)
    '("H" . ignore)
    '("j" . next-line)
    '("J" . join-line)
    '("k" . previous-line)
    '("K" . backward-paragraph)
    '("l" . forward-char)
    '("L" . ignore)
    '(";" . ignore)
    '(":" . ignore)
    '("'" . ignore)
    '("\"" . ignore)

    ;; Right Hand: Row 3.
    '("n" . isearch-repeat-forward)
    '("N" . isearch-repeat-backward)
    '("m" . ignore)
    '("M" . ignore)
    '("," . ignore)
    '("<" . indent-rigidly-left-to-tab-stop)
    '("." . repeat)
    '(">" . indent-rigidly-right-to-tab-stop)
    '("/" . isearch-forward-regexp)
    '("?" . isearch-backward-regexp)

    ;; Other keys.

    '("RET" . newline-and-indent))

  (my-define-keys my-bray-state-insert-map
    ;; Other keys.

    '("<escape>" . bray-state-stack-pop)))

;; Enable bray for "typical" editing operation.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (when (and (null (minibufferp)) (null (derived-mode-p 'special-mode)))
     (bray-mode))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package" "use-package-core")
;; End:
;;; init.el ends here
