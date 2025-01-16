;;; init.el --- Example init. -*- lexical-binding: t -*-

;; This is a minimal example with HJKL motion, insert & normal modes.

;; Run with:
;; emacs --init-dir ./examples/minimal

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
    '("w" . ignore)
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
    '("d" . ignore)
    '("D" . ignore)
    '("f" . ignore)
    '("F" . ignore)
    '("g" . ignore)
    '("G" . ignore)

    ;; Left Hand: Row 3.
    '("z" . ignore)
    '("Z" . ignore)
    '("x" . ignore)
    '("X" . ignore)
    '("c" . ignore)
    '("C" . ignore)
    '("v" . ignore)
    '("V" . ignore)
    '("b" . ignore)
    '("B" . ignore)

    ;; Right Hand: Row 1.
    '("y" . ignore)
    '("Y" . ignore)
    '("u" . ignore)
    '("U" . ignore)
    '("i" .
      (lambda ()
        (interactive)
        (bray-state-stack-push 'insert)))
    '("I" . ignore)
    '("o" . ignore)
    '("O" . ignore)
    '("p" . ignore)
    '("P" . ignore)
    '("\\" . ignore)
    '("|" . ignore)

    ;; Right Hand: Row 2.
    '("h" . backward-char)
    '("H" . ignore)
    '("j" . next-line)
    '("J" . ignore)
    '("k" . previous-line)
    '("K" . ignore)
    '("l" . forward-char)
    '("L" . ignore)
    '(";" . ignore)
    '(":" . ignore)
    '("'" . ignore)
    '("\"" . ignore)

    ;; Right Hand: Row 3.
    '("n" . ignore)
    '("N" . ignore)
    '("m" . ignore)
    '("M" . ignore)
    '("," . ignore)
    '("<" . ignore)
    '("." . ignore)
    '(">" . ignore)
    '("/" . ignore)
    '("?" . ignore)

    ;; Other keys.

    '("RET" . newline-and-indent))

  (my-define-keys my-bray-state-insert-map
    ;; Other keys.

    '("<escape>" . bray-state-stack-pop)))

;; Enable bray for "typical" editing operation.
(add-hook
 'after-change-major-mode-hook
 (lambda ()
   (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
     (bray-mode))))

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; elisp-autofmt-load-packages-local: ("use-package")
;; End:
;;; bray.el ends here
