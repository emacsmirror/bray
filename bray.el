;;; bray.el --- Lightweight modal editing -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025  Campbell Barton

;; Author: Campbell Barton <ideasman42@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-bray
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; Bray provides a blank-slate for users to define their own modal editing workflow.
;;
;; Key features:
;;
;; - A way for users to define custom states (such as `normal', `insert', `special', etc.)
;; - Per *state* settings such as cursor, keymaps and enter/exit hooks.
;; - Enter/exit hooks can be used to further refine the behavior.
;;
;; The user may define any number of states - which may even be buffer-local,
;; allowing for context-dependent modal editing behavior.

;;; Code:

;; ---------------------------------------------------------------------------
;; Custom Variables


(declare-function bray-state-map--auxiliary-maps "bray-state-map")

(defgroup bray nil
  "Bray mode."
  :group 'convenience)

(defcustom bray-state-default nil
  "The default state when `bray-mode' is enabled.

This must be a symbol matching one of the user-defined states
by its :id in `bray-state-definitions'.

- A nil value causes no states to be enabled.
- You may wish to use different states depending on the major mode;
  this can be done using `bray-state-init'."
  :type 'symbol)

(defcustom bray-state-definitions nil
  "A list where each element is a property list defining a state.

Each property list may contain the following keys:

:id (required)
  A symbol used to identify the state;
  it must be unique within this list.
:keymaps (required)
  A list of cons cells, each referencing a predicate and keymap.

  - The first value of the cell is a symbol;
    when non-nil, the keymap is enabled.
    The symbol t can be used so the keymap is always enabled in this state.
  - The second value is a symbol that resolves to the keymap.

  This value may also be a function that returns a list structured as described;
  the function will be evaluated when entering the state.
:lighter (required)
  A string used to set `bray-state-lighter' to be displayed in the mode-line.
:parent (optional)
  A symbol referencing another valid state.

  Note that this is metadata to support:
  - `bray-state-derived-from-provided-p'
  - `bray-state-derived-p'

  Instead of checking the state against a known value,
  the parent value allows multiple states to be derived
  from another, so state checks can match against multiple states.

  This allows you to define states with subtle differences,
  without complicating logic elsewhere that only needs
  to know about the parent state.
:cursor-type (optional)
  The state's cursor type; see `cursor-type' for details.
:is-input (optional)
  When non-nil, the state is used for textual input
  (the input method is enabled).

  Set this for states where users type text, such as an insert state.
:enter-hook (optional)
  A symbol naming the hook to run when the state is entered.
:exit-hook (optional)
  A symbol naming the hook to run when the state exits.

You may set this using `setq-local' to declare buffer-local states.
This must be done before `bray-mode' is activated."
  :type '(repeat (plist :tag "State property list" :key-type (symbol :tag "State property"))))

(defcustom bray-state-map-enabled nil
  "When non-nil, supports binding to keymaps in specific states.
This loads `bray-state-map'.
Changes to this variable only take effect when switching to a new state."
  :type 'boolean)

;; ---------------------------------------------------------------------------
;; Public Variables

;; The default is typically used as it's overridden by the states.
(defvar-local bray-state-lighter " <nil>"
  "The current state's lighter (a string).")

(defvar-local bray-state nil
  "The current state as a symbol.
Matching the value of :id in `bray-state-definitions'.

Do not set this directly, instead use `bray-state-set'.")

(defvar-local bray-state-init nil
  "The initial state to override `bray-state-default'.
Must be set before `bray-mode' is activated.

- When this is a symbol, it is used for the initial state.
- When this is a list of symbols, it is used to initialize
  `bray-state-stack' with the last state set as active.")

(defconst bray-state-next nil
  "Bound to the value that `bray-state' will be set to.

This is only set when switching states and can be used by `:exit-hook'
to perform any special logic that depends on the next state.")

(defconst bray-state-prev nil
  "Bound to the previous value of `bray-state'.

This is only set when switching states and can be used by `:enter-hook'
to perform any special logic that depends on the previous state.")

(defvar-local bray-state-stack nil
  "A state stack that may be used to push/pop states.")


;; ---------------------------------------------------------------------------
;; Forward Declarations

(declare-function bray-state-map--auxiliary-maps "bray-state-map")
(declare-function bray-state-map-set "bray-state-map")
(declare-function bray-state-map-unset "bray-state-map")


;; ---------------------------------------------------------------------------
;; Internal Variables

;; Will be local, matching `bray-state-definitions'.
(defvar bray--state-data (list))

(defvar-local bray--mode-map-alist nil)
(defvar-local bray--input-method nil)

(defvar bray--emulation-mode-map-installed nil)

;; ---------------------------------------------------------------------------
;; State Utilities

(defun bray--state-ok-or-error (state)
  "Ensure STATE is a valid type."
  (declare (important-return-value nil) (ftype (function (symbol) null)))
  ;; May be nil or a symbol.
  (unless (symbolp state)
    (error "Bray: the state must be a symbol, not a %S" (type-of state))))


;; ---------------------------------------------------------------------------
;; State Variable Access
;;
;; Internally state data is stored in a vector for fast lookups.

(defsubst bray--state-var-id (s)
  "Return the :id from state S."
  (aref s 0))
(defsubst bray--state-var-keymaps (s)
  "Return the :keymaps from state S."
  (aref s 1))
(defsubst bray--state-var-lighter (s)
  "Return the :lighter from state S."
  (aref s 2))
(defsubst bray--state-var-parent (s)
  "Return the parent from state S."
  (aref s 3))
(defsubst bray--state-var-cursor-type (s)
  "Return the :cursor-type from state S."
  (aref s 4))
(defsubst bray--state-var-is-input (s)
  "Return the :is-input from state S."
  (aref s 5))
(defsubst bray--state-var-state-hook-enter-symbol (s)
  "Return the :enter-hook from state S."
  (aref s 6))
(defsubst bray--state-var-state-hook-exit-symbol (s)
  "Return the :exit-hook from state S."
  (aref s 7))


;; ---------------------------------------------------------------------------
;; State Access

(defun bray--state-get-by-id (state)
  "Return the state associated with the symbol STATE."
  (declare (important-return-value t) (ftype (function (symbol) (or vector null))))
  (let ((state-iter bray--state-data)
        (result nil))
    (while state-iter
      (let ((state-vars (car state-iter)))
        (setq state-iter (cdr state-iter))
        (when (eq state (bray--state-var-id state-vars))
          (setq result state-vars)
          ;; Break.
          (setq state-iter nil))))
    result))

(defun bray--state-get-active ()
  "Return the current active state."
  (declare (important-return-value t) (ftype (function () symbol)))
  (bray--state-get-by-id bray-state))


;; ---------------------------------------------------------------------------
;; State Setup

(defun bray--state-set-impl (state-vars-next state-vars-prev)
  "Set the bray state from:
STATE-VARS-NEXT & STATE-VARS-PREV for this buffer.

Return non-nil when the state changed."
  (declare (important-return-value nil) (ftype (function (symbol symbol) boolean)))

  ;; Change the current buffer for the changing the cursor.
  ;; Note, why may want this when called from the mini-buffer.
  ;; (with-current-buffer (window-buffer) ...)

  (let ((bray-state-prev (and state-vars-prev (bray--state-var-id state-vars-prev)))
        (bray-state-next (and state-vars-next (bray--state-var-id state-vars-next))))

    ;; Run: `exit-hook'.
    (when state-vars-prev
      (let ((prev-exit-hook (bray--state-var-state-hook-exit-symbol state-vars-prev)))
        (when (boundp prev-exit-hook)
          (run-hooks prev-exit-hook))))

    ;; `is-input'.
    (let ((prev-is-input (and state-vars-prev (bray--state-var-is-input state-vars-prev)))
          (next-is-input (and state-vars-next (bray--state-var-is-input state-vars-next))))
      (unless (or (null state-vars-prev) (eq prev-is-input next-is-input))
        (when prev-is-input
          (setq-local bray--input-method current-input-method)
          (deactivate-input-method))

        (when next-is-input
          (activate-input-method bray--input-method))))

    ;; `cursor-type'.
    (setq cursor-type
          (cond
           (state-vars-next
            (bray--state-var-cursor-type state-vars-next))
           (t
            (default-value 'cursor-type))))

    ;; `lighter'.
    (cond
     (state-vars-next
      (setq bray-state-lighter (bray--state-var-lighter state-vars-next)))
     (t ;; Use the default value.
      (kill-local-variable 'bray-state-lighter)))

    ;; `keymap'.
    (setq bray--mode-map-alist nil)
    (when state-vars-next
      (let ((state-map (bray--state-var-keymaps state-vars-next)))
        (when (and state-map (symbolp state-map))
          (with-demoted-errors "Bray: error in keymaps callback (%S)"
            (setq state-map (funcall state-map))))
        (pcase-dolist (`(,map-p . ,map) state-map)
          (push (cons map-p (symbol-value map)) bray--mode-map-alist))))

    ;; Set the next `state' active.
    (setq bray-state (and state-vars-next (bray--state-var-id state-vars-next)))

    (when bray-state-map-enabled
      ;; TODO: check if autoload could avoid require here.
      (require 'bray-state-map)
      (setq bray--mode-map-alist
            (append (bray-state-map--auxiliary-maps bray-state) bray--mode-map-alist)))

    ;; Run: `enter-hook'.
    (when state-vars-next
      (let ((next-enter-hook (bray--state-var-state-hook-enter-symbol state-vars-next)))
        (when (boundp next-enter-hook)
          (run-hooks next-enter-hook)))))

  (force-mode-line-update)

  ;; The state changed (always true currently).
  t)

;; ---------------------------------------------------------------------------
;; State Definition

(defun bray--state-define (state-plist)
  "Validate & convert STATE-PLIST to internal state data."
  (declare (important-return-value t) (ftype (function (list) vector)))

  (unless (and state-plist (listp state-plist))
    (error "The state values must be a property list"))

  (let ((kw-state nil)
        ;; Required first.
        (kw-keymaps nil)
        (kw-lighter nil)

        ;; Then optional.
        (kw-parent nil)
        (kw-cursor-type nil)
        (kw-is-input nil)
        (kw-enter-hook-symbol nil)
        (kw-exit-hook-symbol nil)

        ;; Iteration variables.
        (keyw nil)
        (val nil))

    (while (keywordp (setq keyw (car state-plist)))
      (setq state-plist (cdr state-plist))
      (setq val (pop state-plist))
      (pcase keyw
        (:id (setq kw-state val))
        (:keymaps (setq kw-keymaps val))
        (:lighter (setq kw-lighter val))

        (:parent (setq kw-parent val))
        (:cursor-type (setq kw-cursor-type val))
        (:is-input (setq kw-is-input val))

        (:enter-hook (setq kw-enter-hook-symbol val))
        (:exit-hook (setq kw-exit-hook-symbol val))

        (_ (message "Bray: ignoring unknown key %S" keyw))))

    (when state-plist
      (error "Bray: unexpected trailing non-keywords: %S" state-plist))

    ;; Ensure required keywords.
    (unless (and kw-state (symbolp kw-state))
      (error "Bray: expected a :id keyword to contain a symbol value"))
    (unless (or (listp kw-keymaps) (symbolp kw-keymaps))
      (error "Bray: expected a :keymaps to be a list of keymaps or a function"))
    (unless (stringp kw-lighter)
      (error "Bray: property :lighter expected a string"))

    `[
      ;; 0: id.
      ,kw-state
      ;; 1: kw-keymap.
      ,kw-keymaps
      ;; 2: lighter.
      ,kw-lighter
      ;; 3: parent.
      ,kw-parent
      ;; 4: cursor-type.
      ,kw-cursor-type
      ;; 5: is-input.
      ,kw-is-input
      ;; 6: state-hook-enter
      ,kw-enter-hook-symbol
      ;; 7: state-hook-exit
      ,kw-exit-hook-symbol]))


;; ---------------------------------------------------------------------------
;; Public Variable Wrappers
;;
;; These have the advantage they can be forward declared from other packages.

;;;###autoload
(defun bray-state ()
  "Return the current state."
  (declare (important-return-value t) (side-effect-free t) (ftype (function () symbol)))
  bray-state)

;; ---------------------------------------------------------------------------
;; Public Functions to Access State Properties
;;
;; Mostly external systems shouldn't need to access these,
;; expose them selectively.

;;;###autoload
(defun bray-state-get-hook-enter (state)
  "Return the enter hook for STATE."
  (declare (important-return-value t) (side-effect-free t) (ftype (function (symbol) symbol)))
  (let ((state-vars (bray--state-get-by-id state)))
    (unless state-vars
      (error "State %S not known" state))
    (bray--state-var-state-hook-enter-symbol state-vars)))

;;;###autoload
(defun bray-state-get-hook-exit (state)
  "Return the exit hook for STATE."
  (declare (important-return-value t) (side-effect-free t) (ftype (function (symbol) symbol)))
  (let ((state-vars (bray--state-get-by-id state)))
    (unless state-vars
      (error "State %S not known" state))
    (bray--state-var-state-hook-exit-symbol state-vars)))


;; ---------------------------------------------------------------------------
;; Public Functions

;;;###autoload
(defun bray-state-derived-from-provided-p (state state-parent)
  "Check whether STATE equals or is derived from STATE-PARENT."
  (declare
   (important-return-value t)
   (side-effect-free t)
   (ftype (function (symbol symbol) boolean)))
  (bray--state-ok-or-error state)
  (bray--state-ok-or-error state-parent)

  (cond
   ((eq state state-parent)
    t)
   (t
    (let ((result nil))
      (while (setq state-parent
                   (let ((state-vars (bray--state-get-by-id state-parent)))
                     (and state-vars (bray--state-var-parent state-vars))))
        (when (eq state state-parent)
          (setq state-parent nil)
          (setq result t)))
      result))))

;;;###autoload
(defun bray-state-derived-p (state-parent)
  "Check whether the current state equals or is derived from STATE-PARENT."
  (declare (important-return-value t) (side-effect-free t) (ftype (function (symbol) boolean)))
  ;; State checks are performed.
  (bray-state-derived-from-provided-p bray-state state-parent))

;;;###autoload
(defun bray-state-set (state)
  "Set STATE to be the active state, or throw an error if STATE is unknown.

A nil STATE may be used to disable all states;
this is similar to disabling `bray-mode' and may be done
to temporarily turn off all bray's functionality.

Return non-nil when the state changed."
  (declare (important-return-value nil) (ftype (function (symbol) boolean)))
  (bray--state-ok-or-error state)

  (let ((state-vars-next nil))
    ;; Allow a nil state.
    (when state
      (setq state-vars-next (bray--state-get-by-id state))
      (unless state-vars-next
        (error "Bray: unknown state %S" state)))

    (cond
     ((eq state bray-state)
      (message "Bray: state %S is already set" state)
      nil)
     (t
      (bray--state-set-impl state-vars-next (bray--state-get-active))))))


;; ---------------------------------------------------------------------------
;; Public State Stack Functions
;;
;; Note that these are optional. Bray doesn't depend on their use however
;; it can be handy to restore the previous state by popping without users
;; having to define their own method of remember the previous state.

;;;###autoload
(defun bray-state-stack-push (state)
  "Push the current state onto the stack and set STATE active.

Return non-nil when the state changed and was pushed."
  (declare (important-return-value nil) (ftype (function (symbol) boolean)))
  (bray--state-ok-or-error state)

  (cond
   ((eq state bray-state)
    (message "Bray: state %S is already set" state)
    nil)

   (t
    (let ((state-prev bray-state))
      ;; Push afterwards in case this state is invalid.
      (cond
       ((bray-state-set state)
        (push state-prev bray-state-stack)
        t)
       (t
        nil))))))

;;;###autoload
(defun bray-state-stack-pop ()
  "Pop the current state off the stack.

Use after `bray-state-stack-push' to restore the previous state.
When the stack is empty, `bray-state-default' is used.

Return non-nil when the state changed."
  (declare (important-return-value nil) (ftype (function () boolean)))
  (interactive)

  (let ((state
         (cond
          (bray-state-stack
           (pop bray-state-stack))
          (t
           bray-state-default))))
    (cond
     ((eq state bray-state)
      (message "Bray: state %S is already set" state)
      nil)
     (t
      (bray-state-set state)))))


;; ---------------------------------------------------------------------------
;; Minor Mode
;;
;; Note: don't make a globalized mode because it complicates
;; logic when users don't want this enabled & can introduce
;; order of initialization issues.

;;;###autoload
(define-minor-mode bray-mode
  "Minor mode for setting up normal mode in a single buffer."
  :init-value nil
  :lighter bray-state-lighter
  :keymap nil

  (cond
   ;; Enable.
   (bray-mode

    ;; Install the mode-map A-list once.
    (unless bray--emulation-mode-map-installed
      ;; Unlikely, but not impossible this has been added before.
      (delq 'bray--mode-map-alist emulation-mode-map-alists)
      (push 'bray--mode-map-alist emulation-mode-map-alists)
      (setq bray--emulation-mode-map-installed t))

    (let ((maybe-state-defs
           (cond
            ((local-variable-p 'bray-state-definitions)
             (make-local-variable 'bray--state-data)
             bray-state-definitions)
            ((null bray--state-data)
             bray-state-definitions)
            (t
             nil))))

      (when maybe-state-defs
        (setq bray--state-data (mapcar #'bray--state-define maybe-state-defs))))

    (setq bray--mode-map-alist nil)

    (let ((state
           (cond
            (bray-state-init
             (cond
              ;; The state as a list of symbols, use this to initialize the stack
              ;; with the last state in the list set as active.
              ((listp bray-state-init)
               (setq bray-state-stack (reverse bray-state-init))
               ;; The last will be set active.
               (pop bray-state-stack))
              ;; The state as a symbol (simple).
              (t
               bray-state-init)))
            (t
             bray-state-default))))

      (when state
        (let ((state-vars-next (bray--state-get-by-id state)))
          (cond
           (state-vars-next
            (bray--state-set-impl state-vars-next nil))
           (t
            ;; Don't use an error to avoid interfering with mode
            ;; initialization which could cascade into other issues.
            (message "Bray: unknown default state: %S" state)))))))
   ;; Disable.
   (t
    (setq cursor-type (default-value 'cursor-type))

    (kill-local-variable 'bray--input-method)
    (kill-local-variable 'bray--mode-map-alist)
    ;; Most likely global, but may be local.
    (kill-local-variable 'bray--state-data)

    ;; Keep `bray-state-init' as it may be used when entering the mode again.
    (kill-local-variable 'bray-state)
    (kill-local-variable 'bray-state-lighter)
    (kill-local-variable 'bray-state-stack))))


;; ---------------------------------------------------------------------------
;; External Autoloads

;; Autoloads for bray-state-map, avoid having to explicitly "require" this.

;;;###autoload (autoload 'bray-state-map-for-keymap-get "bray-state-map")
;;;###autoload (autoload 'bray-state-map-for-keymap-ensure "bray-state-map")
;;;###autoload (autoload 'bray-state-map-for-keymap-remove "bray-state-map")
;;;###autoload (autoload 'bray-state-map-set "bray-state-map")
;;;###autoload (autoload 'bray-state-map-unset "bray-state-map")

;; TODO: investigate why a duplicate block is needed.
(autoload 'bray-state-map-for-keymap-get "bray-state-map")
(autoload 'bray-state-map-for-keymap-ensure "bray-state-map")
(autoload 'bray-state-map-for-keymap-remove "bray-state-map")
(autoload 'bray-state-map-set "bray-state-map")
(autoload 'bray-state-map-unset "bray-state-map")

(provide 'bray)

;; Local Variables:
;; fill-column: 99
;; indent-tabs-mode: nil
;; End:
;;; bray.el ends here
