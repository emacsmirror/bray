;;; bray-state-map.el --- State-specific keymap bindings for bray -*- lexical-binding: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2025 Luis Henriquez

;; Author: Luis Henriquez <luishenriquezperez@gmail.com>

;; URL: https://codeberg.org/ideasman42/emacs-bray
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; This feature allows binding keys to a keymap in a specific state.
;;
;; Key features:
;;
;; - A way for users to bind keys to keymaps in specific states.
;;
;; For example, the following would bind "d" to #'foo only when `vertico-map' is
;; active and bray is in insert state.
;;
;; (bray-state-map-set 'insert vertico-map "d" #'foo)
;;
;; - User facing functions to access auxiliary keymaps:
;;   `bray-state-map-for-keymap-get',
;;   `bray-state-map-for-keymap-ensure',
;;   `bray-state-map-for-keymap-remove'.

;;; Code:

;; ---------------------------------------------------------------------------
;; Internal Variables

(defvar bray-state-map--alist nil
  "An alist that maps keymaps to their auxiliary maps for specific states.
The `car' of each element is the original keymap and the `cdr' is an alist
mapping a state to it is corresponding auxiliary keymap.
Each element looks like:
\(ORIG-KEYMAP . ((STATE1 . AUX-KEYMAP1) (STATE2 . AUX-KEYMAP2) ...)).")

;; ---------------------------------------------------------------------------
;; Internal Functions
;;

(defsubst bray-state-map--for-keymap-get-impl (state keymap)
  "Return the auxiliary keymap for KEYMAP in STATE or nil."
  (alist-get state (cdr (assoc keymap bray-state-map--alist #'eq))))

(defsubst bray-state-map--for-keymap-ensure-impl (state keymap)
  "Return the auxiliary keymap for KEYMAP in STATE, creating it if needed."
  (or (bray-state-map--for-keymap-get-impl state keymap)
      (let ((elt (assoc keymap bray-state-map--alist #'eq))
            (aux-keymap (make-sparse-keymap)))
        (if elt
            (push (cons state aux-keymap) (cdr elt))
          (push
           (cons keymap (list (cons state aux-keymap))) bray-state-map--alist))
        aux-keymap)))

(defsubst bray-state-map--for-keymap-remove-impl (state keymap)
  "Remove the auxiliary keymap for KEYMAP in STATE, return t if removed."
  (when-let* ((elt (assoc keymap bray-state-map--alist #'eq))
              (aux-keymap (alist-get state (cdr elt))))
    (ignore aux-keymap)
    (setcdr elt (assq-delete-all state (cdr elt)))
    (unless (cdr elt)
      (setq bray-state-map--alist (delq elt bray-state-map--alist)))
    t))

(defun bray-state-map--auxiliary-maps (state)
  "Return auxiliary keymaps that should be active in the current STATE."
  (let ((maps nil)
        (active-maps (current-active-maps 'olp)))
    (pcase-dolist (`(,keymap . ,state-alist) bray-state-map--alist)
      (when-let* ((aux-keymap
                   (and (memq keymap active-maps)
                        (alist-get state state-alist))))
        (push (cons t aux-keymap) maps)))
    (nreverse maps)))

;; ---------------------------------------------------------------------------
;; Public Functions
;;

;;;###autoload
(defun bray-state-map-for-keymap-get (state keymap)
  "Return the auxiliary keymap for KEYMAP in STATE.
This is the keymap where state-specific bindings are stored.
If no auxiliary keymap exists, return nil."
  (bray-state-map--for-keymap-get-impl state keymap))

;;;###autoload
(defun bray-state-map-for-keymap-ensure (state keymap)
  "Return the auxiliary keymap for KEYMAP in STATE, creating it if needed.
This is the keymap where state-specific bindings are stored."
  (bray-state-map--for-keymap-ensure-impl state keymap))

;;;###autoload
(defun bray-state-map-for-keymap-remove (state keymap)
  "Remove the auxiliary keymap for KEYMAP in STATE.
Return non-nil if the auxiliary keymap existed and was removed, nil otherwise."
  (bray-state-map--for-keymap-remove-impl state keymap))

;;;###autoload
(defun bray-state-map-set (state keymap key def)
  "Bind KEY to DEF for KEYMAP in STATE.

STATE is a symbol identifying a bray state.
KEYMAP is the keymap that has state-specific bindings.
KEY is a key sequence string (as accepted by `keymap-set').
DEF is the definition to bind to KEY."
  (keymap-set (bray-state-map--for-keymap-ensure-impl state keymap) key def))

;;;###autoload
(defun bray-state-map-unset (state keymap key)
  "Remove the binding for KEY in KEYMAP for STATE.

STATE is a symbol identifying a bray state.
KEYMAP is the keymap that has state-specific bindings.
KEY is a key sequence string (as accepted by `keymap-unset').

Do nothing if no binding exists for this state/keymap/key combination."
  (declare (important-return-value nil))
  (when-let* ((aux-keymap (bray-state-map--for-keymap-get-impl state keymap)))
    (keymap-unset aux-keymap key)))
;;; provide
(provide 'bray-state-map)
;;; bray-state-map.el ends here
