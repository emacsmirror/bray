
############################
Bray Modal Editing for Emacs
############################


Description
===========

Bray provides a blank-slate for users to define their own modal editing workflow.

Key features:

- A way for users to define custom states (such as ``normal``, ``insert``, ``special``, etc.)
- Per **state** settings such as cursor, keymaps and enter/exit hooks.
- Enter/exit hooks can be used to further refine the behavior.
- Ability to bind keys to keymaps in specific states.

The user may define any number of states - which may even be buffer-local,
allowing for context-dependent modal editing behavior.

Available via `melpa <https://melpa.org/#/bray>`__.


Motivation
==========

At the time of writing, modal editing systems are tied to opinionated design decisions
regarding how modal editing should be done.

This is fine as long as you're happy to work under these constraints,
but there can be downsides where you would prefer different behavior;
there can also be problems mixing these modes with other Emacs packages.

In contrast, **bray** aims simply to let the user define their own modal editing system,
comprised of "states" the user may switch between.


Design Decisions
================

While this aims to be a generic modal editing system,
some opinionated decisions have been made.

- No third-party dependencies.

- Only one ``state`` can be active at a time.

  While layering states is not supported, states may share keymaps.

- Only one minor-mode ``bray-mode``, which must be explicitly enabled.

  A globalized minor-mode is avoided as controlling when this is/isn't activated can become complicated.

- Each ``state`` contains a limited number of settings,
  with the expectation that further per-state settings will be implemented via hooks.

- Only explicit actions are called, no ``pre/post-command-hooks``.

  Once ``bray-mode`` has been activated,
  the keymaps associated with the current state will be active;
  there is no further integration or hooks into Emacs
  (which can add overhead and cause compatibility issues).

- Prioritize simplicity.

  Other modal editing systems often resort to kludges
  to implement their entire feature set.

  Since bray aims not to "get in the way" in order to prevent conflicts with other packages,
  functionality that doesn't fit well with Emacs has been left out.
  This is why (for example) there is no support for per-state cursor-color,
  as it's not a buffer-local setting; ensuring its validity gets rather involved.

- Use of a "stack" to manage the state (optional).

  To support entering and restoring the previous state,
  push & pop functions have been included.

  Using the "stack" is optional; you may prefer to consider all states top-level.
  In this case, only ``bray-state-set`` needs to be used and all ``bray-state-stack-*`` functions can be ignored.

  Using a stack has the advantage that the interactive "pop" function
  can be bound to a key that doesn't need any arguments.


Usage
=====


Trying it Out
-------------

If you'd like to try ``bray`` without editing your local configuration,
a simple init file is included for reference.

.. code-block:: sh

   emacs --init-dir ./examples/simple

This provides a *very* basic VIM-like modal editing configuration,
with ``HJKL`` motion, ``V`` for selection, ``I`` for insert, ``/`` for search, etc.

Configuration
-------------

This example shows a minimal configuration (HJKL motion & insert mode enter/exit).
Note that names such as ``normal`` & ``insert`` are user-defined;
you may name & add states as you please.

.. code-block:: elisp

   (use-package bray
    :commands (bray-mode)

    :config

    ;; Typical normal/insert states.
    (defvar my-bray-state-normal-map (make-sparse-keymap))
    (defvar my-bray-state-insert-map (make-sparse-keymap))

    (setq bray-state-default 'normal)
    (setq bray-state-definitions
          (list
           (list
            :id 'normal
            :cursor-type 'hollow
            :lighter "<N>"
            :keymaps (list '(t . my-bray-state-normal-map)))

           (list
            :id 'insert
            :cursor-type 'bar
            :lighter "<I>"
            :keymaps (list '(t . my-bray-state-insert-map))

            ;; Optional.
            :is-input t)))

    ;; Optional, a quick way to mask insertion.
    (define-key my-bray-state-normal-map [remap self-insert-command] 'ignore)

    ;; HJKL & 'I' for insert mode.
    (define-key my-bray-state-normal-map (kbd "h") 'backward-char)
    (define-key my-bray-state-normal-map (kbd "j") 'next-line)
    (define-key my-bray-state-normal-map (kbd "k") 'previous-line)
    (define-key my-bray-state-normal-map (kbd "l") 'forward-char)

    ;; Insert mode & escape to leave.
    (define-key
     my-bray-state-normal-map (kbd "i")
     (lambda ()
       (interactive)
       (bray-state-stack-push 'insert)))

    (define-key my-bray-state-insert-map (kbd "<escape>") 'bray-state-stack-pop))

   ;; Enable bray for "typical" editing operations.
   (add-hook
    'after-change-major-mode-hook
    (lambda ()
      (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
        (bray-mode))))


Defining State-Specific Keymaps
-------------------------------

The following example showcases how to bind keys to keymaps in specific states.
Once evaluated, pressing "h" or "l" in a buffer with dired-mode-map active in
normal state will invoke ``dired-up-directory`` and ``dired-find-file``
respectively. For those familiar with evil, this functionality is akin to
``evil-define-key*``.

Note that to use this feature, the custom variable ``bray-state-map-enabled`` must be
set to a non-nil value before changing state.

.. code-block:: elisp

   (setq bray-state-map-enabled t)
   (bray-state-map-set 'normal dired-mode-map "h" 'dired-up-directory)
   (bray-state-map-set 'normal dired-mode-map "l" 'dired-find-file)

To access this keymap, you can use ``bray-state-map-for-keymap-get`` or
``bray-state-map-for-keymap-ensure`` if you wish to create the keymap as needed.

.. code-block:: elisp

   (bray-state-map-for-keymap-get 'normal dired-mode-map)
   ;; => (keymap (108 . dired-find-file) (104 . dired-up-directory))

To unset a key, bray provides a counterpart to the built-in ``keymap-unset``.
The following example unsets the "h" key bound previously.

.. code-block:: elisp

   (bray-state-map-unset 'normal dired-mode-map "h")

If you wish to toggle all state-specific bindings, set
``bray-state-map-enabled`` to its opposite value and then change state once
for the change to take effect.

.. code-block:: elisp

   (setq bray-state-map-enabled (not bray-state-map-enabled))

.. BEGIN VARIABLES

Custom Variables
----------------

``bray-state-default``: ``nil``
   The default state when ``bray-mode`` is enabled.

   This must be a symbol matching one of the user-defined states
   by its ``:id`` in ``bray-state-definitions``.

   - A nil value causes no states to be enabled.
   - You may wish to use different states depending on the major mode;
     this can be done using ``bray-state-init``.

``bray-state-definitions``: ``nil``
   A list where each element is a property list defining a state.

   Each property list may contain the following keys:

   ``:id`` (required)
     A symbol used to identify the state;
     it must be unique within this list.
   ``:keymaps`` (required)
     A list of cons cells, each referencing a predicate and keymap.

     - The first value of the cell is a symbol;
       when non-nil, the keymap is enabled.
       The symbol t can be used so the keymap is always enabled in this state.
     - The second value is a symbol that resolves to the keymap.

     This value may also be a function that returns a list structured as described;
     the function will be evaluated when entering the state.
   ``:lighter`` (required)
     A string used to set ``bray-state-lighter`` to be displayed in the mode-line.
   ``:parent`` (optional)
     A symbol referencing another valid state.

     Note that this is metadata to support:
     - ``bray-state-derived-from-provided-p``
     - ``bray-state-derived-p``

     Instead of checking the state against a known value,
     the parent value allows multiple states to be derived
     from another, so state checks can match against multiple states.

     This allows you to define states with subtle differences,
     without complicating logic elsewhere that only needs
     to know about the parent state.
   ``:cursor-type`` (optional)
     The state's cursor type; see ``cursor-type`` for details.
   ``:is-input`` (optional)
     When non-nil, the state is used for textual input
     (the input method is enabled).

     Set this for states where users type text, such as an insert state.
   ``:enter-hook`` (optional)
     A symbol naming the hook to run when the state is entered.
   ``:exit-hook`` (optional)
     A symbol naming the hook to run when the state exits.

   You may set this using ``setq-local`` to declare buffer-local states.
   This must be done before ``bray-mode`` is activated.

``bray-state-map-enabled``: ``nil``
   When non-nil, supports binding to keymaps in specific states.
   This loads ``bray-state-map``.
   Changes to this variable only take effect when switching to a new state.


Other Variables
---------------

``bray-state-lighter``: ``" <nil>"``
   The current state's lighter (a string).

``bray-state-init``: ``nil``
   The initial state to override ``bray-state-default``.
   Must be set before ``bray-mode`` is activated.

   - When this is a symbol, it is used for the initial state.
   - When this is a list of symbols, it is used to initialize
     ``bray-state-stack`` with the last state set as active.

``bray-state-next``: ``nil``
   Bound to the value that ``bray-state`` will be set to.

   This is only set when switching states and can be used by ``:exit-hook``
   to perform any special logic that depends on the next state.

``bray-state-prev``: ``nil``
   Bound to the previous value of ``bray-state``.

   This is only set when switching states and can be used by ``:enter-hook``
   to perform any special logic that depends on the previous state.

``bray-state-stack``: ``nil``
   A state stack that may be used to push/pop states.


Commands
--------

``(bray-state-stack-pop)``
   Pop the current state off the stack.

   Use after ``bray-state-stack-push`` to restore the previous state.
   When the stack is empty, ``bray-state-default`` is used.

   Return non-nil when the state changed.


Functions
---------

``(bray-state)``
   Return the current state.

``(bray-state-get-hook-enter STATE)``
   Return the enter hook for STATE.

``(bray-state-get-hook-exit STATE)``
   Return the exit hook for STATE.

``(bray-state-derived-from-provided-p STATE STATE-PARENT)``
   Check whether STATE equals or is derived from STATE-PARENT.

``(bray-state-derived-p STATE-PARENT)``
   Check whether the current state equals or is derived from STATE-PARENT.

``(bray-state-set STATE)``
   Set STATE to be the active state, or throw an error if STATE is unknown.

   A nil STATE may be used to disable all states;
   this is similar to disabling ``bray-mode`` and may be done
   to temporarily turn off all bray's functionality.

   Return non-nil when the state changed.

``(bray-state-stack-push STATE)``
   Push the current state onto the stack and set STATE active.

   Return non-nil when the state changed and was pushed.


State Map
=========

This module provides state-specific keymap bindings, allowing keys to be bound
to a keymap only when a particular state is active.

Requires ``bray-state-map-enabled`` to be non-nil.


Functions
---------

``(bray-state-map-for-keymap-get STATE KEYMAP)``
   Return the auxiliary keymap for KEYMAP in STATE.
   This is the keymap where state-specific bindings are stored.
   If no auxiliary keymap exists, return nil.

``(bray-state-map-for-keymap-ensure STATE KEYMAP)``
   Return the auxiliary keymap for KEYMAP in STATE, creating it if needed.
   This is the keymap where state-specific bindings are stored.

``(bray-state-map-for-keymap-remove STATE KEYMAP)``
   Remove the auxiliary keymap for KEYMAP in STATE.
   Return non-nil if the auxiliary keymap existed and was removed, nil otherwise.

``(bray-state-map-set STATE KEYMAP KEY DEF)``
   Bind KEY to DEF for KEYMAP in STATE.

   STATE is a symbol identifying a bray state.
   KEYMAP is the keymap that has state-specific bindings.
   KEY is a key sequence string (as accepted by ``keymap-set``).
   DEF is the definition to bind to KEY.

``(bray-state-map-unset STATE KEYMAP KEY)``
   Remove the binding for KEY in KEYMAP for STATE.

   STATE is a symbol identifying a bray state.
   KEYMAP is the keymap that has state-specific bindings.
   KEY is a key sequence string (as accepted by ``keymap-unset``).

   Do nothing if no binding exists for this state/keymap/key combination.

.. END VARIABLES


Other Packages
==============

While there are many packages that implement modal editing,
most embed their own editing system. At the time of writing,
there are few direct equivalents.

`Lithium <https://melpa.org/#/lithium>`__
   This is quite close to Bray in that it is a lightweight package
   intended for defining a modal editing workflow.

   The main differences I'm aware of are:

   - Bray uses a single minor-mode that can switch between "states",
     whereas Lithium requires each ``mode`` to be an Emacs minor-mode.

     This has pros and cons: minor-modes conveniently have their own hooks and keymaps,
     but Bray's approach of treating states as configuration allows them to be buffer-local
     with multiple keymaps per state.

   - Bray expects users to create keymaps using Emacs built-in functions,
     whereas Lithium provides its own utilities for defining keymaps.

   - Bray is intended for technical users who define their own modal editing workflow,
     whereas Lithium intends to be middleware (used by other packages).

`ryo-modal <https://melpa.org/#/ryo-modal>`__
   Also has the goal: *to create your own modal editing environment*.

   The main differences I'm aware of are:

   - Bray supports switching between multiple user-defined "states",
     whereas ``ryo-modal`` uses a single ``normal`` mode (to borrow VI's terminology).

   - Bray expects users to provide each state's keymaps,
     whereas ``ryo-modal`` contains sophisticated utilities to declare the keymap,
     integrating other packages such as multiple-cursors & hydra.

   - Bray has no special logic that runs automatically,
     whereas ``ryo-modal`` installs hooks that run after every command to detect whether the command
     should be repeated & update the cursor color.

   So it's fair to say ``ryo-modal`` is more opinionated & sophisticated;
   it does more for you but you also need to buy into how it works.
