
############################
Bray Modal Editing for Emacs
############################


Description
===========

Bray provides a blank-slate for users to define their own modal editing workflow.

Key features:

- A way for  to define custom states (such as ``normal``, ``insert``, ``special`` etc).
- Per **state** settings such as cursor, key-maps & enter/exit hooks.
- Enter/exit hooks can be used to further refine the behavior.

The user may define any number of states - which may even be buffer-local,
allowing for context-dependent modal editing behavior.

Available via `melpa <https://melpa.org/#/bray>`__.


Motivation
==========

At the time of writing, modal editing systems are tied to opinionated design decisions
regarding how modal editing should be done.

This is fine as long as you're happy to work under these constraints,
but there can be down sides where you would prefer different behavior,
there can also be problems mixing these modes with other emacs packages.

In contrast, **bray** aims simply to let the user define their own modal editing system,
comprised of "states" the user may switch between.


Design Decisions
================

While this aims to be a generic modal editing system,
some opinionated decisions have been made.

- No 3rd party dependencies.

- Only one ``state`` can be active at a time.

  While layering states is not supported, states may share key-maps.

- Only one minor-mode ``bray-mode`` which must be explicitly enabled.

  A globalized minor-mode is avoided as controlling when this is/isn't activated can become complicated.

- Each ``state`` contains a limited number of settings,
  with the expectation further per-state settings will be implemented via hooks.

- Only explicit actions are called, no ``pre/post-command-hooks``.

  Once ``bray-mode`` has been activated,
  the key-maps associated with the current state will be active,
  there is no further integration or hooks into Emacs
  (which can add overhead and cause compatibility issues).

- Prioritize simplicity.

  Checking other modal editing systems they often resort to kludges
  to implement their entire feature set.

  Since bray aims not to *"get in the way"* in order to prevent conflicts with other packages,
  functionality that doesn't fit well with Emacs has been left out.
  This is why (for example) there is no support for per-state cursor-color,
  as it's not a buffer local setting, ensuring it's valid gets rather involved.

- Use of a "stack" to manage the state (optional).

  To support entering and restoring the previous state,
  push & pop functions have been included.

  Using the "stack" is optional, you may prefer to consider all states top-level.
  In this case only ``bray-state-set`` needs to be used and all ``bray-state-stack-*`` functions can be ignored.

  Using a stack has the advantage that the interactive "pop" function
  can be bound to a key which doesn't need any arguments.


Usage
=====


Trying it Out
-------------

If you'd like to try ``bray`` without editing your local configuration,
a simple init file is included for reference.

.. code-block:: sh

   emacs --init-dir ./examples/simple

This provides a *very* basic VIM like modal editing configuraiton,
with ``HJKL`` motion, ``V`` for selection, ``I`` for insert, ``/`` for search... etc.


Installation
------------

This example shows a minimal configuration (HJKL motion & insert mode enter/exit).
Note that names such as ``normal`` & ``insert`` are user defined,
you may name & add modes as you please.

.. code-block:: elisp

   (use-package bray
    :commands (bray-mode)

    :config

    ;; Typical normal/insert states.
    (defvar my-bray-state-normal-map (make-keymap))
    (defvar my-bray-state-insert-map (make-keymap))

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

   ;; Enable bray for "typical" editing operation.
   (add-hook
    'after-change-major-mode-hook
    (lambda ()
      (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
        (bray-mode))))


.. BEGIN VARIABLES

Custom Variables
----------------

``bray-state-default``: ``nil``
   Define the default state when ``bray-mode`` is enabled.

   This must be a symbol matching one of the user defined states
   by its ``:id`` in ``bray-state-definitions``.

   - A nil value causes no states to be enabled.
   - You may wish to different states depending on the major mode,
     this can be done using ``bray-state-init``.

``bray-state-definitions``: ``nil``
   An list where values are a property list for each state.

   Each state maps to a property list containing the following keys:

   ``:id`` (required)
     A symbol used to identify the state,
     must be unique within this list.
   ``:keymaps`` (required)
     A list of cons cells, each referencing a predicate and key-map.

     - The first value of the cell is a symbol,
       when non-nil the key-map is enabled.
       The symbol t can be used so the key-map is always enabled in this state.
     - The second value is a symbol that resolves to the key-map.

     This value may also be a function that returns a list structured as described,
     the function will be evaluated when entering the state.
   ``:lighter`` (required)
     A string, used to set ``bray-state-lighter`` to be displayed in the mode-line.
   ``:cursor-type`` (optional)
     The states cursor type, see docs for ``cursor-type``.
   ``:is-input`` (optional)
     When non-nil, the mode is used for textual input
     (the input method is enabled).

     Note that in most cases this should be set.
   ``:enter-hook`` (optional)
     A symbol, naming the hook to run when the state is entered.
   ``:exit-hook`` (optional)
     A symbol, naming the hook to run when the state exits.

   You may set this using ``setq-local`` to declare buffer-local states.
   This must be done before ``bray-mode`` is activated.


Other Variables
---------------

``bray-state-lighter``: ``"<nil>"``
   The current states lighter (a string).

``bray-state``: ``nil``
   The current state as a symbol.
   Matching the value of ``:id`` in ``bray-state-definitions``.

   Do not set this directly, instead use ``bray-state-set``.

``bray-state-init``: ``nil``
   The initial state to override ``bray-state-default``.
   Must be set before ``bray-mode`` is activated.

   - When this is a symbol, it is used for the initial state.
   - When this is a list of symbols it is used to initialize
     ``bray-state-stack`` with the last state set as active.

``bray-state-next``: ``nil``
   Bound to the value ``bray-state`` will be set to.

   This is only set when switching states & can be used by ``:exit-hook``
   to perform any special logic that depends the next state.

``bray-state-prev``: ``nil``
   Bound to the value ``bray-state`` was set to.

   This is only set when switching states & can be used by ``:enter-hook``
   to perform any special logic that depends the previous states.

``bray-state-stack``: ``nil``
   A state stack which may be used to push/pop states from the stack.


Functions
---------

``(bray-state-set STATE)``
   Set STATE to be the active state.
   Or throw an error if state is unknown.

   A nil STATE may be used to disable all states,
   this is similar to disabling `bray-mode' and may be done
   to temporarily turn off all bray's functionality.

   Return non-nil when the state changed.

``(bray-state-stack-push STATE)``
   Push the current state onto the stack and set STATE active.

   Return non-nil when the state changed as was pushed.

``(bray-state-stack-pop)``
   Pop the current state off the stack.

   Use after `bray-state-stack-push' to restore the previous state.
   When the stack is empty `bray-state-default' is used.

   Return non-nil when the state changed.

.. END VARIABLES


Other Packages
==============

While there are many packages that implement modal editing,
most embed their own editing system, at the time of writing
there are few direct equivalents.

`Lithium <https://melpa.org/#/lithium>`__
   This is quite close to Bray in that it is a lightweight package
   intended for defining a modal editing workflow.

   The main differences I'm aware of are:

   - Bray uses a single minor-mode that can switch between "states".
     whereas Lithium requires each ``mode`` to be an emacs minor-mode.

     This has pros & cons, it's convenient as minor-modes have their own hooks & key-maps
     but the disadvantage compared with Bray which treats "states" as configuration,
     meaning they can be buffer-local, with multiple key-maps per state.

   - Bray expects users to create key-maps using Emacs built-in functions.
     whereas Lithium provides it's own utilities for defining key-maps.

   - Bray is intended for technical users who define their own modal editing workflow
     whereas Lithium intends to be middle-ware (used by other packages).

`ryo-modal <https://melpa.org/#/ryo-modal>`__
   Also has the goal: *to create your own modal editing environment*

   The main differences I'm aware of are:

   - Bray supports switching between multiple user defined "states",
     whereas ``ryo-modal`` uses a single ``normal`` mode (to borrow VI's terminology).

   - Bray expects users to provide each "states" key-maps
     whereas ``ryo-modal`` contains sophisticated utilities to declare the key-map
     integrating other packages such as multiple-cursors & hydra.

   - Bray has no special logic that runs automatically
     whereas ``ryo-modal`` installs hooks that run after every command to detect if the command
     should be repeated & update the cursor color.

   So it's fair to say ``ryo-modal`` is more opinionated & sophisticated,
   it does more for you but you also need buy into how it works.
