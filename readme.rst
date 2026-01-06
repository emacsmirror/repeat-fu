###############
Emacs Repeat-FU
###############

Execute sequences of Emacs commands with a single keystroke,
designed for modal editing workflows.

Available via `melpa <https://melpa.org/#/repeat-fu>`__.


Motivation
==========

Unlike built-in repeat commands that only replay single actions,
Repeat-FU can replay *"select 3 words -> replace with text"* as one repeatable unit.

The exact behavior for this is configurable so it can be configured to work differently depending on the users needs.
This is especially well suited to modal editing, where you may wish to repeat a change or insertion elsewhere.


Usage
=====

To use Repeat-FU you must:

- Enable ``repeat-fu-mode``.
- Bind ``repeat-fu-execute`` to a key.

After this, calling ``repeat-fu-execute`` will repeat the last edit,
the exact behavior depends on the ``repeat-fu-preset`` which allows
different behavior to be used based on your preferences.

An "edit" includes buffer changes along with preceding selection/motion commands that set up the change.


----

This example shows how Repeat-FU can be used with the default emacs configuration.

.. code-block:: elisp

   (use-package repeat-fu
      :commands (repeat-fu-mode repeat-fu-execute)
      :bind
      ("C-." . repeat-fu-execute)
      :hook
      (after-change-major-mode
       .
       (lambda ()
         (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
           (repeat-fu-mode)))))


This example shows how Repeat-FU can be used with meow.

.. code-block:: elisp

   (use-package repeat-fu
     :commands (repeat-fu-mode repeat-fu-execute)

     :config
     (setq repeat-fu-preset 'meow)

     :hook
     ((meow-mode)
      .
      (lambda ()
        (when (and (not (minibufferp)) (not (derived-mode-p 'special-mode)))
          (repeat-fu-mode)
          (define-key meow-normal-state-keymap (kbd "C-'") 'repeat-fu-execute)
          (define-key meow-insert-state-keymap (kbd "C-'") 'repeat-fu-execute)))))

Customizing Commands
--------------------

Commands can be customized using ``repeat-fu-declare``, this example ensures a
custom save command is never repeated.

.. code-block:: elisp

   (repeat-fu-declare 'my-save-command :skip t :skip-change t)

See the doc-string below for details.


.. BEGIN VARIABLES

Custom Variables
----------------

``repeat-fu-preset``: ``multi``
   The named preset to use (as a symbol).
   This loads a bundled preset with the ``repeat-fu-preset-`` prefix.
   If you wish to define your own repeat logic, set:
   ``repeat-fu-backend`` P-list directly.

   By convention, the following rules are followed for bundled presets.

   - Any selection that uses the mouse cursor causes selection
     commands to be ignored as they can't be repeated reliably.
   - Undo/redo commands won't be handled as a new edit to be repeated.
     This means it's possible to undo the ``repeat-fu-execute`` and repeat the
     action at a different location instead of repeating the undo.

``repeat-fu-global-mode``: ``t``
   When true, ``repeat-fu`` shares its command buffer between buffers.

``repeat-fu-last-used-on-quit``: ``t``
   When the last command is ``keyboard-quit``, repeat the last used macro.
   This allows any edit to be ignored so the last repeated action can be reused.

   This can be useful if an edit is made by accident.

``repeat-fu-buffer-size``: ``512``
   Maximum number of steps to store.
   When nil, all commands are stored,
   the ``repeat-fu-backend`` is responsible for ensuring the buffer doesn't expand indefinitely.


Commands
--------

``(repeat-fu-execute ARG)``
   Execute stored commands.
   The prefix argument ARG serves as a repeat count.

``(repeat-fu-copy-to-last-kbd-macro)``
   Copy the current ``repeat-fu`` command buffer to the ``last-kbd-macro`` variable.
   Then it can be called with ``call-last-kbd-macro``, named with
   ``name-last-kbd-macro``, or saved for later use with
   ``insert-kbd-macro``.


Functions
---------

``(repeat-fu-declare SYMBOLS &rest PLIST)``
   Support for controlling how ``repeat-fu`` handles commands.

   SYMBOLS may be a symbol or list of symbols, matching command names.

   The PLIST must only contain the following keys.

   ``:skip``
      When non-nil, the command is ignored by ``repeat-fu`` entirely.

      By default, ``save-buffer`` uses this so repeating an action never saves.
   ``:skip-active``
      When non-nil, the command won't include the active-region
      when one of these functions was used to create it.

      By default, ``mouse-set-region`` uses this so repeating an action
      doesn't attempt to replay the mouse-drag used for selection.
   ``:skip-change``
      When non-nil, commands that change the buffer will be skipped
      when detecting commands to be repeated.

      This is used for ``undo`` (and related undo commands),
      so it's possible to undo ``repeat-fu-execute`` and repeat the action elsewhere
      without the undo action being repeated.

      This is different from ``:skip`` since undo *can* be repeated
      when part of multiple edits in ``insert`` mode - for presets that support this.

   The values should be t, other values such as function calls
   to make these checks conditional may be supported in the future.

.. END VARIABLES


Bundled Presets
---------------

These bundled presets can be used by setting ``repeat-fu-preset``.

.. BEGIN PRESETS


``'meep``
   Preset for `MEEP modal editing <https://codeberg.org/ideasman42/emacs-meep>`__.

   This has matching functionality to the Meow preset.

``'meow``
   Preset for `Meow modal editing <https://github.com/meow-edit/meow>`__.

   A preset written for meow which repeats the last edit
   along with selection commands preceding the edit.

   Changes made in insert mode are considered a single edit.
   When entering insert mode changes the buffer (typically `meow-change')
   the events that constructed the selection are included.

   This means the following is a single, repeatable action:

   - Mark 3 words (`meow-next-word', `meow-expand-3').
   - Change them (`meow-change', "replacement text").
   - Leave insert mode (`meow-insert-exit').

   The cursor can be moved elsewhere and `repeat-fu-execute'
   will replace 3 words at the new location.

``'multi``
   Preset for Emacs to repeat multiple consecutive commands.

   Repeats the last changing edits
   along with any preceding prefix arguments.
   Multiple calls to the same command are grouped
   so you can for example, repeat text insertion elsewhere.

   Events creating a selection (active-region)
   leading up to the edit will also be repeated
   unless repeat runs with an active-region
   in which case they will be skipped.

``'single``
   Preset for Emacs (single) repeat.

   This is a very simple form of repeating.

   - Find the last change.
   - Include any prefix commands.

.. END PRESETS


Other Packages
==============

`dot-mode <https://melpa.org/#/dot-mode>`__
   Dot-mode is uses the same method as Repeat-fu,
   the main difference is it repeats all preceding commands that change the buffer
   with an optional prefix command.

   For non-modal editing setups, the difference between this and Repeat-fu is not so large,
   (it matches the ``'multi`` preset).
   For modal editing the difference is more significant, allowing the "edit" to be repeated to
   include motion/selection commands.

   Note that Repeat-fu was originally based on Dot-mode, however it diverged enough
   that it didn't seem practical to attempt to integrate back into the original package.
Various others (``defrepeater``, ``easy-repeat``, ``repeater``)
   Are lightweight packages that only support repeating single commands.
