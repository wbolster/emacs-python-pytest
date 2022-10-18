================
python-pytest.el
================

.. image:: https://melpa.org/packages/python-pytest-badge.svg
   :alt: melpa badge

.. image:: https://stable.melpa.org/packages/python-pytest-badge.svg
   :alt: melpa stable badge


overview
========

``python-pytest.el`` is an `emacs`__ package
to integrate the python `pytest`__ test runner.

__ https://www.gnu.org/software/emacs/
__ https://pytest.org/

most functionality can be used via
a dispatcher popup menu built using `transient`__,
which gives a look and feel
similar to the fantastic `magit`__ package.

__ https://magit.vc/manual/transient
__ https://magit.vc/


features
========

``python-pytest.el`` offers these awesome features:

* various commands with ‘do what i mean’ (dwim) behaviour,
  using heuristics to automatically detect test files and test
  functions:

  * run all tests

  * rerun previous failures

  * repeat the last invocation

  * run only tests for the current python (test) module

  * run only tests for the current (test) function

* easy way to change common switches and options, e.g.
  toggling output capture, failing after the first error,
  and so on.

* edit the automatically generated command line before executing,
  by invoking commands with a prefix argument (``C-u``).

* basic debugger integration using the pdb tracking support
  from the built-in `python-mode` package,
  which will automatically open source files at the right location.

* work simultaneously on multiple python projects.
  each project will use its own dedicated pytest output buffer.

* various customisation options, e.g. to change whether
  a generated command line should be shown for editing by default.

* hooks that get run before and after running pytest,
  which can be used to add custom behaviour.


screenshot
==========

::

  Output
   -c color (--color)
   -q quiet (--quiet)
   -s no output capture (--capture=no)
   -v verbosity ([--verbose|--verbose --verbose])

  Selection, filtering, ordering
   -k only names matching expression (-k=)      --dm run doctests (--doctest-modules)
   -m only marks matching expression (-m=)      --nf new first (--new-first)
                                                --sw stepwise (--stepwise)

  Failures, errors, debugging
   -l show locals (--showlocals)                --ff failed first (--failed-first)
   -p debug on error (--pdb)                    --ft full tracebacks (--full-trace)
   -x exit after first failure (--exitfirst)    --mf exit after N failures or errors (--maxfail=10)
                                                --rx run xfail tests (--runxfail)
                                                --tb traceback style (--tb=)
                                                --tr debug on each test (--trace)

  Run tests
   t all    r repeat         f file (dwim)    m files          d def/class (dwim)
            x last failed    F file (this)    M directories    D def/class (this)


installation
============

``python-pytest.el`` is available from `melpa`__.

__ https://melpa.org/#/python-pytest

with ``use-package``:

.. code-block:: elisp

  (use-package python-pytest)

install manually::

  M-x package-install RET python-pytest RET

note that ``python-pytest.el`` uses `projectile`__
for some of its features, e.g. finding associated test files.
this package is intended to work correctly
even without any ``projectile`` configuration,
since it will likely do the right thing
if a project has a conventional layout.

__ https://github.com/bbatsov/projectile


usage
=====

basics
------

the typical usage pattern is to invoke the popup menu,
named ``python-pytest-dispatch``.
it is a good idea to create a dedicated keybinding for this command,
but it can also be run manually:

::

  M-x python-pytest-dispatch

this shows a dispatcher menu.
change some switches and options,
then run one of the actions.

a dedicated pytest ``comint`` buffer will open,
showing the output in real time,
and allowing interaction with debuggers.

using the correct environment
-----------------------------

this package ultimately invokes ``pytest``.
``python-pytest.el`` does *not* guess execution environments,
so emacs needs to use the right ``exec-path``,
taking into account python virtual environments, and so on.

to manage the execution environment, consider using `direnv`__:
it can change (and revert) paths and environment variables,
simply by switching to a project directory,
making it perfect for automatically ‘activating’ a ``virtualenv``.
use `emacs-direnv`__ and possibly `exec-path-from-shell`__
to achieve the same inside emacs.

__ https://direnv.net/
__ https://github.com/wbolster/emacs-direnv
__ https://github.com/purcell/exec-path-from-shell

working in a monorepo
---------------------

by default, ``pytest`` is run from the project root directory. if
your package is not at the root of your repository, ``pytest`` might
not find your modules.

a workaround is to add the the package root to ``PYTHONPATH`` before
running the tests. this can be found by adding a dummy file in the package
root. the following hook looks for a ``.pyroot`` file in parent directories.
if found, it adds the directory of the file to ``PYTHONPATH``.

.. code-block:: elisp

  (add-hook 'python-mode-hook
            (lambda ()
              (when-let ((r (locate-dominating-file default-directory ".pyroot")))
                (setq python-pytest-executable
                      (concat "PYTHONPATH=" r " " "pytest")))))


editing and repeating
---------------------

to edit the command line before running it,
use a prefix argument before calling the action,
e.g.type ``C-u t`` instead of just ``t`` in the popup menu.

when the popup menu itself is invoked with a prefix argument,
this will run ``python-pytest-repeat`` to rerun pytest.
this means a single key binding can be used for both
an initial run (via the popup), and for repeated calls.
this is great for quick ‘edit, test, edit, test` cycles.

available commands
------------------

the available commands are:

- ``python-pytest``
- ``python-pytest-file``
- ``python-pytest-file-dwim``
- ``python-pytest-files``
- ``python-pytest-function``
- ``python-pytest-function-dwim``
- ``python-pytest-last-failed``
- ``python-pytest-repeat``

all of these are available via the popup menu,
but can also be executed directly (or bound to a key).


heuristics
==========

this package uses a few heuristics for its
‘do what i mean’ behaviour.

test file heuristics
--------------------

the ``python-pytest-file-dwim`` command tries to
do the right thing both when editing the actual code
and its associated test module.
for instance, when editing ``foo/bar.py``,
this will automatically detect ``tests/test_bar.py``
(thanks to the ``projectile`` package),
and only run the tests from that test module.

test function heuristics
------------------------

the ``python-pytest-function-dwim`` command
tries to run only tests related to the function
close to the cursor position
(‘point’ in emacs terminology).

when editing a test module, this runs
only a single test function,
namely the one currently being edited.

when editing the code itself,
things are more complicated.
this command will make a guess
to only run the right test functions.
the matching behaviour can be tweaked using
``python-pytest-strict-test-name-matching``
(see configuration below).

by default, the current function name will be used
as a pattern to match the corresponding tests.
for example, when editing ``foo()`` inside ``utils.py``,
this will match ``test_foo()`` as well as ``test_foo_xyz()``,
by invoking ``pytest test_utils.py -k test_foo``.
if a pattern was specified in the popup (the ``-k`` option),
it will try to make a combined pattern,
by invoking ``pytest test_utils.py -k 'test_foo and other_filter'``.

on the other hand,
when ``python-pytest-strict-test-name-matching`` is non-nil,
only ``test_foo()`` will match, and nothing else,
by invoking ``pytest test_utils.py::test_foo``.

configuration
=============

settings
--------

the behaviour of this package can be tweaked
by customising a few `defcustom` variables.
use the ``customize`` interface to explore those
(each will show a description and possible values)::

  M-x customize-group RET python-pytest RET

to set those permanently without using the customize interface,
use something like this in ``init.el``:

.. code-block:: elisp

  (use-package python-pytest
   :custom
   (python-pytest-confirm t))

the available variables are:

- ``python-pytest-confirm``

  whether to ask for confirmation (allowing editing) by default.
  this inverts the prefix argument (``C-u``) behaviour.

- ``python-pytest-strict-test-name-matching``

  Whether to require a strict match for the ‘test this function’ heuristic.

- ``python-pytest-executable``

  the name of the pytest executable (``pytest`` by default)

- ``python-pytest-unsaved-buffers-behavior``

  whether to ask whether unsaved buffers should be saved before
  running pytest. the check for unsaved buffers can be for only the
  current buffer, or for all project buffers, and those can be saved
  directly, or after confirmation. valid values: ``ask-all``,
  ``ask-current``, ``save-all``, ``save-current``, or ``nil``.

- ``python-pytest-setup-hook``,
  ``python-pytest-started-hook``, and
  ``python-pytest-finished-hook``

  hooks run before starting ``pytest``, after starting ``pytest``,
  and after ``pytest`` finished.

- ``python-pytest-buffer-name`` and ``python-pytest-project-name-in-buffer-name``

  the defaults result in ``*pytest*<project-name>``.

- ``python-pytest-pdb-track``

  whether to enable the pdb tracking support


extending the popup
-------------------

when using pytest plugins that provide extra switches,
it may be useful to integrate those into the popup.
see the `transient`__ manual for more information.

__ https://magit.vc/manual/transient

as an example, this will add a ``-z`` switch that,
when enabled, will invoke ``pytest --zzz``:

.. code-block:: elisp

  (use-package python-pytest
   :config
   ;; just an extra `-y' after the `-x' suffix
   (transient-append-suffix
     'python-pytest-dispatch
     "-x"
     '("-y" "The Y" "-y"))
   ;; group with `-z' after second from the last group,
   ;; that is before `Run tests'
   (transient-append-suffix
     'python-pytest-dispatch
     '(-2)
     ["My Z"
      ("-z" "The Z" "-z")]))

`transient` lets you save defaults you want for it.
just select all options on ``python-pytest-dispatch``
and then

- ``C-x C-s`` to save current settings as default and make
  them persistent,
- ``C-x s`` to save current settings as default for the
  current emacs session.


contributing
============

praise? complaints? bugs? questions? ideas?

please use the github issue tracker.


credits
=======

this package was created by wouter bolsterlee.
i am @wbolster on `github`__ and `twitter`__.

__ https://github.com/wbolster
__ https://twitter.com/wbolster


history
=======

note:
`melpa`__ automatically ships the latest code from the git ``main`` branch,
while `melpa stable`__ only contains tagged (released) versions.

__ https://melpa.org/
__ https://stable.melpa.org/

development branch
------------------

* …

3.3.0 (2022-10-18)
------------------

* add dispatch options for pytest-xdist
  (`#54 <https://github.com/wbolster/emacs-python-pytest/pull/54>`_)

* respect ``projectile-compilation-dir`` if it exists
  (`#59 <https://github.com/wbolster/emacs-python-pytest/pull/59>`_)

* Use ``read-shell-command`` instead of ``read-from-minibuffer``
  (`#60 <https://github.com/wbolster/emacs-python-pytest/pull/60>`_)

* add ``0`` as a valid argument that can be passed to ``-n``
  (`#61 <https://github.com/wbolster/emacs-python-pytest/pull/61>`_)

* switch to ``compilation-mode`` after pytest process finishes
  (`#62 <https://github.com/wbolster/emacs-python-pytest/pull/62>`_)

* fix saving of verbosity setting
  (`#64 <https://github.com/wbolster/emacs-python-pytest/pull/64>`_)

3.2.0 (2021-11-11)
------------------

* do not use melpa unstable versions in package-requires
  (`#52 <https://github.com/wbolster/emacs-python-pytest/issues/52>`_)

3.1.0 (2021-11-09)
------------------

* make python-pytest-files show all files if no test files are found
  (`#38 <https://github.com/wbolster/emacs-python-pytest/issues/38>`_)

* display buffer window before starting comint to fix size detection
  (`#48 <https://github.com/wbolster/emacs-python-pytest/issues/48>`_)

* correctly handle -m and -k flags
  (`#37 <https://github.com/wbolster/emacs-python-pytest/pull/37>`_)

* fix clearing test output buffer
  (`#15 <https://github.com/wbolster/emacs-python-pytest/pull/15>`_)

3.0.0 (2020-08-10)
------------------

* redesign the menu:
  use better groupings,
  use multi-column visual layout,
  add some more flags,
  make all flags start with either ``-`` or ``--``
  (mostly mimicking pytest flags)
  (`#28 <https://github.com/wbolster/emacs-python-pytest/pull/28>`_)

* add a ``python-pytest-directories`` command with interactive
  multi-directory selection
  (`#21 <https://github.com/wbolster/emacs-python-pytest/issues/21>`_,
  `#31 <https://github.com/wbolster/emacs-python-pytest/pull/31>`_)

2.0.0 (2020-08-04)
------------------

* switch to ``transient`` (``magit-popup`` replacement);
  the command for the menu is now ``python-pytest-dispatch``
  (`#18 <https://github.com/wbolster/emacs-python-pytest/issues/18>`_,
  `#26 <https://github.com/wbolster/emacs-python-pytest/pull/26>`_)

* add ``python-pytest-files`` command with interactive multi-file
  selection

* improve ``python-pytest-file-dwim`` heuristic for nested functions/classes

* make ``next-error`` and related-commands work

* add a ``-w`` shortcut for very verbose (``--verbose --verbose``)
  (`#24 <https://github.com/wbolster/emacs-python-pytest/pull/24>`_)

1.0.0 (2018-06-14)
------------------

* this package is useful for quite a few people.
  time to celebrate with a 1.x release!

* save (or ask to save) modified buffers before running pytest
  (`#4 <https://github.com/wbolster/emacs-python-pytest/issues/4>`_)

* put customizable variables in the right group

0.3.1 (2018–03-07)
------------------

* fix package version number for melpa stable

0.3.0 (2018–03-07)
------------------

* repopulate the popup with the previously used values
  when running ``python-pytest-dispatch`` from an output buffer.
  (`#3 <https://github.com/wbolster/emacs-python-pytest/issues/3>`_)

0.2.2 (2018-02-26)
------------------

* avoid ``-as->`` macro since the ``dash.el`` version
  currently on melpa stable does not have it.
  (`#2 <https://github.com/wbolster/emacs-python-pytest/issues/2>`_)

0.2.1 (2018-02-22)
------------------

* fix autoloading for ``python-pytest-popup`` command

0.2.0 (2018-02-19)
------------------

* now available from melpa
  (`#1 <https://github.com/wbolster/emacs-python-pytest/issues/1>`_)
* more docs
* various ‘dwim’ improvements
* renamed and added a few popup flags
* improved relative path handling
* improved hooks
* improved history
* better shell escaping
* remember current command in output buffer to make repeating work
* misc other tweaks and fixes

0.1.0 (2018-02-03)
------------------

* initial release


license
=======

*(this is the osi approved 3-clause "new bsd license".)*

copyright 2018 wouter bolsterlee

all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

* neither the name of the author nor the names of the contributors may be used
  to endorse or promote products derived from this software without specific
  prior written permission.

this software is provided by the copyright holders and contributors "as is" and
any express or implied warranties, including, but not limited to, the implied
warranties of merchantability and fitness for a particular purpose are
disclaimed. in no event shall the copyright holder or contributors be liable
for any direct, indirect, incidental, special, exemplary, or consequential
damages (including, but not limited to, procurement of substitute goods or
services; loss of use, data, or profits; or business interruption) however
caused and on any theory of liability, whether in contract, strict liability,
or tort (including negligence or otherwise) arising in any way out of the use
of this software, even if advised of the possibility of such damage.
