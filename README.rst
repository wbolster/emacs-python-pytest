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
a dispatcher popup menu built using `magit-popup`__,
which gives a look and feel
similar to the fantastic `magit`__ package.

__ https://magit.vc/manual/magit-popup.html
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

  Switches
  -c color (--color)
  -d debug on error (--pdb)
  -f failed first (--failed-first)
  -l show locals (--showlocals)
  -q quiet (--quiet)
  -s do not capture output (--capture=no)
  -t do not cut tracebacks (--full-trace)
  -v verbose (--verbose)
  -x exit after first failure (--exitfirst)

  Options
  =k only names matching expression (-k)
  =m only marks matching expression (-m)
  =t traceback style (--tb=)
  =x exit after N failures or errors (--maxfail=)

  Run tests
  t Test all            x Test last-failed

  Run tests for current context
  f Test file           F Test this file
  d Test def/class      D This def/class

  Repeat tests
  r Repeat last test run

  Common Commands
  C-c C-c Set defaults       C-h i View popup manual    C-t Toggle this section
  C-x C-s Save defaults      ?     Popup help prefix    C-g Abort


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
named ``python-pytest-popup``.
it is a good idea to create a dedicated keybinding for this command,
but it can also be run manually:

::

  M-x python-pytest-popup

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

to automatically set paths and ‘activate’ a ``virtualenv``,
use `direnv`__, `emacs-direnv`__, and `exec-path-from-shell`__.
to make rerunning tests work correctly,
even after editing files from other projects in the mean time,
make the output buffers ``direnv`` aware:

.. code-block:: elisp

  (use-package python-pytest
    :config
    (add-to-list 'direnv-non-file-modes 'python-pytest-mode))


__ https://direnv.net/
__ https://github.com/wbolster/emacs-direnv
__ https://github.com/purcell/exec-path-from-shell

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

when non-nil only ``test_foo()`` will match, and nothing else,
by invoking ``pytest test_utils.py::test_foo``.

configuration
=============

settings
--------

the behaviour of this package can be tweaked
by customising a few `defcustom` variables.
use the ``customize`` interface to explore those::

  M-x customize-group RET python-pytest RET

to set those permanently, use something like this in ``init.el``:

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
see the `magit-popup`__ manual for more information.

__ https://magit.vc/manual/magit-popup.html

as an example, this will add a ``-z`` switch that,
when enabled, will invoke ``pytest ---zzz``:

.. code-block:: elisp

  (use-package python-pytest
   :config
   (magit-define-popup-switch 'python-pytest-popup
    ?z "Custom flag" "--zzz"))


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

* 0.1.0 (2018-02)

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
