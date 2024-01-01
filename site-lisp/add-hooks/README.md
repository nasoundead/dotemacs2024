# `add-hooks`
[![Build Status](https://travis-ci.org/nickmccurdy/add-hooks.svg?branch=master)](https://travis-ci.org/nickmccurdy/add-hooks)
[![MELPA](https://melpa.org/packages/add-hooks-badge.svg)](https://melpa.org/#/add-hooks)
[![MELPA Stable](https://stable.melpa.org/packages/add-hooks-badge.svg)](https://stable.melpa.org/#/add-hooks)

Emacs functions for setting multiple hooks.

Typically, you would need to call `add-hook` multiple times with
similar arguments to declare multiple functions for one hook, or
vice versa.  `add-hooks-pair` is a variant that takes multiple
hooks or functions that apply to each other.  The `add-hooks`
function tidies up duplicate hook and function names further into a
single declarative call (inspired by the
[`bind-key`](https://github.com/jwiegley/use-package/blob/master/bind-key.el)
 package).

If you're using [`use-package`](https://github.com/jwiegley/use-package), use its [`:hook`](https://github.com/jwiegley/use-package#hooks) keyword instead, which has a
similar syntax while establishing autoload definitions and supporting
deferred loading.

## Documentation

### `(add-hooks-pair HOOKS FUNCTIONS)`
Call `add-hook` for each combined pair of items in *HOOKS* and *FUNCTIONS*.

*HOOKS* can be a symbol or a list of symbols representing hook
variables (the `-hook` suffix is implied).  *FUNCTIONS* can be a
symbol, a lambda, or a list of either representing hook
functions.  If lists are used, a function can be added to
multiple hooks and/or multiple functions can be added to a hook.

#### Example
```emacs
ELISP> (add-hooks-pair '(css-mode sgml-mode) 'emmet-mode)
nil
ELISP> css-mode-hook
(emmet-mode)
ELISP> sgml-mode-hook
(emmet-mode)
```

### `(add-hooks PAIRS)`
Call `add-hooks-pair` on each cons pair in *PAIRS*.

Each pair has a `car` for setting hooks and a `cdr` for setting
functions to add to those hooks.  Pair values are passed to the
*HOOKS* and *FUNCTIONS* arguments of `add-hooks-pair`, respectively.

#### Usage
```emacs
(add-hooks ((HOOKS . FUNCTIONS)...))
```

#### Example
```emacs
ELISP> (add-hooks '(((css-mode sgml-mode) . emmet-mode)))
nil
ELISP> css-mode-hook
(emmet-mode)
ELISP> sgml-mode-hook
(emmet-mode)
```
