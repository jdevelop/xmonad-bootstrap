Some simple XMonad config to start with
=======

It's rather complex to adopt the XMonad configurations for newcomers, so this is yet another attempt to simplify the process.

Requirements
-------
* GHC 7.* +
* XMonad 0.10.* +
* [dzen2](https://github.com/robm/dzen) for status bars
* [screen](https://www.gnu.org/software/screen/) for good terminal stack
* [rxvt](http://sourceforge.net/projects/rxvt/) the best terminal ever, try unicode version
* [cabal](http://www.haskell.org/cabal/download.html) package management for haskell

Installation
-----

---

    cabal update
    cabal install xmonad xmonad-contrib directory
    make
    mkdir ~/.xmonad
    cp xmonad-x86_64-linux ~/.xmonad
    cp .screenrc ~/
    cp .xinitrc ~/
    cp -vR xmonad-bin ~/

---
