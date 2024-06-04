#!/bin/sh
EMACS_INIT_FILE="~/.emacs.d/init.el"
emacs -Q --batch -l $EMACS_INIT_FILE --eval "(message \"Emacs config loaded successfully\")"
