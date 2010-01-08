#!/bin/sh -x

EMACSDIR=$HOME/.emacs.d
SLIME="/usr/local/share/emacs/23.0.95/site-lisp/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"

cp xmodmaprc ~/.xmodmaprc
cp dot.emacs ~/.emacs
cp -R lib "$EMACSDIR"

# Generate an image containing the Swank server.

sbcl <<EOF
(load "${SLIME}/swank-loader.lisp")
(swank-loader:dump-image "${CORE}")
EOF
