#!/bin/sh -x

EMACSDIR=$HOME/.emacs.d
SLIME="/usr/local/share/emacs/23.1/site-lisp/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"

cp xmodmaprc ~/.xmodmaprc
cp dot.emacs ~/.emacs
cp emacs-custom.el ${EMACSDIR}

# Compile the library
emacs --batch -f batch-byte-compile lib/*.el
cp -R lib "$EMACSDIR"
rm -f lib/*.elc

# Generate an image containing the Swank server for SBCL.
sbcl <<EOF
(load "${SLIME}/swank-loader.lisp")
(swank-loader:dump-image "${CORE}")
EOF
