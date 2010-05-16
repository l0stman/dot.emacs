#!/bin/sh -x

emacs_version ()
{
    emacs --version | head -1 | sed -E 's/GNU Emacs ([0-9]+\.[0-9]).*/\1/'
}

EMACSDIR=$HOME/.emacs.d
SLIME="/usr/local/share/emacs/`emacs_version`/site-lisp/slime"
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
