#!/bin/sh -x

EMACSDIR=$HOME/.emacs.d
SLIME="~/hacks/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"

if [ ! -d ${EMACSDIR} ]
then
	mkdir ${EMACSDIR}
fi
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
