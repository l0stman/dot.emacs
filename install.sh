#!/bin/sh -x

EMACSDIR=$HOME/.emacs.d
SLIME="$HOME/hacks/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"
DEVCORE="${EMACSDIR}/sbcl-devel.core-with-swank"

usage()
{
    echo "Usage: `basename $0` [T60 | desktop | studio] [ubuntu | freebsd]"
    exit 1
}

# Generate an image containing the Swank server for SBCL.
savecore()
{
    local sbcl=$1
    local core=$2
    $sbcl <<EOF
(load "${SLIME}/swank-loader.lisp")
(swank-loader:dump-image "${core}")
EOF
}

if [ $# -ne 2 ]
then
    usage
fi

case $1 in
    T60|desktop|studio) XMODMAPRC=xmodmaprc.$1;;
    *) usage;;
esac

case $2 in
    ubuntu) OSPARAMS="ubuntu-params.el"
            SBCL="/usr/bin/sbcl";;
    freebsd) OSPARAMS="freebsd-params.el"
             SBCL="/usr/local/bin/sbcl";;
    *) usage;;
esac

if [ ! -d ${EMACSDIR} ]
then
    mkdir ${EMACSDIR}
fi
cp $XMODMAPRC ~/.Xmodmap
cp dot.emacs ~/.emacs
cp emacs-custom.el ${EMACSDIR}
cp dream-theme.el ${EMACSDIR}
cp $OSPARAMS lib/os-params.el

# Compile the library
emacs --batch -f batch-byte-compile lib/*.el
cp -R lib "$EMACSDIR"
rm -f lib/*.elc

savecore $SBCL $CORE
savecore $HOME/bin/sbcl $DEVCORE
