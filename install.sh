#!/bin/sh -x

EMACSDIR=$HOME/.emacs.d
SLIME="$HOME/hacks/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"
DEVCORE="${EMACSDIR}/sbcl-devel.core-with-swank"

usage()
{
    echo "Usage: `basename $0` [T60 | desktop | studio | W530] \
[ubuntu | freebsd]"
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
    T60|desktop|studio|W530) XMODMAPRC=xmodmaprc.$1;;
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
if [ $2 = "ubuntu" ]
then
    cp LoadUserXmap.py ~/bin
fi

# Compile the library
cp -R lib "$EMACSDIR"
emacs --batch -L "$EMACSDIR"/lib -f batch-byte-compile "$EMACSDIR"/lib/*.el

savecore $SBCL $CORE
savecore $HOME/bin/sbcl $DEVCORE
