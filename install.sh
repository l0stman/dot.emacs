#!/bin/sh

EMACSDIR=$HOME/.emacs.d
SLIME="$HOME/hacks/slime"
CORE="${EMACSDIR}/sbcl.core-with-swank"
DEVCORE="${EMACSDIR}/sbcl-devel.core-with-swank"
XKBDIR=$HOME/.xkb/symbols

usage()
{
    echo "Usage: `basename $0` [T60 | desktop | studio | W530 | x250] \
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
    T60|desktop|studio|W530|x250) XMODMAPRC=xmodmaprc.$1;;
    *) usage;;
esac

case $2 in
    ubuntu) SBCL="/usr/bin/sbcl";;
    freebsd) SBCL="/usr/local/bin/sbcl";;
    *) usage;;
esac

install -d ${EMACSDIR}
cp dot.emacs ~/.emacs
install -m 644 emacs-custom.el dream-theme.el ${EMACSDIR}
if [ $2 = "ubuntu" ]
then
    mkdir -p ${XKBDIR}
    setxkbmap dvorak
    xmodmap ${XMODMAPRC}
    xkbcomp -xkm $DISPLAY ${XKBDIR}/dvorak.xkm
    install loadxkm ~/bin
else
    cp ${XMODMAPRC} ~/.Xmodmap
fi

lc=`xrdb -query | sort -b | join - Xresources | wc -l`
if [ $lc -eq 0 ]
then
    cat Xresources >> ~/.Xresources
    xrdb -merge Xresources
else
    echo "WARNING: Some emacs resources are already defined. Please merge \
'Xresources' manually with '~/.Xresources'".
fi

# Compile the library
cp -R lib "$EMACSDIR"
emacs --batch -L "$EMACSDIR"/lib -f batch-byte-compile "$EMACSDIR"/lib/*.el

savecore $SBCL $CORE
savecore $HOME/bin/sbcl $DEVCORE
