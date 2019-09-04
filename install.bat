set HOME=%USERPROFILE%\AppData\Roaming
set EMACSDIR=%HOME%\.emacs.d

reg import emacs.reg
copy dot.emacs %HOME%\.emacs
copy emacs-custom.el %EMACSDIR%
copy dream-theme.el %EMACSDIR%
copy lib %EMACSDIR%
emacs --batch -L %EMACSDIR%\lib -f batch-byte-compile %EMACSDIR%\lib\*.el
