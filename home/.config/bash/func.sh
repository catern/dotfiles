# say something (and don't output a lot of text)
function say() { mplayer -really-quiet "http://translate.google.com/translate_tts?tl=en&q=$*"; } 

# print a nice ruler
function hr() { printf '=%.0s' $(seq $COLUMNS); }

#function ts() { xdg-open $(tracker-search --disable-color -l 1 --disable-snippets "$*" 2> /dev/null | sed -n 2p); }

# output the parent directory of the canonical representation of a file
function dirnamec() { dirname "$(readlink -f $*)"; }

# go to the parent directory of a file (including a symlink)
function cdl() { cd "$(dirnamec "$*")"; }


function magit() { emacsclient -e -qt "(magit-status \"$(pwd)\" 'switch-to-buffer)"; }
