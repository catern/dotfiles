# wrap ranger so it autocds

function ranger-cd {
tempfile="$(mktemp)"
/usr/bin/ranger --choosedir="$tempfile" "${@:-$(pwd)}"
test -f "$tempfile" &&
    if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
        cd -- "$(cat "$tempfile")"
    fi
    rm -f -- "$tempfile"
}

alias ranger='ranger-cd'
