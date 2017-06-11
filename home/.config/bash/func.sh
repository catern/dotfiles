#### aliases and functions
alias g='git'
alias atch='tmux attach -d'
alias topdf='soffice --headless --convert-to pdf'

# print a nice ruler
function hr() { printf '=%.0s' $(seq $COLUMNS); printf '\n'; }

# output the parent directory of the canonical representation of a file
function dirnamec() { dirname "$(readlink -f $*)"; }

# go to the parent directory of a file (including a symlink)
function cdl() { cd "$(dirnamec "$*")"; }

function magit() { emacsclient -e -qt "(magit-status \"$(pwd)\")"; }

function nme() { emacsclient -e -qc "(notmuch)"; }

function nanos() {
    date -d@$(echo $1 | sed 's/.\{9\}$/.&/') --rfc-3339=ns | sed 's/.\{12\}$//'
}
