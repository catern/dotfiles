# This file brought to you by http://mywiki.wooledge.org/BashFAQ/088
# have an extremely large on-disk history of 400MB
HISTFILESIZE=$((400 * 1000 * 1000))
# have a large in-memory history of 5MB
HISTSIZE=$((5 * 1000 * 1000))
# append to the history file, don't overwrite it
shopt -s histappend
# don't ignore any commands in my history
export HISTCONTROL=
# log time command was run (but don't print when running "history")
export HISTTIMEFORMAT=""
# append to the history after every command
PROMPT_COMMAND="${PROMPT_COMMAND:-:} ; history -a"


