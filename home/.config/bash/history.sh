# append to the history file, don't overwrite it
shopt -s histappend
# make a large history of 1MB
export HISTSIZE=1000000
# the difference between HISTSIZE and HISTFILESIZE is pointless
# define HISTFILESIZE to have the value that HISTSIZE currently has
# this will not adjust HISTFILESIZE if HISTSIZE changes, it's just a more DRY way to say
# export HISTFILESIZE=1000000
export HISTFILESIZE=$HISTSIZE
# ignore duplicate commands, and erase them from history if they somehow get in
export HISTCONTROL=ignoredups:erasedups

# a bash function to write my bash history to file
_bash_history_sync() {
  # append current history to file
  builtin history -a
  # the latter two are needed only if you 
  # clear history
  # builtin history -c
  # reload from file
  # builtin history -r
}

# history() {
#   _bash_history_sync
#   builtin history "$@"
# }

# execute the history syncing function every time a command finishes (and we see the prompt)
export PROMPT_COMMAND="_bash_history_sync"
