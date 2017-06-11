### shell and terminal settings
# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# throw an error if we do somecommand > some_already_existing_file
# that would wipe out the current contents of the file, but it could be a typo
# override with somecommand >| some_already_existing_file
#set -o noclobber

# disable flow control so we can use Ctrl-Q and Ctrl-S
stty -ixon
stty -ixoff

# ** does recursive matching
shopt -s globstar
