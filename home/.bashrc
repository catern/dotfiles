# If not running interactively, don't do anything
[[ $- = *i* ]] || return 0

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

### Misc
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

# include any supplementary configs present in .config/bash
# this simplifies management of a large .bashrc
local_config_dir=~/.config/bash
if [[ -d $local_config_dir ]]; then
    for file in $local_config_dir/*; do
        source "$file"
    done
fi

# use homeshick
# https://github.com/andsens/homeshick
source "$HOME/.homesick/repos/homeshick/homeshick.sh"

