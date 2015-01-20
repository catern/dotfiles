# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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

# to add additional commands to my PROMPT_COMMAND by appending '|| command', it needs to be guaranteed-initialized
if [ -z "$PROMPT_COMMAND" ];
then
    export PROMPT_COMMAND="eval";
fi

# include any supplementary configs present in .config/bash
# this simplifies management of a large .bashrc
MY_BASH_LOCAL_CONFIG_DIR=~/.config/bash
if [[ -d $MY_BASH_LOCAL_CONFIG_DIR ]]; then 
    for file in $MY_BASH_LOCAL_CONFIG_DIR/*; do 
        source "$file" 
    done
fi
# give a hoot, don't pollute (the environment); unset this variable now that we're done
unset MY_BASH_LOCAL_CONFIG_DIR

# use homeshick
# https://github.com/andsens/homeshick
source "$HOME/.homesick/repos/homeshick/homeshick.sh"

