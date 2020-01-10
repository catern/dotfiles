# If not running interactively, don't do anything
[[ $- = *i* ]] || return 0

if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# include any supplementary configs present in .config/bash
# this simplifies management of a large .bashrc
local_config_dir=~/.config/bash
if [[ -d $local_config_dir ]]; then
    for file in $local_config_dir/*.sh; do
        source "$file"
    done
fi

# use homeshick
# https://github.com/andsens/homeshick
source "$HOME/.homesick/repos/homeshick/homeshick.sh"

