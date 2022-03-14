#!/usr/bin/env bash
source ~/.config/xinit/always.sh

touch ~/.config/xinit/local.sh
source ~/.config/xinit/local.sh

source ~/.config/xinit/tiling.sh
exec xmonad
#exec i3
#exec gnome-session
#exec startkde 
