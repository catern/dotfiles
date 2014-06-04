#!/bin/bash
run_if_exists() {
    if [ -f "$1" ]
    then 
        $* &
    fi
}

# needed for virt-manager
run_if_exists /usr/lib/lxpolkit/lxpolkit
# a notification daemon
run_if_exists /usr/bin/dunst
# start the urxvt server
run_if_exists /usr/bin/urxvtd -q -o -f

