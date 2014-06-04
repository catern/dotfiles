#!/bin/bash
run_if_exists() {
    if [ -f "$1" ]
    then 
        $* &
    fi
}

run_if_exists /usr/bin/xset r rate 270 40
