#!/usr/bin/env bash
run_if_exists() {
    if [ -f "$1" ]
    then 
        $* &
    fi
}

xset r rate 270 40
