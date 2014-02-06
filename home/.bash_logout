# ~/.bash_logout: executed by bash(1) when login shell exits.

# when leaving the console clear the screen to increase privacy

if [ "$SHLVL" = 1 ]; then
    clear
fi

# invalidates cached sudo credentials
# commented because i'm not sure if that's portable
#sudo -k
