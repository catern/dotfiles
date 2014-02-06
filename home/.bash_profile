# this file is sourced when opening a login interactive shell;
# .bashrc is sourced when opening a non-login interactive shell;
# we don't want to maintain two configurations, so always source .bashrc
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
