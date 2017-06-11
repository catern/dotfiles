# this file is sourced when opening a login interactive shell;
# .bashrc is sourced when opening a non-login interactive shell;
# since every non-login interactive shell was started (possibly indirectly) by a login interactive shell,
# we can set environment variables here and they will be inherited.
# This avoids, e.g., prepending the same thing to the PATH multiple time.

# Unfortunately, most terminal emulators start login shells by
# default, even if a login shell has already run, so the problem of
# setting environment variables twice is not completely solved, and we
# will have to be aware of that.

# Ideally, environment variables would be set exactly once when
# starting a new session, by something like systemd or pam_environment
# or the desktop environment, and we could rely on them not being
# started up again within themselves, unless we explicitly wanted
# that.  But those things have never really taken off, so they aren't
# reliable, and furthermore they aren't as flexible as doing it with
# the shell.

# source profile configuration present in $HOME/.config/profile
# these will set environment variables and do anything else that
# should be inherited by the rest of the session.
profile_dir=$HOME/.config/profile
if [[ -d $profile_dir ]]; then
    for file in $profile_dir/*; do
        source "$file"
    done
fi

# we don't want to maintain two interactive configurations, so always source .bashrc
if [ -f ~/.bashrc ]; then
   source ~/.bashrc
fi
