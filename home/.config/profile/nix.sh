# If we own /nix, this must be a single-user install
# This check is done automatically in newer Nix.
if test -O /nix/; then
    export NIX_REMOTE=local
else
    export NIX_REMOTE=daemon
fi
source $HOME/.nix-profile/etc/profile.d/nix.sh
