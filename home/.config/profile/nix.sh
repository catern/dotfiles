# Detect if we have a Nix installation, and source it if so.
multiuser_nix_sh=/usr/local/etc/profile.d/nix.sh
singleuser_nix_sh=$HOME/.nix-profile/etc/profile.d/nix.sh
if test -e $multiuser_nix_sh; then
    export NIX_REMOTE=daemon
    source $multiuser_nix_sh
elif test -e $singleuser_nix_sh; then
    source $singleuser_nix_sh
fi
