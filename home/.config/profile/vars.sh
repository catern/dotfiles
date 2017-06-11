# put go bin/ pkg/ src/ in .local
export GOPATH=$HOME/.local
# make any executables in .local/bin available for command-line use
# and override system executables
export PATH=~/.local/bin:$PATH
export MANPATH=~/.local/share/man:$MANPATH
export EDITOR=emx
