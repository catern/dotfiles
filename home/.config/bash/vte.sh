# For a long time VTE set TERM=xterm by default, so terminal
# applications avoided using 256colors. Detect VTE and set an
# appropriate TERM value.
# This is fixed upstream now, but not yet packaged.
if [ -n "$VTE_VERSION" ]
then
    TERM=xterm-256color
fi
