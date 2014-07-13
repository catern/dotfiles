# vte terminals only set COLORTERM so some things don't see them as
# being able to take 256colors

if [ "$COLORTERM" == "gnome-terminal" ] || [ "$COLORTERM" == "xfce4-terminal" ]
then
    TERM=xterm-256color
fi
