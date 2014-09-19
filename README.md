These are my dotfiles. 
They are full of dot and files.

I use these dotfiles on all my machines; laptop, desktop, servers, shell accounts, everywhere.

So I avoid putting hardware-specific or system-specific configuration into this repo,
as I don't necessarily have root everywhere I use it.

## .config
I endeavour to move as many files into .config as I can,
to comply with the [XDG Base Directory Specification](http://www.freedesktop.org/software/systemd/man/file-hierarchy.html#Home%20Directory).

I also like the drop-in config directory approach that is increasingly common even at a system level in Linux;
for example, instead of editing /etc/sudoers directly, many distros now support dropping config snippets into the /etc/sudoers.d directory.
I try to emulate this in my personal configs.

### bash
The drop-in configs approach is most noticeable in the way I organize my bash configuration.

My .bashrc just does a few things, and then sources the contents of the .config/bash directory.
In .config/bash are different files setting up my prompt, aliases, etc.

This way if I have some local configuration that I don't want to push to this shared dotfiles repository,
I can just drop a local.sh file in .config/bash, and everything just works.

### xinit
I also try to keep my .xinitrc modular. 
xinitrc usually consists of a list of programs to run after X is started through startx.

I have several small scripts in the .config/xinit directory
which I can run from a local, not-committed .xinitrc.
Each script is just a list of commands to run,
and I put lines sourcing some selection of scripts in my .xinitrc.

Unfortunately, I run very few X programs on startup,
so the files in .config/xinit are very small.
They are so small that it would be a lot easier to just copy the commands into my local .xinitrc.
However, I want to store the commands that I run at X startup in git,
and have them accessible across all my machines.

A better solution would be running these commands as systemd --user services that depend on X.
Eventually systemd --user sessions will become more widely used to start and manage a user's DE, WM, and other X services.
At that point I will be able to just commit these X startup commands as .service files,
and systemctl --user enable/disable them locally.

### systemd
I use [systemd --user sessions](https://wiki.archlinux.org/index.php/Systemd/User)
to run the daemons or programs that I want always running in the background.
At least, those that don't depend on X, and therefore can be run even on shell servers or TTYs.
So this folder contains [systemd.unit(5)](http://www.freedesktop.org/software/systemd/man/systemd.unit.html) files
that run those programs as systemd services.

I don't do anything special with the orginzation of these unit files;
systemd already expects them to be laid out in a neat and orderly way.
