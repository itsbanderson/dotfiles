#!/bin/sh

# Set background color
xsetroot -solid midnightblue

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Hide mouse when not in use
unclutter -idle 1 &
# Load wallpaper
xloadimage -fullscreen -border black -onroot /home/ben/media/images/wallpaper/misty_bw_mountains.jpg
# Auto-lock after set amount of time
xautolock -time 10 -locker slock &
# Let's Go!
exec xmonad
