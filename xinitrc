#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [[ -d /etc/X11/xinit/xinitrc.d ]]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [[ -x "$f" ]] && . "$f"
  done
  unset f
fi

# Load up Xresources
[[ -f ~/.Xresources && -r ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set the cursor (XMonad does set one by default)
xsetroot -cursor_name left_ptr

# Start the VirtualBox guest additons
hash VBoxClient-all 2>/dev/null && (VBoxClient-all &)

# Start compositing manager
#hash xcompmgr 2>/dev/null && (xcompmgr -c &)

# Load a background image
[[ -f ~/.fehbg && -r ~/.fehbg ]] && (sh ~/.fehbg &)

# Start a urxvt daemon
hash urxvtd 2>/dev/null && urxvtd -q -o -f

# Start mouse hider
hash unclutter 2>/dev/null && (unclutter -idle 3 &)

# Some things are not reasonable to be globally shared, i.e. a WM.
# Therefore those settings are placed in .xinitrc.local. If that file
# doesn't exist we try a global default (/etc/xinitrc) and, failing that
# we try some defaults
if [[ -f ~/.xinitrc.local && -x ~/.xinitrc.local ]]; then
  . ~/.xinitrc.local
elif [[ -f /etc/X11/xinit/xinitrc && -x /etc/X11/xinit/xinitrc ]]; then
  . /etc/X11/xinit/xinitrc
else
  twm &
  exec xterm -login
fi
