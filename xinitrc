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
type -p VBoxClient-all && (VBoxClient-all &)

# Start compositing manager
type -p xcompmgr && (xcompmgr -c &)

# Load a background image
[[ -f ~/.fehbg && -r ~/.fehbg ]] && (sh ~/.fehbg &)

# Some things are not reasonable to be globally shared, i.e. a WM.
# Therefore those settings are placed in .xinitrc.local. If that file
# doesn't exist we try a global default (/etc/xinitrc) and, failing that
# we try some defaults
if [[ -f ~/.xinitrc.local && -x ~/.xinitrc.local ]]; then
  . ~/.xinitrc.local
else if [[ -f /etc/X11/xinit/xinitrc && -x /etc/X11/xinit/xinitrc ]]; then
  . /etc/X11/xinit/xinitrc
else
  twm &
  exec xterm -login
fi
