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
hash VBoxClient-all 2>/dev/null && { VBoxClient-all & }

# Load a background image
[[ -f ~/.fehbg && -r ~/.fehbg ]] && { sh ~/.fehbg & }

# Start a urxvt daemon
hash urxvtd 2>/dev/null && urxvtd -q -o -f

# Start mouse hider
hash unclutter 2>/dev/null && { unclutter -idle 3 & }

# Start arbtt to collect statistics about window usage
[[ -f ~/.arbtt/.virthualenv/bin/activate ]] \
  && (source ~/.arbtt/.virthualenv/bin/activate \
    && arbtt-capture &)

# Start redshift to adjust the monitor color
hash redshift 2>/dev/null && {
  redshift -m randr:crtc=0 &
  redshift -m randr:crtc=1 &
}

hash xautolock 2>/dev/null && hash lock 2>/dev/null && { xautolock -time 12 -locker lock & }

# Start xmonad from the user's binary just in case anything ever happens to
# xmonad (e.g. is uninstalled)
exec ~/.xmonad/xmonad-x86_64-linux
