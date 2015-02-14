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
systemctl --user start xresources.service

# Set the cursor (XMonad does set one by default)
xsetroot -cursor_name left_ptr

# Start the VirtualBox guest additons
hash VBoxClient-all 2>/dev/null && { VBoxClient-all & }

# Load a background image
systemctl --user start fehbg.service

# Start a urxvt daemon
systemctl --user start urxvtd.service

# Start mouse hider
systemctl --user start unclutter.service

# Start arbtt to collect statistics about window usage
[[ -f ~/.arbtt/.virthualenv/bin/activate ]] \
  && (source ~/.arbtt/.virthualenv/bin/activate \
    && arbtt-capture &)

# Start redshift to adjust the monitor color
systemctl --user start redshift@0.service
systemctl --user start redshift@1.service

systemctl --user start xautolock.service

# xmonad is not a reparenting window manager, tell java
export _JAVA_AWT_WM_NONREPARENTING=1

# Start xmonad from the user's binary just in case anything ever happens to
# xmonad (e.g. is uninstalled)
exec ~/bin/xmonad
