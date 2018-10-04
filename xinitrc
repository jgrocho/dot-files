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

# Set the cursor (XMonad does not set one by default)
xsetroot -cursor_name left_ptr

# Set screen layout based on hostname
case "$HOSTNAME" in
  qubert) xrandr --output LVDS1 --auto --primary --output VGA1 --auto --above LVDS1 ;;
  theia)  xrandr --output HDMI-0 --auto --primary ;;
esac

declare units=(
  xresources
  fehbg
  urxvtd
  unclutter
  redshift@0
  redshift@1
  xautolock
)

systemctl --user start ${units[@]}

# Start the VirtualBox guest additons
hash VBoxClient-all 2>/dev/null && { VBoxClient-all & }

# Start arbtt to collect statistics about window usage
[[ -f ~/.arbtt/.virthualenv/bin/activate ]] \
  && (source ~/.arbtt/.virthualenv/bin/activate \
    && arbtt-capture &)

xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
xinput set-prop "TPPS/2 IBM TrackPoint" "Device Accel Constant Deceleration" 0.95

# xmonad is not a reparenting window manager, tell java
export _JAVA_AWT_WM_NONREPARENTING=1

# Start xmonad from the user's binary just in case anything ever happens to
# xmonad (e.g. is uninstalled)
exec ~/.local/bin/xmonad
