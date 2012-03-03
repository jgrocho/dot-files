# Install some nice colors for ls.
__dircolors=`type -p dircolors`
[[ -x "${__dircolors}" && -f ~/.dircolors && -r ~/.dircolors ]] \
  && eval `${__dircolors} ~/.dircolors`
unset __dircolors
