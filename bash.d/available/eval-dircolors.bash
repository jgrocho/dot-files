# Install some nice colors for ls.
case $TERM in
  *256color*)
    hash dircolors 2>/dev/null && [[ -f ~/.dircolors && -r ~/.dircolors ]] \
      && eval `dircolors ~/.dircolors`
    ;;
esac
