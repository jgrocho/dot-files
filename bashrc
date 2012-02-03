# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

[[ -d ~/.cabal/bin ]] && export PATH=$PATH:~/.cabal/bin

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm" # Load RVM function
[[ -r $rvm_path/scripts/completion ]] && . $rvm_path/scripts/completion

[[ -r /usr/local/bin/virtualenvwrapper.sh ]] && . /usr/local/bin/virtualenvwrapper.sh

hg_ps1() {
	return 0
	if builtin hash hg 2>&- && hg prompt 2>&-; then
		hg prompt "{ on \[\e[0;36m\]{branch} \[\e[0;34m\]☿\[\e[0m\]}" 2>/dev/null
	fi
}

rvm_ps1() {
    if [[ -x ~/.rvm/bin/rvm-prompt ]]; then
        local system=$(~/.rvm/bin/rvm-prompt s)
        local interp=$(~/.rvm/bin/rvm-prompt i)
        if [[ ! -n $system ]]; then
            echo -n " (\[\e[0;31m\]♦ "
            case $interp in
                ruby) echo -n "$(~/.rvm/bin/rvm-prompt v g)";;
                *)    echo -n "$(~/.rvm/bin/rvm-prompt i v g)";;
            esac
            echo -n "\[\e[0m\])"
        fi
    fi
}

virtenv_ps1() {
    if [ -n "$VIRTUALENVWRAPPER_VIRTUALENV" ]; then
        if [ -n "$VIRTUAL_ENV" ]; then
            echo " (\[\e[0;32m\]ᴤ $(basename $VIRTUAL_ENV)\[\e[0m\])"
        fi
    fi
}

# add some fun features to the prompt
PROMPT_COMMAND='pc_ret=$?;\
		if [[ $pc_ret -eq 0 ]]; then\
			pc_color="\e[0;32m";\
		else\
			pc_color="\e[0;31m";\
		fi;\
		pc_host="";\
		if [[ -n $SSH_CLIENT ]]; then\
			pc_host=" at \[\e[1;35m\]\h\[\e[0m\]";\
		fi;\
		pc_dvcs="$(__git_ps1 " on \[\e[0;36m\]%s \[\e[0;32m\]∓\[\e[0m\]")$(hg_ps1)";\
		pc_rvm="$(rvm_ps1)";\
		pc_virtenv="$(virtenv_ps1)";\
		PS1="\[${pc_color}\]${pc_ret}\[\e[0m\] ${debian_chroot:+($debian_chroot)}\[\e[1;31m\]\u\[\e[0m\]${pc_host} [\w]\n${pc_rvm}${pc_virtenv}${pc_dvcs} \$ ";'
unset pc_ret pc_color pc_host pc_dvcs pc_rvm pc_virtenv

[[ -r ~/.dir_colors ]] && eval `dircolors ~/.dir_colors`

# set editor
if builtin hash vim 2>&-; then
	export EDITOR=vim
	export VISUAL=$EDITOR
fi
