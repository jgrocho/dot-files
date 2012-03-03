# Return the current git branch. Will return an empty string when
# outside a git repository. Also indicate dirty repos, untracked files
# and upstream divergence.
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM="auto"
function git_ps1() {
    builtin hash __git_ps1 2>&- \
      && __git_ps1 " on \[\e[0;36m\]%s \[\e[0;32m\]∓\[\e[0m\]"
}

# Return the current ruby version and gemset.
function rvm_ps1() {
    if [[ -n $rvm_path && -x ${rvm_path}/bin/rvm-prompt ]]; then
        local system=$(${rvm_path}/bin/rvm-prompt s)
        # Ignore system ruby. This gives a visual clue to switch.
        if [[ $system != "system" ]]; then
            # A nice red diamond.
            echo -n " (\[\e[0;31m\]◇ "
            case $(${rvm_path}/bin/rvm-prompt i) in
              # When using MRI/YARV Ruby, we don't need to be explicit.
              ruby) echo -n "$(${rvm_path}/bin/rvm-prompt v g)" ;;
              *)    echo -n "$(${rvm_path}/bin/rvm-prompt i v g)" ;;
            esac
            echo -n "\[\e[0m\])"
        fi
    fi
}

# Return the current Python virtual environment, if any.
function virtenv_ps1() {
    [[ -n $VIRTUALWRAPPER_VIRTUALENV && -n $VIRTUAL_ENV ]] \
      && echo " (\[\e[0;32m\] $(basename $VIRTUAL_ENV)\[\e\0m\])"
}

function virthask_ps1() {
    [[ -n $VIRTHUALENV && -n $VIRTHUALENV_NAME ]] \
      && echo " (\[\e[0;34m\]λ $VIRTHUALENV_NAME\[\e[0m\])"
}

# Set the prompt command, any functions called from here should be quick
# as they will be run each time the prompt is dispalyed.
function prompt_command() {
    # First save the return value of the last run command.
    EXIT=$?
    local pc_color pc_host pc_dvcs pc_dev_env
    # Pick a color based on that return value.
    if [[ $EXIT -eq 0 ]]; then
      pc_color="\e[0;32m"
    else
      pc_color="\e[0;31m"
    fi

    # Only show the host name for remote connections
    [[ -n $SSH_CLIENT ]] \
      && pc_host=" at \[\e[1;35m\]\h\[\e[0m\]" \
      || pc_host=""

    # Show any DVCS information. Assuming only one is active at a time,
    # ordering of these should not matter.
    pc_dvcs="$(git_ps1)"

    # Show current development environment status, e.g. rvm for Ruby,
    # virtualenv for Python, virthualenv for Haskell.
    # Ordering here is important, and spacing has to be handled by the
    # functions to prevent superfulous spacing.
    pc_dev_env="$(rvm_ps1)$(virthask_ps1)$(virtenv_ps1)"

    # Set the window title for xterm and (u)rxvt
    printf "\033]0;%s%s [%s]\007" "${USER}" "${pc_host}" "${PWD/#$HOME/~}"

    # Set the prompt.
    PS1="\[${pc_color}\]${EXIT}\[\e[0m\] \[\e[1;31m\]\u\[\e[0m\]${pc_host} [\w]
${pc_dev_env}${pc_dvcs} \$ "
}
PROMPT_COMMAND=prompt_command
