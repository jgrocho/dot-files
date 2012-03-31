# Source all *-bash-completion files under ~/.bash-completion.d/enabled.
BASH_COMPLETION_USER_DIR=~/.bash-completion.d/enabled
if [[ -d ${BASH_COMPLETION_USER_DIR} && -r ${BASH_COMPLETION_USER_DIR} && \
  -x ${BASH_COMPLETION_USER_DIR} ]]; then
    for f in ${BASH_COMPLETION_USER_DIR}/*-bash-completion ; do
        [[ -f $f && -r $f ]] && . "$f"
    done
fi
unset f
