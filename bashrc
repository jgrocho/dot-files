# .bashrc

# Source global definitions
[[ -f /etc/bashrc ]] && . /etc/bashrc

# User specific aliases and functions

# Source *.bash files under ~/.bash.d/enabled
BASH_USER_DIR=~/.bash.d/enabled
if [[ -d ${BASH_USER_DIR} && -r ${BASH_USER_DIR} && -x ${BASH_USER_DIR} ]]; then
    for f in ${BASH_USER_DIR}/*.bash ; do
        [[ -f $f && -r $f ]] && . "$f"
    done
fi
