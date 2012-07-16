# Test if $1 is a base rails directory
function rails_dir()
{
  [[ -x ${1}/script/rails || -x ${1}/script/server ]]
}

function tmux_rails()
{
  if ! rails_dir `pwd`; then
    echo `pwd`
    echo $(rails_dir `pwd`)
    echo $(! rails_dir `pwd`)
    echo "Must be run from a base rails directory. Exiting..."
    return 1
  fi

  name=$(basename `pwd`)

  if tmux has-session -t "${name}"; then
    tmux attach-session -t "${name}"
  else
    tmux new-session -d -s "${name}" -n "server"
    tmux send-keys  -t "${name}:0" "rails server" C-m

    tmux new-window -t "${name}:1" -n "guard"
    tmux send-keys  -t "${name}:1" "guard" C-m

    tmux new-window -t "${name}:2" -n "vim"
    tmux send-keys  -t "${name}:2" "vim ." C-m

    tmux new-window -t "${name}:3"

    tmux select-window -t "${name}:2"
    tmux select-window -t "${name}:3"
    tmux attach-session -t "${name}"
  fi
}
