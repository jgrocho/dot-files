# Load the ssh keys into memory
if hash keychain 2>/dev/null; then
  eval $(keychain --agents ssh,gpg --eval --quiet --quick --noask)
  alias ssh='keychain -Q -q --ignore-missing id_ecdsa id_rsa; ssh'
fi
