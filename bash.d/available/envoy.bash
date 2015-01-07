# Load the ssh keys into memory
if hash envoy 2>/dev/null; then
  envoy -d id_ecdsa id_rsa
  source <(envoy -p)
fi
