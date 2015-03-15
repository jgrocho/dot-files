################################################################################
SRC         = $(shell find src/ -type f -name '*.hs')
SANDBOX     = cabal.sandbox.config

XMONADSRC   = xmonadrc.hs
XMONAD      = $(HOME)/bin/xmonad
XMONADRC    = dist/build/xmonadrc/xmonadrc

GHC_CHECK   = ghc -V | grep -q '7\.8\.4' # Required compiler version.
CABAL_FLAGS = --enable-optimization=2

################################################################################
.PHONEY: all install restart clean realclean ghc-version

################################################################################
all: $(XMONADRC)

################################################################################
install: $(XMONADRC)
	install -Dm755 $(XMONADRC) $(XMONAD)

################################################################################
restart: install
	.cabal-sandbox/bin/xmonad --restart

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(SANDBOX)

################################################################################
realclean: clean
	rm -rf .cabal-sandbox

################################################################################
$(XMONADRC): $(SANDBOX) $(SRC) $(XMONADSRC)
	$(GHC_CHECK)
	cabal build

################################################################################
$(SANDBOX):
	cabal sandbox init
	cabal sandbox add-source ~/src/xmonad
	cabal sandbox add-source ~/src/XMonadContrib
	cabal install --dependencies-only
