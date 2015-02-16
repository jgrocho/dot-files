################################################################################
SRC         = $(shell find src/ -type f -name '*.hs')
SANDBOX     = cabal.sandbox.config

XMONADSRC   = xmonadrc.hs
XMONAD      = $(HOME)/bin/xmonad
XMONADRC    = dist/build/xmonadrc/xmonadrc

XMOBARSRC   = xmobar.hs
XMOBAR      = dist/build/xmobar/xmobar
XMOBARRC    = xmobarrc

GHC_CHECK   = ghc -V | grep -q '7\.8\.4' # Required compiler version.
CABAL_FLAGS = --enable-optimization=2

################################################################################
.PHONEY: all install restart clean realclean ghc-version

################################################################################
all: $(XMONADRC) $(XMOBARRC)

################################################################################
install: $(XMONADRC) $(XMOBARRC)
	install -Dm755 $(XMONADRC) $(XMONAD)

################################################################################
restart: install
	.cabal-sandbox/bin/xmonad --restart

################################################################################
clean:
	rm -rf dist $(XMONADRC) $(XMOBARRC) $(SANDBOX)

################################################################################
realclean: clean
	rm -rf .cabal-sandbox

################################################################################
$(XMONADRC): $(SANDBOX) $(SRC) $(XMONADSRC)
	$(GHC_CHECK)
	cabal build

################################################################################
$(XMOBAR): $(SANDBOX) $(SRC) $(XMOBARSRC)
	$(GHC_CHECK)
	cabal build xmobar

################################################################################
$(XMOBARRC): $(XMOBAR)
	$(XMOBAR)

################################################################################
$(SANDBOX):
	cabal sandbox init
	cabal install --dependencies-only
