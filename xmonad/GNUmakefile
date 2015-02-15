################################################################################
SRC         = $(shell find . -type f -name '*.hs')
SANDBOX     = cabal.sandbox.config
XMONAD      = $(HOME)/bin/xmonad
XMONADRC    = dist/build/xmonadrc/xmonadrc
CABAL_FLAGS = --enable-optimization=2

################################################################################
.PHONEY: all install restart clean realclean

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
realclean:
	rm -rf .cabal-sandbox

################################################################################

$(XMONADRC): $(SRC) $(SANDBOX)
	ghc -V | grep -q 7.8.4 # Required compiler version.
	cabal build

################################################################################
$(SANDBOX):
	cabal sandbox init
	cabal install --dependencies-only
