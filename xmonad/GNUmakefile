################################################################################
ARCH        = $(shell uname -m)
OS          = $(shell uname -s | tr '[A-Z]' '[a-z]')
TARGET      = $(HOME)/.xmonad/xmonad-$(ARCH)-$(OS)
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
install: $(TARGET)
	mkdir -p $(dir $(XMONAD))
	cp $(XMONADRC) $(XMONAD)-new
	mv $(XMONAD)-new $(XMONAD)

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

################################################################################
$(TARGET): $(XMONADRC)
	mkdir -p $(dir $@)
	if [ -r $@ ]; then mv $@ $@.prev; fi
	cp -p $< $@
	cd $(dir $@) && ln -nfs $(notdir $@) xmonadrc
