HOME=$(shell echo $$HOME)
PWD=$(shell pwd)
LN=ln -s -f

install: install_bash install_vim install_git install_hg
	
install_bash: profile bashrc dir_colors
	$(LN) $(PWD)/profile $(HOME)/.profile
	$(LN) $(PWD)/bashrc $(HOME)/.bashrc
	$(LN) $(PWD)/dir_colors $(HOME)/.dir_colors

install_vim: vimrc
	$(LN) $(PWD)/vimrc $(HOME)/.vimrc
	$(LN) $(PWD)/vim $(HOME)/.vim

install_git: gitconfig gitignore git-completion.bash
	$(LN) $(PWD)/gitconfig $(HOME)/.gitconfig
	$(LN) $(PWD)/gitignore $(HOME)/.gitignore
	$(LN) $(PWD)/git-completion.bash $(HOME)/.git-completion.bash

install_hg: hgrc hgignore
	$(LN) $(PWD)/hgrc $(HOME)/.hgrc
	$(LN) $(PWD)/hgignore $(HOME)/.hgignore
