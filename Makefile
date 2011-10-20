HOME=$(shell echo $$HOME)
PWD=$(shell pwd)

install: install_bash install_vim install_git install_hg
	
install_bash: profile bashrc dir_colors
	ln -s $(PWD)/profile $(HOME)/.profile
	ln -s $(PWD)/bashrc $(HOME)/.bashrc
	ln -s $(PWD)/dir_colors $(HOME)/.dir_colors

install_vim: vimrc
	ln -s $(PWD)/vimrc $(HOME)/.vimrc
	rsync -ax vim/ $(HOME)/.vim

install_git: gitconfig gitignore git-completion.bash
	ln -s $(PWD)/gitconfig $(HOME)/.gitconfig
	ln -s $(PWD)/gitignore $(HOME)/.gitignore
	ln -s $(PWD)/git-completion.bash $(HOME)/.git-completion.bash

install_hg: hgrc hgignore
	ln -s $(PWD)/hgrc $(HOME)/.hgrc
	ln -s $(PWD)/hgignore $(HOME)/.hgignore
