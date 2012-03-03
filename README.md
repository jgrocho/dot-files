# Dot Files

My personal collection of dotfiles for various programs and utilities.
Most of these can be installed by symlinking or copying them to the
appropriate location, generally `$HOME` (with a `.` prepended). At some
point, I would like to create a script or program for installing and
managing these files.

Throughout this document, when referring to these dotfiles, I will leave
off the leading `.` except where it may be ambiguous.

## \*.d

I have attempted (and will continue) to make my settings as
OS/distro/external environment agnostic as possible (though I make no
claims as to how well I have achieved that). However, there will always
been a need for external programs/libraries. To that end, I have devised
to create, where appropriate, `.d` directories to manage configuration
settings. Within these directories exist `available` and `enabled`
directories which contain all available and the current set of enabled
configurations, respectively.

That is, the files in `enabled` are symbolic links to files in the
`available` directory and the appropriate configuration file loads the
files listed in the `enabled` directory. In this way, certain features
can be enabled and disabled more easily on specific machines as needed.
All symlinks between `available` and `enabled` should be relative and
not absolute to ensure that they still function even after moving the
directory.

Since the ordering of loading files can be important, it is generally
encouraged that all links in the `enabled` directories be prefixed by a
number (my convention is two digits and a hyphen), since most times,
files will be loaded in lexicographical order.

The git repository is set to ignore all the files under the `enabled`
directory. Since users may want to symlink to these `.d` directories,
ignoring the contents of the `enabled` directories ensures that any
locally enabled settings do not get reflected in this git repository and
thus don't become globally enabled. However, there may be times when it
is appropriate to enable certain settings (as sane defaults). In these
situations, after the symlink is created in the `enabled` directory, it
can be tracked with a `git add -f`, which will override the git ignores.

### bash.d

This directory contains scripts for configuring bash. Since
`bash_profile` sources `bashrc`, and `bashrc` sources each of the files
in `bash.d/enabled`, anything in this directory will be available in all
bash shells, login and not. Since I have yet to encounter a situation in
which I needed to distinguish between the two, I will leave this setup
until, and if, the need arises to change it.

When sourcing files under `bash.d`, `bashrc` will only load those ending
in `.bash`. This allows scripts the ability to use the local directory
for scratch or configuration files without interfering with the
automatic sourcing of the files. As stated above, it is generally good
practice to prefix the file names with a number, to force an ordering.
The leading number, however, is not required. This provides a primitive
dependency chaining mechanism, but no other mechanism yet exists to
describe to enforce any dependency requirements.

#### Available

What follows is a brief description of some of the available scripts.

*   `bash-completions.bash`: sources files under
    `~/.bash-completion.d/enabled` that in `-bash-completion`. (See
    below for more details.)

*   `prompt-command.bash`: Uses the `PROMPT_COMMAND` variable to set
    `PS1` to include some handy information. Currently, it displays the
    last command's exit status, the user's name, the current hostname
    when connected via SSH, the full current directory, the current Git
    branch and status, and information about any virtual development
    environments (i.e. rvm, virtualenv, or virthualenv).

    This display makes use of color escape codes, and does not provide
    detection of capabilities nor fallback settings. The results on
    non-color enabled terminals is not guarenteed.

*   `solarized-fix.bash`: The solarized `dircolors.256dark` theme
    provided by seebi/dircolors-solarized has to approximate some of the
    solarized colors for maximum compatibility. The magical incantation
    provided here remaps the colors to make the approximated colors
    exact. This fix works in xterm and maybe some other terminals. (See
    altercation/solarized/#8 for more details.)

### bash-completion.d

Bash offers tab completion for a large variety of commands, but not all
of them. This directory contains the bash completion scripts I have
found useful for those commands which either do not provide them in a
default installation.

Only files ending in `-bash-completion` are automatically loaded. As
with `bash.d`, number prefixes are recommended, but not required nor as
likely as important as these files tend not to require an ordering.

In some cases the files in this directory are not the actual bash
completion scripts, but scripts to load them, for those commands which
provide bash completion and yet do not enable it by default. One notable
example of this is RVM, which provides a bash completion script, but
does not load it by default.

I have chosen to separate this functionality out, just in case. It
needs to be enabled with the `bash-completion.bash` file under `bash.d`.

## tmux

tmux has replaced screen and I have finally joined the fun. Most of
these settings given here for tmux are to change the key bindings to
match screen, though not all changes have been made yet.

## Vim

My current `vimrc` is not very feature rich and could stand to use some
improvements and enhancements. Nevertheless, it does mostly what I want
it to do most of the time. This is one file which will like see a lot of
improvement over time.

### pathogen

To handle Vim plugins, I have decided to use the pathogen plugin from
Tim Pope. Essentially pathogen allows you to separate Vim plugins into
distinct directories under `.vim/bundle` (or any other directory under
`.vim` as specified by the call to `pathogen#infect`). This make
updating and removing Vim plugins much simpler, as you no longer have to
determine which files belong to which plugins when making a
modification.

This nice separation of Vim plugins means we can utilize git's support
for submodules to efficiently manage them. Furthermore, we can even use
this feature to manage `pathogen` itself. However, this feature comes at
a slight cost as we need to initialize and update the submodules
whenever we clone this repository. This means running

    git submodule init
    git submodule update

in the top-level directory of the repository after `git clone`. This
will clone all submodules into their respective directories. This
process can be made more selective by appending the name of the
submodule to the commands above. Since `pathogen` is one these
submodules though, if that submodule is not cloned then pathogen will
not be loaded along with any bundles.

### Local Changes

Since not all configurations are necessarily part of any specific plugin
or should part of a change to plugin, (e.g. personal preferences), any
local changes to the Vim configuration can still be placed in the usual
locations under `vim`.

## X

X uses a few files to store configuration settings.

### xinitrc

This file is sourced when running `startx` and `xinit` or by most
display managers. The main purpose of this file is to start up your
window manager among a few other programs.

A few settings/commands don't make sense to be placed in this file, such
as the choice of window manager since that is something that is going to
be specific to the system being used. I could check for display managers
in an order I prefer but it seems more practical to off-load starting
the window manager to an external file. So the last thing this file does
is load `.xinitrc.local` which should launch a window manager and any
other very host specific applications. If that file isn't found the
global `/etc/X11/xinit/xinitrc` file is sourced and, failing that, a
default window manager (`twm`) is started with a single `xterm`
instance.

### Xresources

This file is really the meat of X's configuration (or it should be).
Many X programs make use of Xresources to determine their configuration
(and more should). This file needs to loaded or merged into the database
with `xrdb` and this is handled by `xinitrc`.

Since this file is run through the C preprocessor, we can take advantage
of `#define`s, `#include`s, and `#ifdef`s. We can place included files
under Xresources.d and load them as needed.

#### Xresources.d

This directory holds files which can be loaded by `Xresources`.

*   `colors`: This holds a color theme which changes the colors used by
    X programs. Currently this is from the solarized theme.

