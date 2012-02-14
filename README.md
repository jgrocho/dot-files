# Dot Files

My personal collection of dotfiles for various programs and utilities. Most of
these can be installed by symlinking or copying them to the appropriate
location, generally `$HOME` (with a `.` prepended). At some point, I would
like to create a script or program for installing and managing these files.

Throughout this document, when referring to these dotfiles, I will leave off
the leading `.` except where it may be ambiguous.

## `*.d`

I have attempted (and will continue) to make my settings as OS/distro/external
environment agnostic as possible (though I make no claims as to how well I
have achieved that). However, there will always been a need for external
programs/libraries. To that end, I have devised to create, where appropriate,
`.d` directories to manage configuration settings. Within these directories
exist `available` and `enabled` directories which contain all available and
the current set of enabled configurations, respectively.

That is, the files in `enabled` are symbolic links to files in the `available`
directory and the appropriate configuration file loads the files listed in the
`enabled` directory. In this way, certain features can be enabled and disabled
more easily on specific machines as needed. All symlinks between `available`
and `enabled` should be relative and not absolute to ensure that they still
function even after moving the directory.

Since the ordering of loading files can be important, it is generally
encouraged that all links in the `enabled` directories be prefixed by a number
(my convention is two digits and a hyphen), since most times, files will be
loaded in lexicographical order.

The git repository is set to ignore all the files under the `enabled`
directory. Since users may want to symlink to these `.d` directories, ignoring
the contents of the `enabled` directories ensures that any locally enabled
settings do not get reflected in this git repository and thus don't become
globally enabled. However, there may be times when it is appropriate to enable
certain settings (as sane defaults). In these situations, after the symlink is
created in the `enabled` directory, it can be tracked with a `git add -f`,
which will override the git ignores.

### `bash.d`

This directory contains scripts for configuring bash. Since `bash_profile`
sources `bashrc`, and `bashrc` sources each of the files in `bash.d/enabled`,
anything in this directory will be available in all bash shells, login and
not. Since I have yet to encounter a situation in which I needed to
distinguish between the two, I will leave this setup until, and if, the need
arises to change it.

When sourcing files under `bash.d`, `bashrc` will only load those ending in
`.bash`. This allows scripts the ability to use the local directory for
scratch or configuration files without interfering with the automatic sourcing
of the files. As stated above, it is generally good practice to prefix the
file names with a number, to force an ordering. The leading number, however,
is not required. This provides a primitive dependency chaining mechanism, but
no other mechanism yet exists to describe to enforce any dependency
requirements.

### `bash-completion.d`

Bash offers tab completion for a large variety of commands, but not all of
them. This directory contains the bash completion scripts I have found useful
for those commands which either do not provide them in a default installation.

Only files ending in `-bash-completion` are automatically loaded. As with
`bash.d`, number prefixes are recommended, but not required nor as likely as
important as these files tend not to require an ordering.

In some cases the files in this directory are not the actual bash completion
scripts, but scripts to load them, for those commands which provide bash
completion and yet do not enable it by default. One notable example of this is
RVM, which provides a bash completion script, but does not load it by default.

I have choosen to separate this functionality out, just in case. It needs to
be enabled with the `bash-completion.bash` file under `bash.d`.
