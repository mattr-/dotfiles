export ZDOTDIR=$HOME/.zsh
fpath=($ZDOTDIR/functions $fpath)


# automatically remove duplicates from these arrays
typeset -gU path cdpath fpath manpath fignore

# link ld library path variables
typeset -TU LD_LIBRARY_PATH ld_library_path

path=(/usr/local/share/npm/bin /usr/local/sbin /usr/local/bin $path)

#don't print errors about matching globs
setopt nonomatch

# autoload executables in $fpath
for dirname in $fpath; do
    autoload -- $dirname/[^_]*(:t) &>/dev/null
done

# make colors available
autoload -U colors ;  colors

#Set editor variables
export EDITOR=vim
export VISUAL=$EDITOR

path=($HOME/.go/bin $path)
path=(/usr/local/heroku/bin $path)
path=($HOME/.rbenv/bin $path)
path=($HOME/.cargo/bin $path)
path=($HOME/bin $path)
