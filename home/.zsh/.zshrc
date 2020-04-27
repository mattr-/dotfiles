setopt auto_cd \
       no_beep \
       no_hup \
       pushd_ignore_dups

bindkey -e

export LSCOLORS=ExfxcxdxCxegedabagacad
#Configure the completion system

#list choices on an ambiguous completion
setopt auto_list

#complete things for aliases too
setopt completealiases

#use menu after the second attempt
#setopt auto_menu

#recognize exact matches even if ambiguous
setopt rec_exact

#cursor to the end
setopt always_to_end

#correct the command, if needed
setopt correct

#magical adding/removing final characters on tab completion
setopt auto_param_keys auto_param_slash auto_remove_slash

#zinit initialization
source ~/.zinit/bin/zinit.zsh

#homeshick initialization
source ~/.homesick/repos/homeshick/homeshick.sh
fpath=($HOME/.homesick/repos/homeshick/completions $fpath)

#load and initialize stuff
zmodload zsh/complist

autoload -U compinit
compinit

# cache results
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path $ZDOTDIR/cache-$HOST

# Don't complete CVS or SVN files/directories
zstyle ':completion:*:(all-|)files' ignored-patterns '(|*/)CVS' '(|*/).svn'
zstyle ':completion:*:cd:*' ignored-patterns '(*/)#CVS' '(|*/).svn'

# remove the trailing slash on directories
zstyle ':completion:*' squeeze-slashes true

# allow cursor-key navigation through completion set
zstyle ':completion:*:*:*:*' menu select

# use colours in completion lists and menus
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# ignore working and backup copies, and compiled objects
zstyle ':completion:*:(all-|)files' ignored-patterns \
  '*.bk' '*.bak' '*.old' '*~' '.*.sw?' '*.o' '*.pyc'

# fuzzy matching of completions when I mistype them
zstyle ':completion:*' completer _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# functions which start with _ are internal and ignored
zstyle ':completion:*:functions' ignored-patterns '_*'

# do not offer files already specified on the line
zstyle ':completion:*:rm:*' ignore-line yes

# ignore current directory when completing in ../
zstyle ':completion:*' ignore-parents parent pwd

# complete process IDs with menu selection
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

# don't complete uninteresting users
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
    dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
    hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
    mailman mailnull mldonkey mysql nagios \
    named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
    operator pcap postfix postgres privoxy pulse pvm quagga radvd \
    rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs

# unless we really want to
zstyle '*' single-ignored show

# setup some nice completions with fzf-tab - from their readme
# give a preview of commandline arguments when completing `kill`
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm,cmd -w -w"
zstyle ':fzf-tab:complete:kill:argument-rest' extra-opts --preview=$extract'ps --pid=$in[(w)1] -o cmd --no-headers -w -w' --preview-window=down:3:wrap

# give a preview of directory by exa when completing cd
zstyle ':fzf-tab:complete:cd:*' extra-opts --preview=$extract'exa -1 --color=always $realpath'


# commands that take commands as arguments
compdef _precommand gdb
compdef _precommand nohup
compdef _precommand strace


#ls aliases
alias ls="ls -G"
[[ $OSTYPE == "linux-gnu" ]] && alias ls="ls --color=auto"
alias l="ls -l"
alias lh="ls -lh"
alias la="ls -lha"
alias lla="ls -la"
alias lsa="ls -ah"
alias lsd="ls -d"
alias ld="ls -ldh"
alias lart="ls -larth"
alias llart="ls -lart"
alias lr='ls -lRh'
alias llr='ls -lR'

#process aliases
alias psg="ps ax | grep"
alias psug="ps ax | grep"
alias k="kill"
alias k9="kill -9"
alias sk="sudo kill"
alias sk9="sudo kill -9"

#debian/ubuntu shortcuts
alias sapt="sudo aptitude"
alias sapti="sudo aptitude install"
alias saptbd="sudo aptitude build-dep"

#SUSE shortcuts
alias szi="sudo zypper install"
alias szs="sudo zypper search"

#Archlinux shortcuts
alias spac="sudo pacman-color"
alias spacs="sudo pacman-color -S"
alias pacsearch="sudo pacman-color -Ss"
alias aurs="yaourt -Ss"
alias aurdl="cower -d"


#apply me some patches
alias pp0i="patch -p0 -i "
alias pp1i="patch -p1 -i "

alias roo="roo.sh"
alias ant="ant -emacs"

alias t="todo.sh"

alias vim="nvim"
alias -g ...=../..
alias -g ....=../../..
alias -g .....=../../../..
alias -g ......=../../../../..

alias -g L='| less'
alias -g LS='| less -S'
alias -g EL='|& less'
alias -g ELS='|& less -S'
alias -g TRIM='| cut -c 1-$COLUMNS'

alias -g H='| head'
alias -g HL='| head -n $(( +LINES ? LINES - 4 : 20 ))'
alias -g EH='|& head'
alias -g EHL='|& head -n $(( +LINES ? LINES - 4 : 20 ))'

alias -g T='| tail'
alias -g TL='| tail -n $(( +LINES ? LINES - 4 : 20 ))'
alias -g ET='|& tail'
alias -g ETL='|& tail -n $(( +LINES ? LINES - 4 : 20 ))'
setopt append_history \
       bang_hist \
       extended_history \
       hist_ignore_space

HISTSIZE=2000
SAVEHIST=6000
HISTFILE=~/.zsh/history.$HOST



restart () {
  if jobs | grep . >/dev/null; then
    echo "Jobs running; won't restart." >&2
    jobs -l >&2
  else
    exec $SHELL $SHELL_ARGS "$@"
  fi
}

# Sweet trick from zshwiki.org :-)
cd () {
  if (( $# != 1 )); then
    builtin cd "$@"
    return
  fi

  if [[ -f "$1" ]]; then
    builtin cd "$1:h"
  else
    builtin cd "$1"
  fi
}

z () {
  cd ~/"$1"
}

function title() {
  # escape '%' chars in $1, make nonprintables visible
  a=${(V)1//\%/\%\%}

  a=$(print -Pn "%40>...>$a" | tr  -d "\n")

  case $TERM in
  screen)
    print -Pn "\ek$a:$3\e\\" # screen title (in ^A")
    ;;
  xterm*|rxvt)
    print -Pn "\e]2;$2 | $a:$3\a" # plain xterm title
    ;;
  esac
}

export ANDROID_HOME=$HOME/Applications/Android
export ANT_HOME=$HOME/Applications/apache-ant-1.8

autoload -U bashcompinit
bashcompinit

alias be="bundle exec"

# Aliases
alias g='git'
compdef g=git

alias gst='git status'
compdef _git gst=git-status

alias gl='git pull'
compdef _git gl=git-pull

alias gup='git fetch && git rebase'
compdef _git gup=git-fetch

alias gp='git push'
compdef _git gp=git-push

gdv() { git-diff -w "$@" | view - }
compdef _git gdv=git-diff

alias gc='git commit -v'
compdef _git gc=git-commit

alias gca='git commit -v -a'
compdef _git gca=git-commit

alias gco='git checkout'
compdef _git gco=git-checkout

alias gb='git branch'
compdef _git gb=git-branch

alias gba='git branch -a'
compdef _git gba=git-branch

alias gcount='git shortlog -sn'
compdef gcount=git

alias gcp='git cherry-pick'
compdef _git gcp=git-cherry-pick

alias glg='git log --stat --max-count=5'
compdef _git glg=git-log

alias glgg='git log --graph --max-count=5'
compdef _git glgg=git-log

alias gss='git status -s'
compdef _git gss=git-status

alias ga='git add'
compdef _git ga=git-add

alias gm='git merge'
compdef _git gm=git-merge

# Git and svn mix
alias git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
compdef git-svn-dcommit-push=git

alias gsr='git svn rebase'
alias gsd='git svn dcommit'
#
# Will return the current branch name
# Usage example: git pull origin $(current_branch)
#
function current_branch() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
  echo ${ref#refs/heads/}
}

# these aliases take advantage of the previous function
alias ggpull='git pull origin $(current_branch)'
compdef ggpull=git
alias ggpush='git push origin $(current_branch)'
compdef ggpush=git
alias ggpnp='git pull origin $(current_branch) && git push origin $(current_branch)'
compdef ggpnp=git


alias git=hub
#  vim: set et sts=2 sw=2 ts=2 ft=zsh : 
alias rvmu="rvm use"
alias rvmg="rvm gemset"
alias rvmgla="rvm gemset list_all"
alias rvmi="rvm install"
# Information in "man zshcontrib"
zstyle ':vcs_info:git:*' check-for-changes true

# Custom staged and unstaged changes strings
zstyle ':vcs_info:*' unstagedstr '!'
zstyle ':vcs_info:*' stagedstr '+'
 
# Format when some action is going on in 
# your repository (rebranch,merge conflict, ...)
#
zstyle ':vcs_info:*' actionformats \
    "%r (%s) | %b | %a"

# Default format: repo(vcs)|branch
# Full of prompt formatting :)
zstyle ':vcs_info:*' formats       \
    "%r(%s) | %b%c%u"
 
# Used VCS use 
# %  vcs_info_printsys 
# for supported systems 
  zstyle ':vcs_info:*' enable git cvs svn hg

autoload -U promptinit; promptinit
prompt mattr2

compctl -/ -W ~/Code c
compctl -/ -W ~/Projects/Hireology h
function precmd() {
  title "zsh" "$USER@%m" "%55<...<%~"
}

function preexec() {
  title "$1" "$USER@%m" "%35<...<%~"
}
export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
which nodenv &> /dev/null
[[ $? -eq 0 ]]  && eval "$(nodenv init -)"
# This loads rbenv into a shell session.
which rbenv &> /dev/null
[[ $? -eq 0 ]] && eval "$(rbenv init -)"
# REE / 1.8.7 (for some really old apps)
export RUBY_HEAP_FREE_MIN=1024
export RUBY_HEAP_MIN_SLOTS=4000000
export RUBY_HEAP_SLOTS_INCREMENT=250000
export RUBY_GC_MALLOC_LIMIT=500000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1

# for 2.1.2
export RUBY_GC_HEAP_INIT_SLOTS=600000
export RUBY_GC_HEAP_FREE_SLOTS=600000
export RUBY_GC_HEAP_GROWTH_FACTOR=1.25
export RUBY_GC_HEAP_GROWTH_MAX_SLOTS=300000
export RUBY_GC_MALLOC_LIMIT=64000000
export RUBY_GC_OLDMALLOC_LIMIT=64000000
#eval $(ssh-agent)
#if [ -n "$SSH_AGENT_PID" ]
#then
#    ssh-add $HOME/.ssh/id_rsa >/dev/null 2>&1
#fi

[[ -f /usr/local/opt/asdf/asdf.sh ]] && source /usr/local/opt/asdf/asdf.sh
[[ -f /opt/asdf-vm/asdf.sh ]] && source /opt/asdf-vm/asdf.sh

#zinit plugins
zinit light zsh-users/zsh-autosuggestions
zinit light zdharma/fast-syntax-highlighting
zinit light Aloxaf/fzf-tab
