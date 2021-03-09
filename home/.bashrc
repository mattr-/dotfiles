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

alias vim="nvim"
alias hs="homeshick"
