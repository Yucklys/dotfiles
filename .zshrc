# Path to your oh-my-zsh installation.
export ZSH="/home/yucklys/.oh-my-zsh"
export PATH=$HOME/.cargo/bin:$HOME/.local/bin:$HOME/.yarn/bin:$HOME/.local/lib/python3.7/site-packages:$PATH

# oxide
# bullet-train
# typewritten
# spaceship
ZSH_THEME='typewritten'
export TYPEWRITTEN_CURSOR="underscore"
# BULLETTRAIN_PROMPT_CHAR='$'
# BULLETTRAIN_CONTEXT_DEFAULT_USER="yucklys"
# BULLETTRAIN_PROMPT_ORDER=(
#   git
#   context
#   dir
#   time
#   screen
# )
# BULLETTRAIN_CONTEXT_DEFAULT_USER='yucklys'
# eval "$(starship init zsh)"

SPACESHIP_CHAR_SYMBOL='$ '

plugins=(autojump git extract zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

alias zshconfig='vim ~/.zshrc'
alias zshreload='source ~/.zshrc'
alias i3config='vim ~/.i3/config'
alias pacman='sudo pacman'
alias rm='trash'
alias c='xclip -i'
alias clip='xclip -sel clip'
alias v='xclip -o'
alias ls='exa'
alias lsl='exa -l'
alias lsa='exa -a'
alias vim='nvim'
alias svim='sudo -E nvim'
alias pipi='pip3 install --user'
alias rime-install='rime_frontend=fcitx-rime bash ~/.config/fcitx/plum/rime-install'
alias config='/usr/bin/git --git-dir=$HOME/Documents/dotfiles --work-tree=$HOME'
alias you-get-cookies='you-get -c ~/.mozilla/firefox/w0qavkoa.default-release/cookies.sqlite'
alias pyay='proxychains -q yay'
alias dcargo='RUST_BACKTRACE=1 cargo'
