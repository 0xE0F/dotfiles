# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"
#ZSH_THEME="agnoster"
DEFAULT_USER="0xe0f"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow-completion zsh-syntax-highlighting history-substring-search)

# [[ "$TERM" == "xterm" ]] && export TERM=xterm-256color


source $ZSH/oh-my-zsh.sh

fancy-ctrl-z () {
	if [[ $#BUFFER -eq 0 ]]; then
		BUFFER="fg"
		zle accept-line
	else
		zle push-input
		zle clear-screen
	fi
}

zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

if [[ $(uname) = 'Darwin' ]]; then
	export CLICOLOR=1
	export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
	alias df='df -h'
	alias du='du -h -d 1 | sort -n'
	alias ls='ls -lhG'
else
	alias -r nautilus="nautilus --no-desktop"
	alias -r ack="ack-grep"
	alias df='df -h'
	alias du='du -h --max-depth=1 | sort -h'
	alias ls='ls -lh --color'
	alias pbcopy='xsel --clipboard --input'
	alias pbpaste='xsel --clipboard --output'
fi


export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[38;5;246m'    # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[04;38;5;146m' # begin underline

# Customs PATHs
export GOPATH=~/Documents/Argotech/Sources/golang
export PATH=:$PATH:$GOPATH/bin
export LC_ALL=en_US.UTF-8
