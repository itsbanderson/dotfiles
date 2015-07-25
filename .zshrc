# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
#bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/ben/.zshrc'

autoload -Uz compinit
compinit

autoload -U promptinit
promptinit
#prompt bart
# End of lines added by compinstall

# Basic coloring
autoload -U colors && colors
alias ls="ls -h --color=auto"
alias grep="grep --color=auto"
export GREP_COLOR='1;32'

export EDITOR=vim
export PROMPT="%{$fg[red]%}%n%{$reset_color%}@%{$fg[blue]%}%m %{$fg_no_bold[yellow]%}%1~ %{$reset_color%}"

# Set up urxvt as term
export LANG=en_US.UTF-8
export TERM=rxvt-unicode-256color
