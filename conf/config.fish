# alias
alias emacs "emacsclient -nw"

# 便利スクリプト置場
set -x PATH $PATH $HOME/local/bin

# fish
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# Perl
set -x PATH $HOME/.plenv/bin $PATH
set -x PATH $HOME/.plenv/shims $PATH
alias reply "rlwrap reply"

# Go
set -x GOPATH $HOME/develop
set -x PATH $PATH $GOPATH/bin

# Python
status --is-interactive; and source (pyenv init -|psub)
