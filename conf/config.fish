# alias

if test (uname) = "Linux"
    alias open "xdg-open"
end

alias f "open ."
alias emacs "emacsclient -nw"

# 便利スクリプト置場
set -x PATH $PATH $HOME/local/bin

# rbenv
set -x PATH $PATH $HOME/.rbenv/bin
set -x PATH $PATH $HOME/.rbenv/shims
rbenv init - | source

# pyenv
set -x PYENV_ROOT $HOME/.pyenv
set -x PATH $PYENV_ROOT/bin $PATH
set -x PATH $PYENV_ROOT/shims $PATH

# plenv
set -x PATH $HOME/.plenv/bin $PATH
set -x PATH $HOME/.plenv/shims $PATH
plenv init - | source
alias reply "rlwrap reply"

# golang
if test (uname) = "Linux"
    set -x PATH $PATH /usr/local/go/bin
end
set -x GOPATH $HOME/develop
set -x PATH $PATH $GOPATH/bin

# rust (Cargo)
set -x PATH $HOME/.cargo/bin $PATH
set -x RUST_SRC_PATH (rustc --print sysroot)/lib/rustlib/src/rust/src

function peco_change_directory
  ghq list -p | peco | read dir
  cd $dir
end

function peco_history
  history | peco | read cmd
  commandline $cmd
end

function fish_user_key_bindings
  bind \cr peco_history
  bind \cs peco_change_directory
end

function history-merge --on-event fish_preexec
  history --save
  history --merge
end
