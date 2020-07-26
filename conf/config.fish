# alias
alias emacs "emacsclient -nw"
alias screen "screen -U"

# 便利スクリプト置場
set -x PATH $PATH $HOME/local/bin

# fish
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

set -g GHQ_SELECTOR peco
set -g GHQ_SELECTOR_OPTS "--layout=top-down --prompt='ghq>'"

# Perl
set -x PATH $HOME/.plenv/bin $PATH
set -x PATH $HOME/.plenv/shims $PATH
alias reply "rlwrap carton exec -- reply"

# Go
set -x GOPATH $HOME/develop
set -x PATH $PATH $GOPATH/bin

# Python
status --is-interactive; and source (pyenv init -|psub)

if test -d /usr/local/opt/readline
  # readline compilers config
  set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
  set -gx CPPFLAGS "-I/usr/local/opt/readline/include"

  # readline pkg_config config
  set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"
end
