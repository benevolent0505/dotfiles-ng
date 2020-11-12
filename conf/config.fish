# alias
alias screen "screen -U"
alias f "open ."

# 変数
set -g EDITOR "emacsclient -nw"

# 便利スクリプト置場
set -x PATH $PATH $HOME/local/bin

# fish
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

set -g GHQ_SELECTOR peco

# Perl
status --is-interactive; and . (plenv init -|psub)
alias reply "rlwrap carton exec -- reply"

# Go
set -x GOPATH $HOME/develop
set -x PATH $PATH $GOPATH/bin

# Node
status --is-interactive; and source (nodenv init -|psub)

if test -d /usr/local/opt/readline
  # readline compilers config
  set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
  set -gx CPPFLAGS "-I/usr/local/opt/readline/include"

  # readline pkg_config config
  set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"
end
