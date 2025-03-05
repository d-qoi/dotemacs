# Drop these in ~/.bash_aliases

[ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
  source "$EAT_SHELL_INTEGRATION_DIR/bash"

alias emacsdaemon="emacs --daemon"
alias emax="emacsclient -c"
alias remote_copy_term_info='f() { infocmp -x | ssh "$1" -- tic -x -; }; f'