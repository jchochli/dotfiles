[ -n "$PS1" ] && source ~/.bash_profile
eval "$(grunt --completion=bash)"

homeshick --quiet refresh
