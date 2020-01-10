[ -z "$TMUX"  ] && { tmux attach-session -t home || exec tmux new -s home && exit;}
