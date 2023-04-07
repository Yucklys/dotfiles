if status is-interactive
    # Commands to run in interactive sessions can go here
end

starship init fish | source
zoxide init fish | source

set -g fish_key_bindings fish_vi_key_bindings

eval $(opam env)

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/yucklys/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

