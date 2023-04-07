# Nushell Environment Config File
#
# version = 0.78.0

def create_left_prompt [] {
    mut home = ""
    try {
        if $nu.os-info.name == "windows" {
            $home = $env.USERPROFILE
        } else {
            $home = $env.HOME
        }
    }

    let dir = ([
        ($env.PWD | str substring 0..($home | str length) | str replace -s $home "~"),
        ($env.PWD | str substring ($home | str length)..)
    ] | str join)

    let path_segment = if (is-admin) {
        $"(ansi red_bold)($dir)"
    } else {
        $"(ansi green_bold)($dir)"
    }

    $path_segment
}

def create_right_prompt [] {
    let time_segment = ([
        (date now | date format '%m/%d/%Y %r')
    ] | str join)

    $time_segment
}

# Use nushell functions to define your right and left prompt
let-env PROMPT_COMMAND = {|| create_left_prompt }
let-env PROMPT_COMMAND_RIGHT = {|| create_right_prompt }

# The prompt indicators are environmental variables that represent
# the state of the prompt
let-env PROMPT_INDICATOR = {|| "> " }
let-env PROMPT_INDICATOR_VI_INSERT = {|| ": " }
let-env PROMPT_INDICATOR_VI_NORMAL = {|| "> " }
let-env PROMPT_MULTILINE_INDICATOR = {|| "::: " }

# Specifies how environment variables are:
# - converted from a string to a value on Nushell startup (from_string)
# - converted from a value back to a string when running external commands (to_string)
# Note: The conversions happen *after* config.nu is loaded
let-env ENV_CONVERSIONS = {
  "PATH": {
    from_string: { |s| $s | split row (char esep) | path expand -n }
    to_string: { |v| $v | path expand -n | str join (char esep) }
  }
  "Path": {
    from_string: { |s| $s | split row (char esep) | path expand -n }
    to_string: { |v| $v | path expand -n | str join (char esep) }
  }
}

# Directories to search for scripts when calling source or use
#
# By default, <nushell-config-dir>/scripts is added
let-env NU_LIB_DIRS = [
    ($nu.config-path | path dirname | path join 'scripts')
]

# Directories to search for plugin binaries when calling register
#
# By default, <nushell-config-dir>/plugins is added
let-env NU_PLUGIN_DIRS = [
    ($nu.config-path | path dirname | path join 'plugins')
]

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# let-env PATH = ($env.PATH | split row (char esep) | prepend '/some/path')

# pnpm
let-env PNPM_HOME = "/home/yucklys/.local/share/pnpm"

# To add entries to PATH (on Windows you might use Path), you can use the following pattern:
# let-env PATH = ($env.PATH | split row (char esep) | prepend '/some/path')
let-env PATH = ($env.PATH | split row (char esep) |
                prepend ['~/.cargo/bin',
                         '~/.local/bin',
                         '~/.local/share/gem/ruby/3.0.0/bin',
                         '~/anaconda3/bin',
                         $env.PNPM_HOME])

# load zoxide
zoxide init nushell | save -f ~/.zoxide.nu

# load opam env
let-env OPAM_SWITCH_PREFIX = '/home/yucklys/.opam/4.12.0'
let-env CAML_LD_LIBRARY_PATH = '/home/yucklys/.opam/4.12.0/lib/stublibs:/home/yucklys/.opam/4.12.0/lib/ocaml/stublibs:/home/yucklys/.opam/4.12.0/lib/ocaml'
let-env OCAML_TOPLEVEL_PATH = '/home/yucklys/.opam/4.12.0/lib/toplevel'
let-env MANPATH = ':/home/yucklys/.opam/4.12.0/man'
let-env PATH = ($env.PATH | split row (char esep) | prepend ['/home/yucklys/.opam/4.12.0/bin', '/home/yucklys/.local/bin', '/home/yucklys/.cargo/bin', '/home/yucklys/.emacs.d/bin', '/usr/local/bin', '/usr/bin', '/bin', '/usr/local/sbin', '/usr/lib/jvm/default/bin', '/usr/bin/site_perl', '/usr/bin/vendor_perl', '/usr/bin/core_perl', '/var/lib/snapd/snap/bin'])

# bitwarden
let-env BW_SESSION = "Mn/ZgN3brE1JBSFFa4ort3fkgaEgmkA8Ov+2Kp1NB6Qul4EwHsgqx+H3xO/vGPq8wkEA4i21rc0Tohg0LK36Ag=="
