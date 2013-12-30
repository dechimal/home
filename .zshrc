# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
bindkey -e
# End of lines configured by zsh-newuser-install

typeset -U fpath path

# The following lines were added by compinstall
zstyle :compinstall filename '/home/d/.zshrc'

# add fpath for local defined completions
fpath=(~/.zsh.d $fpath)

autoload -Uz compinit
compinit
# End of lines added by compinstall

setopt hist_ignore_dups
setopt pushd_ignore_dups

setopt promptsubst
git-info() {
  local branch gitdir color_start color_end rebasing_marker
  branch=$(git branch 2>/dev/null | sed '/^*/s/^* //p;d')
  gitdir=$(git rev-parse --git-dir 2>/dev/null)
  if [[ -d $gitdir/rebase-merge || -d $gitdir/rebase-apply ]]; then
    color_start='%{\x1b[31m%}'
    color_end='%{\x1b[0m%}'
    rebasing_marker='! '
  fi
  if [[ -n $branch ]]; then
    echo -n \ $color_start\[$rebasing_sign$branch\]$color_end
  fi
}
last-code() {
  echo -n ' %(?.o.%{\x1b[31m%}x%{\x1b[0m%})'
}
abbrev-pwd() {
  pwd | sed -r "s,^$HOME,~,;s,.*/(.*/.*/.*)$,...\1,;s/^/ /"
}

function {
  local ssh launcher
  [[ -n $SSH_CLIENT ]] && ssh=' |'`hostname`
  [[ -n $SSH_AGENT_PID ]] && launcher=\*

  PS1=$launcher'$(whoami)$(last-code)$(git-info)$(abbrev-pwd)'$ssh'%% '
}
# PS1="$launcher%n %(4c;...;)%3~$ssh%# "

path=($HOME/progs/bin $path)

LOVELIVE=(高坂穂乃果 絢瀬絵里 南ことり 園田海未 星空凛 西木野真姫 東條希 小泉花陽 矢澤にこ)

alias zshreload='source ~/.zshrc'
alias diff='diff -u'
alias fromclip='xsel -o -b'
alias toclip='xsel -i -b'
alias appendclip='xsel -a -b'
alias draft='evince ~/junk/doc/newer.pdf &>/dev/null &|'
alias xmm='xmodmap ~/.Xmodmap'
alias semacs='sudo emacs -u d -nw'
alias ldcrontab='crontab ~/.crontab'
alias xlock='xlock -mode blank'
alias ll='ls -l'
alias CP932='iconv -f cp932'
alias scrot-rect='sleep 3; scrot -s ~/ss-`date +%Y-%m-%d-%T`.png'
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

export EDITOR=emacs${WINDOWID:+client}
export ALTERNATE_EDITOR=emacs

setopt autopushd
setopt extendedglob

[[ -z $WINDOWID ]] && export LOCALE=en_US.UTF-8

precmd() {
    path=($path)
}
