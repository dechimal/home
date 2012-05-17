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

setopt promptsubst
git-info() {
  local branch=$(git branch 2>/dev/null | sed '/*/s/* //p;d')
  local gitdir=`pwd`
  until [[ -d $gitdir/.git || `(cd $gitdir; pwd)` == / ]]; do
    gitdir=$gitdir/..
  done
  if [[ -d $gitdir/.git/rebase-merge ]]; then
    local color_start=$'\\x1b[31m'
    local color_end=$'\\x1b[0m'
    local rebasing_message='! '
  fi
  if [[ -n $branch ]]; then
    echo -n \ $color_start\[$rebasing_message$branch\]$color_end
  fi
}

function {
  [[ -n $SSH_CLIENT ]] && local ssh=' |'`sed -r 's/ .*//;' <<<$SSH_CLIENT`
  [[ -n $SSH_AGENT_PID ]] && local launcher=\*

  PS1=$launcher'$(users)$(git-info) $(pwd | sed -r "s,^$HOME,~,;s,.*/(.*/.*/.*)$,...\1,")'$ssh'%% '
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
alias updatecrontab='perl -i -nle '\''s/(\d+)(-\w+)\s*$/($1+1).$2/e; print;'\'' ~/.crontab'
alias xlock='xlock -mode blank'
alias ll='ls -l'
[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local

export EDITOR=emacs${WINDOWID:+client}
export ALTERNATE_EDITOR=emacs

setopt autopushd
setopt extendedglob

[[ -z $WINDOWID ]] && export LOCALE=en_US.UTF-8

precmd() {
    path=($path)
}
