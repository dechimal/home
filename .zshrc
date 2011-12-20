# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/d/.zshrc'

# add fpath for local defined completions
grep -q ~/.zsh.d <<<$fpath || fpath=(~/.zsh.d $fpath)

autoload -Uz compinit
compinit
# End of lines added by compinstall

[[ -n $SSH_CLIENT ]] && ssh=' |'`sed -r 's/ .*//;' <<<$SSH_CLIENT`
[[ -n $SSH_AGENT_PID ]] && launcher=\*

PS1="$launcher%n %(4c;...;)%3~$ssh%# "
grep $HOME/progs/bin -q <<<$path || PATH=($HOME/progs/bin $path)

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
source ~/.zshrc.local

export EDITOR=emacs${WINDOWID:+client}
export ALTERNATE_EDITOR=emacs

setopt autopushd
setopt extendedglob

[[ -z $WINDOWID ]] && export LOCALE=en_US.UTF-8
