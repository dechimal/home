# Lines configured by zsh-newuser-install
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/d/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

if [[ ! -z $SSH_CONNECTION ]]; then
    ssh=' @'$SSH_CONNECTION
fi

[[ ! -z $SSH_CLIENT ]] && ssh=' |'`echo $SSH_CLIENT | sed -r 's/ .*//;'`

PS1="%n %(4c;...;)%3~$ssh%# "
PATH=$(echo $PATH | sed -e 's,/home/d/progs/bin:,,g')
PATH=/home/d/progs/bin:$PATH

LOVELIVE=(高坂穂乃果 絢瀬絵里 南ことり 園田海未 星空凛 西木野真姫 東條希 小泉花陽 矢澤にこ)

alias zshreload='source ~/.zshrc'
alias diff='diff -u'
alias fromclip='xsel -o -b'
alias toclip='xsel -i -b'
alias appendclip='xsel -a -b'
alias draft='evince ~/junk/doc/newer.pdf &>/dev/null &|'
alias xmm='xmodmap ~/.Xmodmap'
alias semacs='sudo emacs -u d'
alias ldcrontab='crontab ~/.crontab'
alias updatecrontab='perl -i -nle '\''s/(\d+)(-\w+)\s*$/($1+1).$2/e; print;'\'' ~/.crontab'
export EDITOR=emacs${WINDOWID:+client}
export ALTERNATE_EDITOR=emacs

setopt autopushd
setopt extendedglob

[[ $WINDOWID == '' ]] && export LOCALE=en_US.UTF-8
