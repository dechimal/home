#!/bin/zsh

exec 2>~/.xerrlog

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
wallpaper=$HOME/.wallpaper
prehook=$HOME/.xinitrc.d/pre
posthook=$HOME/.xinitrc.d/post

# change keyboard layout
setxkbmap -layout jp

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# set default cursor (this command call after to merge xresources)
xsetroot -cursor_name left_ptr

# set wallpaper
if [ -f $wallpaper ]; then
    hsetroot -center $wallpaper
fi

# include local settings
if [ -f $prehook ]; then
    source $prehook
fi

export SKKSERVER=localhost
export LANG=ja_JP.UTF-8
export LC_CTYPE=ja_JP.UTF-8

# set im
export GTK_IM_MODULE=uim
export XMODIFIERS=@im=uim
uim-xim &

# start some nice programs

[[ ~/.xmonad/xmonad.o -ot ~/.xmonad/xmonad.hs ]] && xmonad --recompile

# run xcompmgr for window transparency
xcompmgr &

post() {
sleep 1
if [[ -f $posthook ]]; then
    source $posthook
fi
}

post &

cd $HOME
xmonad

# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login
