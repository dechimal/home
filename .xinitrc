#!/bin/zsh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap
wallpaper=$HOME/.wallpaper

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

# set im
export GTK_IM_MODULE=uim
export XMODIFIERS=@im=uim
uim-xim &

# start some nice programs

[[ ~/.xmonad/xmonad.o -ot ~/.xmonad/xmonad.hs ]] && xmonad --recompile

SKKSERVER=localhost
LANG=ja_JP.UTF-8
LC_CTYPE=ja_JP.UTF-8

# run xcompmgr for window transparency
xcompmgr &

cd $HOME
exec xmonad

# xclock -geometry 50x50-1+1 &
# xterm -geometry 80x50+494+51 &
# xterm -geometry 80x20+494-0 &
# exec xterm -geometry 80x66+0+0 -name login
