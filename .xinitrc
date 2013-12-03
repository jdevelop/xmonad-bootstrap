#!/bin/sh
 
# Launch WM
xmonad &
wmpid=$!
# Launch tray and statusbar
#xsetbg -center -border black images/IMG_4486.jpg -border '#090909'
xsetroot -cursor_name left_ptr -solid black
#fbsetroot -mod 16 16 -bg rgb:0/0/0 -fg rgb:10/18/20
#stalonetray -t -i 16 --icon-gravity E -geometry 48x16-0+0 -bg '#090909' -p --sticky --skip-taskbar &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --height 12 --transparent true --tint 0x000000 &
traypid=$!
~/xmonad-bin/dzen | dzen2 -u -e 'onstart=lower,uncollapse' -p \
-ta r -fn '-*-terminus-medium-r-*-*-12-*-*-*-*-*-koi8-r' \
-bg '#2e3436' -fg '#babdb6' -h 16 -w 1300 -x 400 &
dzenpid=$!
numlockx off &
xset b off
#xmodmap -e "keycode 96 = F12 Insert"
#cat /tmp/syndaemon.pid | xargs kill
#syndaemon -Ki 1 -d -p /tmp/syndaemon.pid
xxkb -w &
xxkbpid=$!
root-tail --noinitial -f -fn '-xos4-terminus-medium-r-*-*-12-*-*-*-*-*-koi8-r' -g 1800x700+0+0 /var/log/messages.log,darkgreen /var/log/exim/mainlog,green
wait $wmpid
kill -15 $xxkbpid $stardictpid $dzenpid $traypid
