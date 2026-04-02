#!/bin/sh

if test -z "$2"; then
    basedir=
else
    basedir="$(echo "$2" | sed 's,/$,,')/"
fi
LC_ALL=C 7z l "$1" | sed -r '1,/^-----/ d; /^-----/,$ d; s/^([^ ]* *){2}([^ ]*) *([^ ]* *){2}(.*)/\2,\4/' | while read -r i; do
    filename="$basedir$(echo "$i" | iconv -f cp932 | sed s/^[^,]*,//)"
    raw_filename="$basedir$(echo "$i" | sed s/^[^,]*,//)"
    echo "$i" | grep -q '^[^,]*D,'
    if [[ $? == 0 ]]; then
        mkdir -p "$filename"
    else
        mkdir -p "$(dirname "$filename")"
        LC_ALL=C 7z x -so "$1" "$raw_filename" >"$filename" 2>/dev/null
    fi
done
