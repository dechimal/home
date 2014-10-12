#!/bin/sh

if test -z "$2"; then
    basedir=
else
    basedir="$(echo "$2" | sed 's,/$,,')/"
fi
7z l "$1" | sed '1,/^-----/d;/^-----/,$d' | perl -ple 'use open OUT => ":encoding(ASCII)"; use Encode; $_ = Encode::decode_utf8($_)' | sed -r 's/^([^ ]* *){5}//' | while read i; do
    filename="$basedir$(echo "$i" | iconv -f CP932)"
    7z l "$1" "$(echo "$i" | iconv -f CP932)" | sed '1,/^-----/d;/^-----/,$d;s/  */ /g' | perl -ple 'use open OUT => ":encoding(ASCII)"; use Encode; $_ = Encode::decode_utf8($_)' | cut -d' ' -f3 | grep -q D;
    if [[ $? == 0 ]]; then
        mkdir -p $filename
    else
        mkdir -p "$(dirname "$filename")"
        7z e -so "$1" "$i" >"$filename" 2>/dev/null
    fi
done
