#!/bin/bash

HEADER='
\include "chord-markup.ly"
\layout {
  \override ChordNames.ChordName.font-size = #0.0
  \override ChordNames.ChordName.color = #red
}
'

function runLilypond() {
  image=images/image"$1"

  lilypond -dpreview --png -o "$image" - &&
  mv "$image".preview.png "$image".png

  rm -f "$image".preview.png "$image".preview.eps
}

blockNum=0
while IFS="" read -r line; do
  if [ "$line" = '```lilypond' ]; then
    inBlock=true
    block=
  elif [ "$line" = '```' ]; then
    inBlock=false
    if [ "$blockNum" -gt 0 ]; then
      echo "$HEADER$block" | runLilypond "$blockNum"
    fi
    let blockNum++
  elif [ "$inBlock" = true ]; then
    block="$block $line"
  fi
done < README.md
