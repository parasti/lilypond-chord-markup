# lilypond-chord-markup
LilyPond markup commands for displaying chords and making chord/lyric sheets

## Usage

### Chords and lists of chords
```lilypond
\markup \line { Intro: \chord d:maj7 \chord e:m \chord c:1.5.8 }
\markup \line { Intro: \chord { d:maj7 e:m c:1.5.8 } }
```

### Chord over a lyric
```lilypond
\markup { \with-chord f This is the \with-chord a/e last time I'll \concat { a \with-chord d:m bandon } you }
```
Or use `\ch` instead of `\with-chord`.

### Fixing overlap
Chords use lyric dimensions and may sometimes overlap successive chords. To fix it, use chord dimensions:
```lilypond
\markup { \override #'(full-extent . 1) \with-chord fis:m I \with-chord e:m lie. }
```
Or use `\chf` instead of `\override #'(full-extent . 1) \with-chord`

### Transposing chords
You can transpose chords in existing markup. Use this with `\book` to make chord sheets in different keys:
```lilypond
\markup \override #'(transpose . ("c" . "f")) \existingChordMarkup
\markuplist ... \existingChordMarkupList
```
Or use `\transpose c f \existingChordMarkup`.

### Formatting options
Color, font, etc, can be controlled via `\layout`:
```lilypond
\layout {
  \override ChordNames.ChordName.font-size = #0.0
  \override ChordNames.ChordName.color = #red
}
```

### Arbitrary text
You can combine chords over lyrics with arbitrary text (empty in the example). Use this for instructions, parenthesis, etc.
```lilypond
\markup \with-chord d:m youuuuu \with-chord-text (2x) " "
```
Or use `\cht` instead of `\with-chord-text`.
