\version "2.18.2"

% (c) 2014-2016 Jānis Rūcis
%
% A collection of cobbled-together LilyPond markup commands for chord sheets.
%
% Examples:
%  List of chords:
%    \markup \line { Intro: \chord d:maj7 \chord e:m \chord c:1.5.8 }
%  Chord over a lyric
%    \markup { \with-chord f This is the \with-chord a/e last time I'll \concat { a \with-chord d:m bandon } youuuuuu }
%    or use \ch instead of \with-chord
%  Chords use lyric dimensions and may sometimes overlap successive chords. To fix it, use chord dimensions:
%    \markup { \override #'(full-extent . 1) \with-chord fis:m I \with-chord e:m lie. }
%    or use \chf instead of \override %'(full-extent . 1) \with-chord
%  Transpose chords in existing markup. You can use this with \book to make chord sheets in different keys:
%    \markup \override #'(transpose . ("c" . "f")) \existingChordMarkup
%    \markuplist ... \existingChordMarkupList
%  Color, font, etc, can be controlled via \layout:
%    \layout {
%      \override ChordNames.ChordName.font-size = #0.0
%      \override ChordNames.ChordName.color = #red
%    }


#(define (string->music str)
   (let ((clone (ly:parser-clone parser)))
     (ly:parse-string-expression clone str))) % TODO filename and line number


#(define (parse-chords chords)
   (let ((str (string-concatenate (list "\\chordmode { " chords " }"))))
     (string->music str)))


#(define-markup-command (chord layout props chord)
   (markup?)
   #:properties ((transpose '("c" . "c")))
   "Interpret chord as chordmode input and display the result."
   (let ((mus (parse-chords chord))
         (from (string->music (car transpose)))
         (to (string->music (cdr transpose))))
     (interpret-markup layout props
       #{
         \markup {
           \left-align
           \score {
             % Spacer rest for consistent chord duration
             \new ChordNames \with {
               %\override ChordName.font-size = #0.0
               %\override ChordName.font-shape = #'italic
               %\override ChordName.color = #red
             }
             { s32 \transpose #from #to $mus }
             \layout { }
           }
         }
       #})))


#(define-markup-command (with-chord layout props chord lyric) (markup? markup?)
   ; We use the extent of the lyric, but sometimes the chord overlaps successive chords.
   ; To use the full extent of the combined stencil, use this:
   ;   \override #'(full-extent . 1)
   ;   \with-chord c lyric
   ; TODO If I actually knew Scheme, I could check for overlap.
   #:properties ((full-extent 0))
   "Single chord superimposed over a lyric."
   (let* ((mup #{ \markup
                  \override #'(direction . 1)
                  \dir-column { #lyric \chord #chord } #}) ; Used twice, save typing.
          (mstil (interpret-markup layout props mup)) ; Combined stencil
          (lstil (interpret-markup layout props lyric)) ; Lyric stencil
          (x (ly:stencil-extent (if (equal? full-extent 1) mstil lstil) X)) ; Desired X extent
          (y (interval-widen (ly:stencil-extent mstil Y) 0.5))) ; Combined Y extent, some padding
     (interpret-markup layout props #{ \markup \with-dimensions #x #y #mup #})))


#(define-markup-command (ch layout props chord lyric) (markup? markup?)
   "Shortcut for \\markup \\with-chord chord lyric"
   (interpret-markup layout props #{ \markup \with-chord #chord #lyric #}))


#(define-markup-command (chf layout props chord lyric) (markup? markup?)
   "Shortcut for \\markup \\override #'(full-extent . 1) \\with-chord chord lyric"
   (interpret-markup layout props #{ \markup \override #'(full-extent . 1) \with-chord #chord #lyric #}))
