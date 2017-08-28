\version "2.18.2"

% (c) 2014-2017 Jānis Rūcis


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


#(define-markup-command (with-chord-text layout props text lyric) (markup? markup?)
   ; We use the extent of the lyric, but sometimes the chord overlaps successive chords.
   ; To use the full extent of the combined stencil, use this:
   ;   \override #'(full-extent . 1)
   ;   \with-chord c lyric
   ; TODO If I actually knew Scheme, I could check for overlap.
   #:properties ((full-extent 0))
   "Arbitrary text superimposed over a lyric."
   (let* ((mup #{ \markup
                  \override #'(direction . 1)
                  \override #'(baseline-skip . 2)
                  \dir-column { #lyric #text } #}) ; Used twice, save typing.
          (mstil (interpret-markup layout props mup)) ; Combined stencil
          (lstil (interpret-markup layout props lyric)) ; Lyric stencil
          (x (ly:stencil-extent (if (equal? full-extent 1) mstil lstil) X)) ; Desired X extent
          (y (interval-widen (ly:stencil-extent mstil Y) 0.25))) ; Combined Y extent, some padding
     (interpret-markup layout props #{ \markup \with-dimensions #x #y #mup #})))


#(define-markup-command (with-chord layout props chord lyric) (markup? markup?)
   "Chord superimposed over a lyric."
   (interpret-markup layout props #{ \markup \with-chord-text \chord #chord #lyric #} ))

%----------
% Shortcuts
%----------

#(define-markup-command (ch layout props chord lyric) (markup? markup?)
   "Shortcut for \\markup \\with-chord chord lyric"
   (interpret-markup layout props #{ \markup \with-chord #chord #lyric #}))


#(define-markup-command (chf layout props chord lyric) (markup? markup?)
   "Shortcut for \\markup \\override #'(full-extent . 1) \\with-chord chord lyric"
   (interpret-markup layout props #{ \markup \override #'(full-extent . 1) \with-chord #chord #lyric #}))


#(define-markup-command (cht layout props text lyric) (markup? markup?)
   "Shortcut for \\markup \\with-chord-text text lyric"
   (interpret-markup layout props #{ \markup \with-chord-text #text #lyric #}))


#(define-markup-command (transpose layout props from to arg) (markup? markup? markup?)
   "Shortcut for \\markup \\override #'(transpose . (\"from\" . \"to\")) chords"
   (interpret-markup layout (prepend-alist-chain 'transpose (cons (markup->string from) (markup->string to)) props) arg))
