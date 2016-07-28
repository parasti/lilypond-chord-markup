\version "2.18.2"

#(define QUOTE "\"")

#(define-markup-command (put-left layout props arg1 arg2) (markup? markup?)
   (interpret-markup layout props
     #{\markup \put-adjacent #X #LEFT #arg2 #arg1 #}))

#(define-markup-command (quoth layout props arg) (markup?)
   (interpret-markup layout props #{ \markup \concat { #QUOTE #arg #QUOTE } #}))