<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>
]>
<style-sheet>
<style-specification id="print" use="common docbook">
<style-specification-body> 

(define %indent-programlisting-lines%
  ;; Indent lines in a 'ProgramListing'?
  " ")

(define formal-object-float
  ;; Do formal objects float?
  #t)

(define ($bold-sanserif-seq$ #!optional (sosofo (process-children)))
  (make sequence
    font-family-name: %admon-font-family%
    font-weight: 'bold
    sosofo))

(define ($admon-graphic$ #!optional (nd (current-node)))
  ;; Admonition graphic file
  (cond ((equal? (gi nd) (normalize "tip"))
         (string-append %admon-graphics-path% "tip.eps"))
        ((equal? (gi nd) (normalize "note"))
         (string-append %admon-graphics-path% "note.eps"))
        ((equal? (gi nd) (normalize "important"))
         (string-append %admon-graphics-path% "important.eps"))
        ((equal? (gi nd) (normalize "caution"))
         (string-append %admon-graphics-path% "caution.eps"))
        ((equal? (gi nd) (normalize "warning"))
         (string-append %admon-graphics-path% "warning.eps"))
        (else (error (string-append (gi nd) " is not an admonition.")))))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">
</style-sheet>
