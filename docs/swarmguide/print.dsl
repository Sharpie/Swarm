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

(define %example-rules%
  ;; Specify rules before and after an Example
  #t)

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">
</style-sheet>
