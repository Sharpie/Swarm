<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>
]>

<style-sheet>
<style-specification id="html" use="common docbook">
<style-specification-body> 

(define %html-ext% ".html")
(define %use-id-as-filename% #t)

(define %generate-legalnotice-link%
  ;; Should legal notices be a link to a separate file?
  #t)

(define %stylesheet%
  ;; Name of the stylesheet to use. #f = don't make link to text/css in HTML
  #f)

(define %shade-verbatim-attr% 
  ;; Attributes used to create a shaded verbatim environment.
  (list
   (list "BORDER" "1")
   (list "BGCOLOR" "#EOEOEO")
   (list "WIDTH" "70%")))

(define %shade-verbatim%
    ;; Should verbatim environments be shaded?
    #t) 

(define (toc-depth nd)
  ;; make table of contents:
  ;; 2-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 3)
        ((string=? (gi nd) (normalize "chapter")) 1)
        (else 3)))

(define %show-comments%
  ;; Display Comment elements?
  #t)

;; Make text that comes from unimplemented tags easy to spot
(element comment
  (make element gi: "FONT"
 	attributes: '(("COLOR" "GREEN"))
 	(process-children)))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">
</style-sheet>
  