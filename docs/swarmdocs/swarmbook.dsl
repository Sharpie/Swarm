<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY common.dsl SYSTEM "common.dsl" CDATA DSSSL>
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl SYSTEM "/net/wijiji/disk2/src/docbook/html/docbook.dsl" CDATA DSSSL>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl SYSTEM "/net/wijiji/disk2/src/docbook/print/docbook.dsl" CDATA DSSSL>
]]>
]>

<style-specification id="print" use="common docbook">
<style-specification-body>

;; customize the print stylesheet

(define %generate-lot-list%
  ;; Should a List of Titles be produced?
  (list "TABLE" "FIGURE" "EXAMPLE" "EQUATION"))

(define %graphic-extensions% 
  ;; List of graphic filename extensions
  '("gif" "jpg" "jpeg"  "ps" ))

(define (make-divider)
    (make rule 
          orientation: 'horizontal
          line-thickness: 1pt))

(define (make-paragraph nl)
    (make paragraph
          (process-node-list nl)))

</style-specification-body>
</style-specification>

<style-specification id="html" use="common docbook">
<style-specification-body> 

;; customize the html stylesheet

(define %graphic-extensions% 
  ;; List of graphic filename extensions
  '("gif" "jpg" "jpeg"  "ps" ))

(define %shade-verbatim%  
  ;; Should verbatim environments be shaded?
  #t)

(define %shade-verbatim-attr% 
  ;; Attributes used to create a shaded verbatim environment.
  (list
   (list "BORDER" "1")
   (list "BGCOLOR" "#EOEOEO")
   (list "WIDTH" "70%")))

(define %use-id-as-filename%
  ;; Use ID attributes as name for component HTML files?
  #t)

;; customizing SET stuff...

(define %generate-set-titlepage%
  ;; Should a set title page be produced?
  #t)

(define %generate-set-toc% 
  ;; Should a Table of Contents be produced for Sets?
  #t)

;; customizing SECTs

(define %section-autolabel%
  ;; Are sections enumerated?
  #t)

;; customizing REFERENCE sections

(define %generate-reference-toc% 
  ;; Should a Table of Contents be produced for References?
  #t)

(define %generate-reference-titlepage% 
  ;; Should a reference title page be produced?
  #t)

(define %generate-partintro-on-titlepage%
  ;; Should the PartIntro appear on the Part/Reference title page?
  #f)

;; customizing REFENTRYs

(define %funcsynopsis-decoration%
  ;; Decorate elements of a FuncSynopsis?
  #t)

(define (make-divider)
    (make empty-element gi: "HR"
          attributes: (list (list "SIZE" "1"))))

(define (make-paragraph nl)
    (sosofo-append
     (make empty-element gi: "P")
     (process-node-list nl)))
  

</style-specification-body>
</style-specification>

<external-specification id="common" document="common.dsl">
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>

