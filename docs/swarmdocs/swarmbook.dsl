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

(define %graphic-extensions% 
  ;; List of graphic filename extensions
  '("gif" "jpg" "jpeg"  "ps" ))

(define (make-divider)
    (make rule 
          orientation: 'horizontal
          line-thickness: 1pt))

(define (make-linebreak)
    (make paragraph (empty-sosofo)))

(define $generate-lot-list$
  ;; Should a List of Titles be produced? 
 ;; Which Lists of Titles should be produced for Books?
  (list (normalize "table")
        (normalize "figure")
        (normalize "example")
        (normalize "equation")))

(define (toc-depth nd)
  ;; make table of contents:
  ;; 1-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 1)
         ((string=? (gi nd) (normalize "set")) 2)
         (else 3))
  )

;; SET customization

(define %generate-set-toc% 
  ;; Should a Table of Contents be produced for Sets?
  #t)

;; customizing auto-labelling of SECTs

(define %section-autolabel% 
  ;; Are sections enumerated?
  #t)

(element PRIMARYIE
         (sosofo-append
          (process-children)
          (let* ((linkends-string (attribute-string "LINKENDS")))
            (let loop ((linkends (split-string linkends-string #\space)))
                 (if (null? linkends)
                     (empty-sosofo)
                     (sosofo-append
                      (make paragraph
                            (let ((id (car linkends)))
                              (sosofo-append
                               (make link
                                     destination: (idref-address id)
                                     (literal
                                      (id-to-indexitem id)))
                               (literal " -- ")
                               (element-page-number-sosofo
                                (element-with-id id))))
                            (loop (cdr linkends)))))))))


</style-specification-body>
</style-specification>

<style-specification id="html" use="common docbook">
<style-specification-body> 

;; customize the html stylesheet

(define %stylesheet%
  ;; Name of the stylesheet to use. #f = don't make link to text/css in HTML
  #f)

(define ($generate-book-lot-list$)
  ;; Which Lists of Titles should be produced for Books?
  (list (normalize "table")
        (normalize "figure")
        (normalize "example")
        (normalize "equation")))

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

(define (toc-depth nd)
  ;; make table of contents:
  ;; 1-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 1)
         ((string=? (gi nd) (normalize "set")) 2)
         (else 3))
  )

;; customizing SET stuff...

(define %generate-set-titlepage%
  ;; Should a set title page be produced?
  #t)

(define %generate-set-toc% 
  ;; Should a Table of Contents be produced for Sets?
  #t)


(define (set-titlepage-verso-elements)
  ;; by default style sheet doesn't include the "verso" elements on the 
  ;; title page.
  (list (normalize "abstract")
        (normalize "biblioset")
        (normalize "releaseinfo")
        (normalize "revhistory")))

;; customizing BOOKs

(define %generate-book-titlepage%
  ;; Should a book title page be produced?
  #t)

(define (book-titlepage-verso-elements)
  ;; by default style sheet doesn't include the "verso" elements on the 
  ;; title page.
  (list (normalize "abstract")
        (normalize "releaseinfo")
        (normalize "bookbiblio")
        (normalize "revhistory")))

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

(define (make-linebreak)
   (make empty-element gi: "BR"))

(element PRIMARYIE
         (sosofo-append
          (process-children)
          (make element gi: "UL"
                (let* ((linkends-string (attribute-string "LINKENDS")))
                  (let loop ((linkends (split-string linkends-string #\space)))
                       (if (null? linkends)
                           (empty-sosofo)
                           (sosofo-append
                            (make element gi: "LI"
                                  (let ((id (car linkends)))
                                    (make element gi: "A"
                                          attributes:
                                          (list
                                           (list "HREF"
                                                 (href-to
                                                  (element-with-id id))))
                                          (literal
                                           (id-to-indexitem id)))))
                            (loop (cdr linkends)))))))))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common.dsl">
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>

