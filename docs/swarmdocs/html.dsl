<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>
]>

<style-sheet>
<style-specification id="html" use="common docbook">
<style-specification-body> 

(define %html-ext% ".html")
(define %use-id-as-filename% #t)

(define %stylesheet%
  ;; Name of the stylesheet to use. #f = don't make link to text/css in HTML
  #f)

(define %generate-reference-toc% 
  ;; Should a Table of Contents be produced for References?
  #t)

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
  ;; 1-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 1)
        ((string=? (gi nd) (normalize "set")) 2)
        (else 3)))

(define (make-divider)
    (make empty-element gi: "HR"
          attributes: (list (list "SIZE" "1"))))

(define (make-linebreak)
    (make empty-element gi: "BR"))

(element PRIMARYIE
         (let* ((linkends-string (attribute-string "LINKENDS"))
                (linkends (split-string linkends-string #\space)))
           (if (type-id-p (car linkends) "METHOD")
               (sosofo-append
                (process-children)
                (make element gi: "UL"
                      (let loop ((linkends linkends))
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
                                (loop (cdr linkends)))))))
               (let loop ((linkends linkends))
                    (if (null? linkends)
                        (empty-sosofo)
                        (sosofo-append
                         (let ((id (car linkends)))
                           (sosofo-append
                            (make element gi: "A"
                                  attributes:
                                  (list
                                   (list "HREF"
                                         (href-to
                                          (element-with-id id))))
                                  (literal
                                   (id-to-indexitem id)))
                            (make-linebreak)))
                         (loop (cdr linkends))))))))

(define (funcprototype)
  (let ((paramdefs (select-elements (children (current-node)) (normalize "paramdef"))))
    (make sequence
      (make element gi: "P"
            (make element gi: "CODE"
                  (process-children)
                  (if (equal? %funcsynopsis-style% 'kr)
                      (with-mode kr-funcsynopsis-mode
                        (process-node-list paramdefs))
                      (empty-sosofo)))))))

(element type
         (make element gi: "PRE"
               (process-children)))

;; chunk-element-list and book-element-list are needed
;; in order to make html-document put revhistory in 
;; it's own file.
(define (chunk-element-list)
  (list (normalize "preface")
        (normalize "chapter")
        (normalize "appendix")
        (normalize "article")
        (normalize "glossary")
        (normalize "bibliography")
        (normalize "index")
        (normalize "reference")
        (normalize "refentry")
        (normalize "part")
        (normalize "sect1")
        (normalize "book") ;; just in case nothing else matches...
        (normalize "set")  ;; sets are definitely chunks...
        (normalize "revhistory")))

(define (book-element-list)
    (list (normalize "book")
          (normalize "revhistory")))

;; want context-sensitive subtitles for 'refentry'(s) as well as
;; 'sect1'(s)
(define (nav-context? elemnode)
  (cond
   ((equal? (gi elemnode)
           (normalize "refentry")))
   ((equal? (gi elemnode)
           (normalize "sect1")))
   (else #f)))

;; override (component-element-list) only locally in the list of
;; ancestor-members by removing 'refentry' from the list - this
;; ensures that that 'reference' is correctly intepreted as the
;; ancestor of 'refentry' and correctly generates reference title as
;; the navigation subtitle

(define (nav-context elemnode)
  (let* ((component 
          (ancestor-member 
           elemnode
           (append (book-element-list) (division-element-list)
                   ;; override old (component-element-list)
                   (list (normalize "preface")
                         (normalize "chapter")
                         (normalize "appendix") 
                         (normalize "article")
                         (normalize "glossary")
                         (normalize "bibliography")
                         (normalize "index")
                         (normalize "reference")
                         (normalize "book")))))
	 (num  (if (node-list-empty? component)
                   0
                   (element-number component))))
    (if (nav-context? elemnode)
	(if (equal? (element-label component) "")
            (make sequence
	      (element-title-sosofo component))
	    (make sequence
	      (literal (gentext-element-name-space (gi component)))
	      (element-label-sosofo component)
	      (literal (gentext-label-title-sep (gi component)))
	      (element-title-sosofo component)))
	(empty-sosofo))))

(define (revhistory)
    (let ((title (id-to-indexitem (id (current-node)))))
      (sosofo-append
       (html-document 
        (literal title)
        (process-children))
       (make element gi: "A"
             attributes: (list
                          (list "HREF" (href-to (current-node))))
             (literal title)))))

(element pubdate
         (make element gi: "DIV"
               (literal "Publication Date ")
               (process-children)))

(define ($img$ #!optional (nd (current-node)) (alt #f))
    ;; overridden to handle public identifiers
    (let* ((fileref (attribute-string (normalize "fileref") nd))
           (entattr (attribute-string (normalize "entityref") nd))
           (gensysid (entity-generated-system-id entattr))
           (entityref (if entattr
                          (string-append "../figs/"
                                         (car (reverse (split-string gensysid #\/))))
                          #f))
           (format  (attribute-string (normalize "format")))
           (align   (attribute-string (normalize "align")))
           (attr    (append
                     (if align
                         (list (list "ALIGN" align))
                         '())
                     (if entityref
                         (list (list "SRC" (graphic-file entityref)))
                         (list (list "SRC" (graphic-file fileref))))
                     (if alt
                         (list (list "ALT" alt))
                         '()))))
      (if (or fileref entityref)
          (make empty-element gi: "IMG"
                attributes: attr)
          (empty-sosofo))))

;; defined in dbttlpg.dsl to not use $img$.
(define (graphic) (make element gi: "P" ($img$)))
(mode book-titlepage-recto-mode (element graphic (graphic)))
(mode set-titlepage-recto-mode (element graphic (graphic)))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">

</style-sheet>