<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [

<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>

<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
]]>

<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
]]>

]>

<style-sheet>
<style-specification id="print" use="common docbook">
<style-specification-body>

(define %generate-reference-toc% 
  ;; Should a Table of Contents be produced for References?
  #f)

(define (toc-depth nd)
  ;; make table of contents:
  ;; 1-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 2)
        ((string=? (gi nd) (normalize "set")) 2)
        (else 3)))

(define (make-divider)
    (make rule 
          orientation: 'horizontal
          line-thickness: 1pt))

(define (make-linebreak)
    (make paragraph-break
          space-before: 6pt))

(define (printed-link)
    (let* ((id (attribute-string "LINKEND"))
           (nl (element-with-id id)))
      (sosofo-append
       (literal (id-to-indexitem id))
       ($italic-seq$ 
        (sosofo-append
         (literal " (see page ")
         (element-page-number-sosofo nl)
         (literal ")"))))))

(element LINK (printed-link))

(element ulink 
  ;; make URL appear in parentheses in printed version
  (sosofo-append
   (process-children)
   (let* ((url-string (attribute-string (normalize "url"))))
     (sosofo-append
      ($italic-seq$ 
        (sosofo-append
         (literal " (")
         (literal url-string) 
         (literal ") ")))))))

(element (REFSECT1 PARA LINK)
         (make paragraph 
               (printed-link)))

(element PRIMARYIE
         (let* ((linkends-string (attribute-string "LINKENDS"))
                (linkends (split-string linkends-string #\space)))
           (if (type-id-p (car linkends) "METHOD")
               (sosofo-append
                (process-children)
                (make sequence
                      (let loop ((linkends linkends))
                           (if (null? linkends)
                               (empty-sosofo)
                               (sosofo-append
                                (make paragraph
                                      start-indent: 72pt
                                      (let* ((id (car linkends))
                                             (nl (element-with-id id)))
                                        (sosofo-append
                                         (literal (id-to-indexitem id))
                                         (literal " -- ")
                                         (element-page-number-sosofo nl))))
                                (loop (cdr linkends)))))))
               (sosofo-append
                (process-children)
                (literal " --")
                (let loop ((linkends linkends))
                     (if (null? linkends)
                         (empty-sosofo)
                         (sosofo-append
                          (literal " ")
                          (element-page-number-sosofo 
                           (element-with-id (car linkends)))
                          (loop (cdr linkends)))))))))

;; These are defined with skip-content so that they
;; can be expanded and have their ID recorded (and won't
;; be excluded from the index.
(define (skip-content)
    (make sequence
          (process-node-list (children (current-node)))))
(element (listitem programlisting) (skip-content))
(element (listitem screen) (skip-content))
(element (listitem synopsis) (skip-content))
(element (listitem funcsynopsis) (skip-content))
(element (listitem literallayout) (skip-content))
(element (listitem address) (skip-content))
(element (listitem para) (skip-content))
(element (listitem formalpara) (skip-content))

(define (generic-list-item indent-step line-field)
  (let* ((itemcontent (children (current-node)))
         (first-child (node-list-first itemcontent))
         (spacing (inherited-attribute-string (normalize "spacing"))))
    (make sequence
      start-indent: (+ (inherited-start-indent) indent-step)
      (make paragraph
        use: (cond
              ((equal? (gi first-child) (normalize "programlisting"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "screen"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "synopsis"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "funcsynopsis"))
               verbatim-style)
              ((equal? (gi first-child) (normalize "literallayout"))
               linespecific-style)
              ((equal? (gi first-child) (normalize "address"))
               linespecific-style)
              (else
               para-style))
        space-before: (if (equal? (normalize "compact") spacing)
                          0pt
                          %para-sep%)
        first-line-start-indent: (- indent-step)
        (make sequence
          use: para-style
          line-field)
        (process-node-list first-child))
      (process-node-list (node-list-rest itemcontent))))) 

(define (funcprototype)
    (let ((paramdefs (select-elements (children (current-node)) (normalize "paramdef"))))
      (make sequence
            (process-children)
            (if (equal? %funcsynopsis-style% 'kr)
                (with-mode kr-funcsynopsis-mode
                  (process-node-list paramdefs))
                (empty-sosofo)))))

(define (revhistory)
    (sosofo-append
     (let* ((tlevel 2)
            (hs (HSIZE (- 3 tlevel))))
       (make paragraph
             font-family-name: %title-font-family%
             font-weight: 'bold
             font-size: hs
             line-spacing: (* hs %line-spacing-factor%)
             space-before: (* hs %head-before-factor%)
             space-after: (* hs %head-after-factor%)
             quadding: 'start
             keep-with-next?: #t
             heading-level: (if %generate-heading-level% (+ tlevel 2) 0)
             (literal "Revision History")))
     (process-children)))

(element abstract (make sequence (process-children)))

</style-specification-body>
</style-specification>

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

