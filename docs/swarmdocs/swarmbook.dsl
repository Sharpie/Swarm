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

;;
;; CUSTOMIZE THE PRINT STYLESHEET
;;

(define (make-divider)
    (make rule 
          orientation: 'horizontal
          line-thickness: 1pt))

(define (make-linebreak)
    (make paragraph-break
          space-before: 6pt))

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

(define (set-titlepage-verso-elements) 
 ;; by default style sheet doesn't include some "verso" elements on the 
 ;; title page.
  (list (normalize "title") 
	(normalize "subtitle") 
	(normalize "corpauthor") 
;	(normalize "authorgroup") 
;	(normalize "author") 
;	(normalize "editor")
;	(normalize "edition") 
	(normalize "pubdate") 
        (normalize "copyright")
	(normalize "legalnotice")         
        (normalize "abstract")
	(normalize "revhistory")
        ))

(mode set-titlepage-verso-mode

 (element copyright
  (make paragraph
    use: para-style
    (make sequence
      (literal (gentext-element-name (current-node)))
      (literal " ")
      (literal (dingbat "copyright"))
      (literal " ")
      (process-children-trim))))

  (element abstract 
    (make display-group
      (process-children)))
  )

;; BOOK customization

(define (book-titlepage-verso-elements)
  ;; by default style sheet doesn't include the "verso" elements on the 
  ;; title page.
 (list (normalize "title")
       (normalize "corpauthor")
       (normalize "pubdate")         
       (normalize "copyright")
       (normalize "legalnotice")
       (normalize "abstract")        
       (normalize "revhistory")        
       ))

(mode book-titlepage-verso-mode

 (element copyright
  (make paragraph
    use: para-style
    (make sequence
      (literal (gentext-element-name (current-node)))
      (literal " ")
      (literal (dingbat "copyright"))
      (literal " ")
      (process-children-trim))))
 )

;; REFERENCE customization

(define %generate-reference-titlepage% 
  ;; Should a reference title page be produced?
  #t)

(define (reference-titlepage-recto-elements)
  (list (normalize "title") 
        (normalize "subtitle")
        ))

(define (reference-titlepage-verso-elements)
  (list (normalize "abstract")
        (normalize "revhistory")))

(mode reference-titlepage-recto-mode)

(mode reference-titlepage-verso-mode

  (element abstract
    (make display-group
      start-indent: (+ (inherited-start-indent) 0.25in)
      end-indent: (+ (inherited-end-indent) 0.25in)
      font-size: (* %bf-size% 0.9)
      (process-children))))

;; ARTICLE customization

(define %generate-article-titlepage% 
  ;; Should an article title page be produced?
  #f)

;; customizing auto-labelling of SECTs

(define %section-autolabel% 
  ;; Are sections enumerated?
  #t)

;; other customizations

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

(element (REFSECT1 PARA LINK)
         (make paragraph 
               (printed-link)))

(element PRIMARYIE
         (sosofo-append
          (process-children)
          (make sequence
                (let* ((linkends-string (attribute-string "LINKENDS")))
                  (let loop ((linkends (split-string linkends-string #\space)))
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
                            (loop (cdr linkends)))))))))

         
(mode formal-object-title-mode
      (element (example title)
               (let ((example-node (parent (current-node))))
                 (make paragraph
                       use: para-style
                       font-weight: 'bold
                       space-before: %block-sep%
                       space-after: %para-sep%
                       start-indent: (+ %block-start-indent% (inherited-start-indent))
                       keep-with-next?: #t
                       (sosofo-append
                        (literal "Example")
                        (literal " ")
                        (literal (example-label example-node)))))))
      
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
            (make paragraph
                  font-family-name: %mono-font-family%
                  (process-children))
            (if (equal? %funcsynopsis-style% 'kr)
                (with-mode kr-funcsynopsis-mode
                  (process-node-list paramdefs))
                (empty-sosofo)))))

(element (bookinfo revhistory) ($revhistory$))

</style-specification-body>
</style-specification>

<style-specification id="html" use="common docbook">
<style-specification-body> 

;;
;; CUSTOMIZE THE HTML STYLESHEET
;;

(define %stylesheet%
  ;; Name of the stylesheet to use. #f = don't make link to text/css in HTML
  #f)

(define ($generate-book-lot-list$)
  ;; Which Lists of Titles should be produced for Books?
  (list (normalize "table")
        (normalize "figure")
        (normalize "example")
        (normalize "equation")))

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

(mode set-titlepage-recto-mode
        (element graphic
                 (make element gi: "P"
                       ($img$))))

(define ($img$ #!optional (nd (current-node)) (alt #f))
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

(mode book-titlepage-recto-mode
      (element graphic
               (make element gi: "P"
                     ($img$)))
)

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
  #t)

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

(element (example title)
         (let ((example-node (parent (current-node))))
           (make paragraph
                 keep-with-next?: #t
                 (sosofo-append
                  (literal "Example")
                  (literal " ")
                  (literal (example-label example-node))))))

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
        (normalize "revhistory")
        ))  

(define (book-element-list)
    (list (normalize "book")
          (normalize "revhistory")))

(element revhistory
         (let ((title (id-to-indexitem (id (current-node)))))
           (sosofo-append
            (html-document 
             (literal title)
             ($revhistory$))
            (make element gi: "A"
                  attributes: (list
                               (list "HREF" (href-to (current-node))))
                  (literal title)))))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">

</style-sheet>

<!--
Local variables:
mode: DSSSL
end:
-->
