<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0">

 <xsl:import href="http://docbook.sourceforge.net/release/xsl/current/html/chunk.xsl"/>

 <xsl:param name="use.id.as.filename" select="'1'"/>

</xsl:stylesheet>

<!--

(define %html-ext% ".html")



(define %use-id-as-filename% #t)

(define %generate-legalnotice-link%
  ;; Should legal notices be a link to a separate file?
  #t)

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

(element primaryie
         (let* ((linkends-string (attribute-string "linkends"))
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

(define (set-html-base nd)
  (let ((number (number->string (all-element-number nd)))
        ;(number (pad-string (number->string 3) 2 "0"))
        (pibase (inherited-pi-value nd "html-basename"))
        (idbase (if (and %use-id-as-filename%
                         (attribute-string (normalize "id") nd))
                    (case-fold-down (attribute-string (normalize "id") nd))
                    #f)))
    (if idbase
        idbase
        (string-append (if pibase pibase "set") number))))

(define (html-file #!optional (input_nd (current-node)))
  (let* ((nd (chunk-parent input_nd))
         (base (cond ((member (gi nd) (book-element-list))
                      (book-html-base nd))
                     ((member (gi nd) (division-element-list))
                      (division-html-base nd))
                     ((member (gi nd) (component-element-list))
                      (component-html-base nd))
                     ((member (gi nd) (section-element-list))
                      (section-html-base nd))
                     ((equal? (gi nd) (normalize "set"))
                      (set-html-base nd))
                     (else "xxx1")))
         ;; If this chunk-level element isn't a chunk, get the pifile from
         ;; the parent element.
         (pifile (if (chunk? nd)
                     (pi-value nd "html-filename")
                     (pi-value (parent nd) "html-filename")))
         (pidir (inherited-pi-value nd "html-dir")))
    (if pifile
        (if pidir
            (string-append pidir "/" pifile)
            pifile)
        (if pidir
            (string-append pidir "/" base %html-ext%)
            (string-append base %html-ext%))))) 

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
;; ancestor-members by removing `refentry' from the list - this
;; ensures that that `reference' is correctly intepreted as the
;; ancestor of `refentry' and correctly generates reference title as
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

(define (pubdate)
         (make element gi: "DIV"
               (literal "Published ")
               (process-children)))

;; defined in dbttlpg.dsl to not use $img$.
(define (graphic) (make element gi: "P" ($img$)))
(mode book-titlepage-recto-mode (element graphic (graphic)))
(mode set-titlepage-recto-mode (element graphic (graphic)))

;; modified from html/dbttlpg.dsl  
(define (copyright)
    (let ((years (select-elements (descendants (current-node))
				  (normalize "year")))
	  (holders (select-elements (descendants (current-node))
				    (normalize "holder")))
	  (legalnotice (select-elements (children (parent (current-node)))
					(normalize "legalnotice"))))
      (make element gi: "P"
	    attributes: (list
			 (list "CLASS" (gi)))
	    (if (and %generate-legalnotice-link%
		     (not nochunks)
		     (not (node-list-empty? legalnotice)))
		(make sequence
		  (make element gi: "A"
			attributes: (list 
				     (list "HREF" 
					   ($legalnotice-link-file$
					    (node-list-first legalnotice))))
			(literal (gentext-element-name (gi (current-node)))))
		  (literal " ")
		  (literal (dingbat "copyright"))
		  (literal " ")
		  (process-node-list years)
		  (literal (string-append " ")); (gentext-by) " "))
		  (process-node-list holders))
		(make sequence
		  (literal (gentext-element-name (gi (current-node))))
		  (literal " ")
		  (literal (dingbat "copyright"))
		  (literal " ")
		  (process-node-list years)
		  (literal (string-append " " )); (gentext-by) " "))
		  (process-node-list holders))))))

;; from html/dbttlpg.dsl
(define (legalnotice) 
  (let ((notices (select-elements 
                  (descendants (parent (current-node)))
		    (normalize "legalnotice"))))
    (if (and %generate-legalnotice-link%
             (not nochunks))
        ;; Divert the contents of legal to another file.  It will be xref'd
        ;; from the Copyright.
        (if (first-sibling? (current-node))
            (make entity
              system-id: (html-entity-file
                          ($legalnotice-link-file$ (current-node)))
              (if %html-pubid%
                  (make document-type
                    name: "HTML"
                    public-id: %html-pubid%)
                  (empty-sosofo))
              (make element gi: "HTML"
                    (make element gi: "HEAD"
			    ($standard-html-header$))
                    (make element gi: "BODY" 
			    attributes: %body-attr%
			    (header-navigation (current-node))
                            (with-mode  book-titlepage-verso-mode 
                              ($semiformal-object$))
			    (with-mode ;book-titlepage-verso-mode 
                                legal-notice-link-mode
			      (process-node-list (node-list-rest notices)))
			    (footer-navigation (current-node)))))
            (empty-sosofo))
        ($semiformal-object$))))

(define (set-titlepage-separator side)
  (if (equal? side 'recto)
      (make empty-element gi: "HR")
      (empty-sosofo)))

(element book
  (let* ((bookinfo  (select-elements (children (current-node)) (normalize "bookinfo")))
         (ititle   (select-elements (children bookinfo) (normalize "title")))
         (title    (if (node-list-empty? ititle)
                       (select-elements (children (current-node)) (normalize "title"))
                       (node-list-first ititle)))
         (nl       (titlepage-info-elements (current-node) bookinfo))
         (tsosofo  (with-mode head-title-mode
                     (process-node-list title)))
         (dedication (select-elements (children (current-node)) (normalize "dedication"))))
    (html-document
     tsosofo
     (make element gi: "DIV"
           attributes: '(("CLASS" "BOOK"))
           (if %generate-book-titlepage%
               (make sequence
                 (book-titlepage nl 'recto)
                 (book-titlepage nl 'verso))
               (empty-sosofo))

           (if (node-list-empty? dedication)
               (empty-sosofo)
               (with-mode dedication-page-mode
                 (process-node-list dedication)))

           (if (not (generate-toc-in-front))
               (process-children)
               (empty-sosofo))

           (if %generate-book-toc%
               (make sequence
                 (build-toc (current-node) (toc-depth (current-node))))
               (empty-sosofo))

           (let loop ((gilist ($generate-book-lot-list$)))
                (if (null? gilist)
                    (empty-sosofo)
                    (if (not (node-list-empty?
                              (select-elements (descendants (current-node))
                                               (car gilist))))
                        (make sequence
                              (build-lot (current-node) (car gilist))
                              (loop (cdr gilist)))
                        (loop (cdr gilist)))))
           
           (if (generate-toc-in-front)
               (process-children)
               (empty-sosofo)))))) 

(define (example-title text)
    (make element gi: "B"
          (literal text)))

(define (example-entry-text example-node)
    (make element gi: "LI"
          (make element gi: "A"
                attributes:
                (list 
                 (list "HREF"
                       (href-to example-node)))
                (literal (example-label example-node #f)))))

(define ($lot-entry$ tocentry)
    (sosofo-append
     (make element gi: "A"
           attributes:
           (list
            (list "HREF"
                  (href-to tocentry)))
           (element-title-sosofo tocentry))
     (make-linebreak)))

(define (lot-title first? lotgi)
  (if first?
      (sosofo-append
       (make-linebreak)
       (make element gi: "B"
             (literal ($lot-title$ lotgi)))
       (make-linebreak))
      (empty-sosofo)))

(define (make-list sosofo)
    (make element gi: "UL" sosofo))

(define (make-listitem level title contents)
    (make element gi: "LI" 
          title
          contents))

;; docinfo is only used in a titlepage mode - so make it empty here,
;; so it is not duplicated.
(element docinfo (empty-sosofo))

; use local definitions of copyright, legalnotice and pubdate to
; generate links to legalnotices properly.
(mode book-titlepage-verso-mode
  (element copyright (copyright))
  (element legalnotice (legalnotice))
  (element pubdate (pubdate))  
  (element (set book bookinfo copyright) (empty-sosofo))
  (element (set book bookinfo pubdate) (empty-sosofo))
  (element (set book bookinfo releaseinfo) (empty-sosofo))
  (element (set book bookinfo legalnotice) (empty-sosofo)))

; likewise for sets
(mode set-titlepage-verso-mode
  (element copyright (copyright))
  (element legalnotice (legalnotice))
  (element pubdate (pubdate)))

-->
  
<!-- 
Local variables:
mode: xml
sgml-default-dtd-file: "xsl.ced"
sgml-indent-step: 1
sgml-indent-data: 1
End:
-->
