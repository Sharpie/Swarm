<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY website.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook Website Stylesheet//EN" CDATA DSSSL>
<!ENTITY xpointer.dsl PUBLIC "-//Norman Walsh//DOCUMENT DocBook XPointer Stylesheet//EN" CDATA DSSSL>
<!ENTITY % en.words
PUBLIC "-//Norman Walsh//ENTITIES DocBook Stylesheet Localization//EN">
%en.words;
]>

<style-sheet>
<style-specification id="org" use="website xpointer docbook">
<style-specification-body>

;; make a subtoc for each main item *only* on the homepage
(define %homepage-subtoc% #t)

;; Specify rules before and after an Example
(define %example-rules% #t)

;; make feedback go the "Contact Us!" page
(define feedback-href "contact.html#")

(define %admon-graphics% #t)
(define %admon-graphics-path% (string-append graphics-dir graphics-icons-subdir))
(define %html-ext% ".html")
(define %use-id-as-filename% #t)
(define %use-navbanner-images% #f)

(define %iconhome-filename% "sdg-small.jpeg")

(define %shade-verbatim-attr% 
  ;; Attributes used to create a shaded verbatim environment.
  (list
   (list "BORDER" "1")
   (list "BGCOLOR" "#EOEOEO")
   ;;(list "WIDTH" "70%")
   ))

(define %shade-verbatim%
    ;; Should verbatim environments be shaded?
    #t) 

;;; end simple options

(define ($bold-sanserif-seq$ #!optional (sosofo (process-children)))
  (make element gi: "FONT"
	attributes: (list (list "FACE" "Trebuchet, Arial, sans-serif")
                          (list "SIZE" "-1"))
	(make element gi: "B"
	      sosofo)))

(element application ($bold-sanserif-seq$))

(define %always-format-variablelist-as-table%
  ;; Always format VariableLists as tables?
  #t)

(define %may-format-variablelist-as-table%
  ;; Format VariableLists as tables?
  #t)

;; bibliographic entry stuff....

(define %biblioentry-in-entry-order% #f)
(define bibltable #t)

(element conftitle
  ($italic-seq$))

(element citetitle
  (let ((work (attribute-string (normalize "pubwork"))))
    (cond 
     ((equal? work (normalize "article"))
      (make sequence
        (literal (gentext-start-quote))
        (process-children-trim)
          (literal (gentext-end-quote))))
     (else ($italic-seq$)))))

;; below, not currently working

;;(element abbrev
;;  ($bold-seq$))

;; (element abstract
;;   (make sequence 
;;     ($bold-seq$ (literal "Abstract: "))
;;     (process-children))) 

;; (element seriesinfo
;;   (make sequence 
;;     ($bold-seq$ (literal "Series: "))
;;     (process-children))) 

(define ($gen-tip-image$)
  (make empty-element gi: "IMG"
        attributes: (list 
                     (list "SRC" 
                           (root-rel-path 
                            (string-append %admon-graphics-path% "tip.gif")))
                     (list "HSPACE" "5")
                     (list "ALT" "Active jobs"))))
(element firstterm
  (make sequence
    (let ((role (attribute-string (normalize "role"))))
      (if (equal? role "active-jobs")
          ($gen-tip-image$)
          (empty-sosofo)))
      (process-children)))

(element seglistitem
  (make sequence
    (make empty-element gi: "HR")
    (let ((role (attribute-string (normalize "role"))))
      (if (equal? role "active-jobs")
          ($gen-tip-image$)
          (empty-sosofo)))
    (process-children)))

(element variablelist
  (let ((role (attribute-string (normalize "role"))))
    (if (equal? role "programmers")
        (with-mode programmer-info-mode
          (process-children))
        (process-children))))

(mode programmer-info-mode

  (element term
    (let* ((elem (children (current-node)))
           (athr (select-elements elem (normalize "author")))
           (cathr (select-elements elem (normalize "corpauthor")))
           (ulnk (select-elements elem (normalize "ulink"))))
      (make sequence
        (if (not (node-list-empty? cathr))
            (make sequence
              (process-node-list cathr)
              (make empty-element gi: "BR"))
            (empty-sosofo))
        (if (not (node-list-empty? athr))
            (make sequence
              (process-node-list athr)
              (make empty-element gi: "BR"))
            (empty-sosofo))
        (if (not (node-list-empty? athr))
            (make sequence
              (process-node-list ulnk)
              (make empty-element gi: "BR"))
            (empty-sosofo)))))

  (element corpauthor
    (make sequence
      ($bold-italic-seq$ (literal "Group: "))
      (process-children)))
  
  (element address (process-children))

  (element affiliation 
    (let* ((elem (children (current-node)))
           (oname (select-elements elem (normalize "orgname")))
           (odiv (select-elements elem (normalize "orgdiv")))
           (other (node-list-filter-by-not-gi elem
                                              (list (normalize "orgname") 
                                                    (normalize "orgdiv")
                                                    (normalize "address")
                                                    (normalize "jobtitle")))))
      (if (and (node-list-empty? oname) (node-list-empty? odiv)
               (node-list-empty? other))
          (empty-sosofo)
          (make sequence
            ($bold-italic-seq$ (literal "Affiliation(s): "))
            (let loop ((nl odiv))
              (if (node-list-empty? nl)
                  (empty-sosofo)
                  (make sequence
                    (process-node-list (node-list-first nl))
                    (make empty-element gi: "BR")
                    (loop (node-list-rest nl)))))
            (process-node-list oname)
            (process-node-list other)))))
  
  (element author
    (let* ((elem (children (current-node)))
           (fname (select-elements elem (normalize "firstname")))
           (sname (select-elements elem (normalize "surname")))
           (affl (select-elements elem (normalize "affiliation")))
           (addr (select-elements (children (node-list-first affl))
                                  (normalize "address")))
           (jtitle (select-elements (children (node-list-first affl))
                                  (normalize "jobtitle")))           
           (otheraddr (node-list-filter-by-not-gi 
                       (children 
                        (node-list-first affl))
                       (list (normalize "address")))))
      (make sequence
        ($bold-italic-seq$ (literal "Contact: "))
        (process-node-list fname)
        (literal " ")
        (process-node-list sname)
        (literal " ")
        (if (not (node-list-empty? jtitle))
            (make sequence
              (literal ", ")
              (process-node-list jtitle))
            (empty-sosofo))
        (process-node-list addr)
        (with-mode biblioentry-inline-mode
          (make sequence
            (if (not (node-list-empty? otheraddr))
                (make empty-element gi: "BR")
                (empty-sosofo))))
        (process-node-list affl))))    

  (element (term ulink)
    (make element gi: "A"
          attributes: (list
                       (list "HREF" (attribute-string (normalize "url")))
                       (list "TARGET" "_top"))
          (if (node-list-empty? (children (current-node)))
              (literal (attribute-string (normalize "url")))
              (process-children))))
  
  (element phone
    (make sequence 
      ($italic-seq$ (literal "ph: "))
      (process-children)))
  
  (element fax
    (make sequence 
      ($italic-seq$ (literal "fax: "))
      (process-children)))
  
  (element varlistentry
    (make sequence
      (make empty-element gi: "HR")
      (process-children)))
  
  (element (varlistentry listitem)
    (make element gi: "P" 
          (make sequence
            ($bold-italic-seq$ (literal "Experience using Swarm:"))
            (process-children))))
  )



(define (homepage-header-navigation elemnode prev next prevsib nextsib)
  (let* ((navbanner (config-node elemnode "homebanner"))
	 (graphic   (attribute-string (normalize "value") navbanner))
	 (altval    (attribute-string (normalize "altval") navbanner))
	 (grpath   (root-rel-path graphics-dir elemnode)))
    (make sequence
      (make element gi: "CENTER"
            (make empty-element gi: "IMG"
                  attributes: (list (list "SRC" (string-append 
                                                 grpath
                                                 graphic))
                                    (list "ALT" altval))))
      (if (equal? (config-value elemnode "homerule") "no")
	  (empty-sosofo)
	  (make empty-element gi: "HR")))))

(define (href-has-graphic node)
  (let* ((navbanner (config-node node "banner"))
         (graphic (attribute-string (normalize "value") navbanner)))
    (if (and graphic %use-navbanner-images%)
        graphic
        #f)))

(define (make-href-graphic node)
  (let* ((graphic (href-has-graphic node))
         (altval  (data (webpage-title-element node)))
         (grpath   (root-rel-path graphics-dir node))
         (iconpath (string-append grpath graphics-icons-subdir)))
    (make empty-element gi: "IMG"
          attributes: (list (list "SRC" (string-append iconpath graphic))
                            (list "ALT" altval)
                            (list "ALIGN" "BOTTOM")
                            (list "BORDER" "0")))))

(define (make-sep node row)
  (let* ((grpath   (root-rel-path graphics-dir node))
         (iconpath (string-append grpath graphics-icons-subdir)))
    (if (href-has-graphic node)
        (make sequence
          (make entity-ref name: "nbsp"))
        (if (equal? row 1)
            (make sequence
              (make entity-ref name: "nbsp")
              (literal "|")
              (make entity-ref name: "nbsp"))
            (make empty-element gi: "IMG"
                  attributes: (list 
                               (list "SRC" 
                                     (string-append 
                                      iconpath 
                                      "iconsmallrightarrow.gif"))
                               (list "ALT" "==>")
                               (list "ALIGN" "BOTTOM")
                               (list "BORDER" "0")))))))

(define (webpage-href node #!optional (sosofo #f))
  (make element gi: "A"
        attributes: (list (list "HREF" (href-to node)))
        (if (href-has-graphic node)
            (make-href-graphic node)
            (if (sosofo? sosofo)
                sosofo
                (with-mode title-mode
                  (process-node-list (webpage-title-element node)))))))

(define (webpage-title-element #!optional (node (current-node)))
  (let* ((head (select-elements (children node) (normalize "head")))
	 (title (select-elements (children head) (normalize "title"))))
    title))

(define (make-navbar row elemnode webpages #!optional (startpage #f))
  (let* ((grpath   (root-rel-path graphics-dir elemnode))
	 (iconpath (string-append grpath graphics-icons-subdir)))	       
    (make element gi: "DIV"
	  attributes: '(("CLASS" "NAVBAR"))

	  (case row
	    ((1)
	     (make sequence
	       (make element gi: "A"
		     attributes: (list 
				  (list "HREF" (href-to (nav-home elemnode))))
		     (make empty-element gi: "IMG"
			   attributes: (list (list "SRC" (string-append iconpath %iconhome-filename% ))
					     (list "ALT" "Home")
					     (list "ALIGN" "BOTTOM")
					     (list "BORDER" "0"))))
               ;; base separator generation node *after* the home
               ;; navbar
               (let loop ((nl webpages))
                 (make-sep (node-list-first nl) row))))
            ((2)
             (make sequence
               (webpage-href startpage)
               (make-sep elemnode row))))
          
	  (let loop ((nl webpages))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(sosofo-append
		 (let* ((title-sosofo
			 (with-mode title-mode
			   (process-node-list (webpage-title-element 
					       (node-list-first nl)))))
			(isat (node-list=? (node-list-first nl) elemnode))
			(islink (and (not isat))))
		   (make sequence
		     (if islink
			 (webpage-href (node-list-first nl) title-sosofo)
                         (if (href-has-graphic elemnode)
                             (make-href-graphic elemnode)
                             title-sosofo))
                     (if isat
			 (make empty-element gi: "IMG"
			       attributes: (list (list "SRC" (string-append iconpath "iconat.gif"))
						 (list "ALT" "@")
						 (list "ALIGN" "BOTTOM")
						 (list "BORDER" "0")))
			 (empty-sosofo))

		     (if (node-list-empty? (node-list-rest nl))
			 (empty-sosofo)
			 (make-sep (node-list-first nl) row))

		     (loop (node-list-rest nl))))))))))

(define (division-html-base nd)
  (let* ((number  (number->string (all-element-number nd)))
	 (prefix  (inherited-dbhtml-value nd "prefix"))
         (idbase (id-based-filename nd))
	 (base    (cond ((equal? (gi nd) (normalize "article"))      "t")
			((equal? (gi nd) (normalize "refentry"))     "r")
			((equal? (gi nd) (normalize "homepage"))     "h")
			((equal? (gi nd) (normalize "webpage"))      "w")
			;; "x" is section
			(else "z"))))
    (if idbase
        (string-append (if prefix prefix "") idbase)
        (string-append (if prefix prefix "") base number))))


(element webtoc
  (let* ((component (ancestor-member (current-node) 
				     (component-element-list))))
    (if (equal? (gi component) (normalize "homepage"))
        (empty-sosofo)
;;  	(summary-table-toc (select-elements
;;                             (children (parent component))
;;                            (normalize "webpage")) #t)
	(summary-table-toc (select-elements
                            (children component)
                            (normalize "webpage"))))))

(define (webtoc-entry elem #!optional (startpage #f))
  (make element gi: "DIV"
        attributes: '(("CLASS" "TOCENTRIES"))
        (make sequence
          (make element gi: "A"
                attributes: (list 
                             (list "HREF"
                                   (href-to 
                                    (node-list-first elem)))) 
                (if (and (href-has-graphic 
                          (node-list-first elem)) startpage)
                    (make-href-graphic (node-list-first elem))
                    (with-mode title-mode
                      (process-node-list 
                       (webpage-title-element (node-list-first elem))))))
          (if (and startpage %homepage-subtoc%)
              (make element gi: "DIV"
                    attributes: '(("CLASS" "SUBTOCENTRIES"))
                    (toc (select-elements
                          (children (node-list-first elem))
                          (normalize "webpage"))))
              (make empty-element)))))

(define (summary-table-toc elements #!optional (startpage #f))
  (make element gi: "TABLE"
	attributes: '(("BORDER" "0"))
	(let loop ((nl elements))
	  (if (node-list-empty? nl)
	      (empty-sosofo)
	      (make sequence
;;                 (if startpage
;;                     (empty-sosofo)
;;                     (make element gi: "TR"
;;                           (make sequence
;;                             (make element gi: "TD"
;;                                   (make empty-element gi: "HR"))
;;                             (make element gi: "TD"
;;                                   (make empty-element gi: "HR")))))
		(make element gi: "TR"
                      attributes: '(("VALIGN" "TOP"))
                      (make element gi: "TD"
                            attributes: '(("ALIGN" "LEFT"))
                            (webtoc-entry nl startpage))
                      (if startpage
                          (empty-sosofo)
                          (let* ((head (select-elements 
                                        (children (node-list-first nl))
                                        (normalize "head")))
                                 (abs  (select-elements
                                        (children head)
                                        (normalize "abstract"))))
                            (make element gi: "TD"
                                  attributes: '(("ALIGN" "LEFT")
                                                ("VALIGN" "TOP"))
                                  (process-node-list abs)))))
                (loop (node-list-rest nl)))))))

(element homepage
  (let* ((homepg (current-node))
	 (desc (config-value homepg "desc")))
    (make sequence
      (html-document
       (make sequence
	 (literal desc " - ")
	 (with-mode title-mode (process-node-list (webpage-title-element))))
     
       (let* ((elem (node-list-filter-by-not-gi (children (current-node))
                                                (list (normalize "head")
                                                      (normalize "webtoc")))))
         (make element gi: "TABLE"
               attributes: '(("BORDER" "0"))
               (make element gi: "TR"
                     attributes: '(("VALIGN" "TOP"))
                     (make sequence
                       (make element gi: "TD"
                             (summary-table-toc (select-elements
                                                 (children (parent (ancestor-member (current-node) (component-element-list)))) (normalize "webpage")) #t))
                       (make element gi: "TD"
                             (process-node-list elem))))))
       )
      ($sitemap$))))

(define ($sitemap$)
  (let* ((head (select-elements (children (current-node)) "HEAD"))
	 (title (select-elements (children head) "TITLE"))
	 (desc  (config-value (current-node) "desc"))
	 (pages (node-list (current-node)
			   (select-elements (children (sgml-root-element))
					    "webpage")))
	 (scripts (select-elements (children head) "script"))
	 (styles (select-elements (children head) "style")))
    (make entity
      system-id: "sitemap.html"
      (make document-type
	name: "HTML"
	public-id: %html-pubid%)
      (make element gi: "HTML"
	    (make element gi: "HEAD"
		  (make element gi: "TITLE"
			(make sequence
			  (with-mode title-mode
			    (process-node-list title))
			  (literal " Site Map")))
		  (process-node-list styles)
		  (process-node-list scripts)
		  ($standard-html-header$))
            (make element gi: "BODY" 
                  attributes: %body-attr%
                  (webpage-header-navigation (sgml-root-element)
                                             (empty-node-list)
                                             (empty-node-list)
                                             (empty-node-list)
                                             (empty-node-list))
                  (make element gi: "H1"
                        (make sequence
                          (with-mode title-mode
                            (process-node-list title))
                          (literal " Site Map")))
                  (make element gi: "DL"
                        (let loop ((nl pages))
                          (if (node-list-empty? nl)
                              (empty-sosofo)
                              (make sequence
                                ($sitemap-page$ (node-list-first nl))
                                (loop (node-list-rest nl))))))
                  
                  (webpage-footer-navigation (empty-node-list)
                                             (empty-node-list) 
                                             (empty-node-list)
                                             (empty-node-list) 
                                             (empty-node-list)))))))

(define (en-xref-strings)
  (list (list (normalize "appendix")    (if %chapter-autolabel%
					    "&Appendix; %n"
					    "%t"))
	(list (normalize "article")     (string-append %gentext-en-start-quote%
						       "%t"
						       %gentext-en-end-quote%))
	(list (normalize "bibliography") "%t")
	(list (normalize "equation")    "&Equation; %n")
	(list (normalize "example")     "&Example; %n")
	(list (normalize "figure")      "&Figure; %n")
	(list (normalize "glossary")    "%t")
	(list (normalize "index")       "%t")
	(list (normalize "listitem")    "%n")
	(list (normalize "part")        "&Part; %n")
	(list (normalize "preface")     "%t")
	(list (normalize "procedure")   "&Procedure; %n, %t")
	(list (normalize "reference")   "&Reference; %n, %t")
	(list (normalize "section")     (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sect1")       (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sect2")       (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sect3")       (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sect4")       (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sect5")       (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "simplesect")  (if %section-autolabel%
					    "&Section; %n"
					    "%t"))
	(list (normalize "sidebar")     "%t")
	(list (normalize "step")        "&step; %n")
	(list (normalize "table")       "&Table; %n")
        (list (normalize "webpage")     "%t")
        ))

(define (auto-xref-indirect? target ancestor)
  #f)

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">
<external-specification id="xpointer" document="xpointer.dsl">
<external-specification id="website" document="website.dsl">

</style-sheet>
