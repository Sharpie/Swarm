<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook.dsl 
  PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY xpointer.dsl SYSTEM "xpointer.dsl" CDATA DSSSL>
]>

<!-- Version: $Id: website.dsl,v 1.1.1.1 2003-05-16 19:59:00 mgd Exp $ -->

<style-sheet>
<style-specification use="docbook xpointer">
<style-specification-body>

;; ======================================================================

(define graphics-dir "graphics/")
(define graphics-icons-subdir "icons/")

(define %admon-graphics% #t)
(define %admon-graphics-path% (string-append graphics-dir "admon/"))

;; The title of the web page is appended to this URL
(define feedback-href "/cgi-bin/feedback?TOPIC=")

;; ======================================================================
;; xlink

(define (external-xlink-embed xptr #!optional (nd (current-node)))
  (let* ((splitxptr (split xptr '(#\#)))
	 (tfile     (car splitxptr))
	 (txptr     (if (null? (cdr splitxptr))
			"root()"
			(car (cdr splitxptr))))
	 (targdoc   (sgml-parse tfile))
	 (targroot  (node-property 'document-element targdoc))
	 (targelem  (xpointer txptr targroot)))
    (process-node-list targelem)))

(define (external-xlink xptr #!optional (nd (current-node)))
  (let ((href (attribute-string (normalize "href") nd))
	(show (attribute-string (normalize "show") nd)))
    (if (equal? show "new")
	(make element gi: "A"
	      attributes: (list (list "HREF" href)
				(list "TARGET" "_new"))
	      (process-children))
	(make element gi: "A"
	      attributes: (list (list "HREF" href))
	      (process-children)))))

(define (internal-xlink-embed xptr #!optional (nd (current-node)))
  (let* ((target (my-debug (xpointer xptr))))
    (process-node-list target)))

(define (internal-xlink xptr #!optional (nd (current-node)))
  (let* ((target (xpointer xptr))
	 (href (attribute-string (normalize "href") nd))
	 (show (attribute-string (normalize "show") nd)))
    (if (equal? show "new")
	(make element gi: "A"
	      attributes: (list (list "HREF" (href-to target))
				(list "TARGET" "_new"))
	      (process-children))
	(make element gi: "A"
	      attributes: (list (list "HREF" (href-to target)))
	      (process-children)))))

(element xlink
  (let* ((href      (attribute-string (normalize "href")))
	 (show      (attribute-string (normalize "show")))
	 (internal? (and href 
			 (> (string-length href) 0)
			 (equal? (string-ref href 0) #\#))))
    (if internal?
	(if (equal? show "embed")
	    (internal-xlink-embed (substring href 1 (string-length href)))
	    (internal-xlink (substring href 1 (string-length href))))
	(if (equal? show "embed")
	    (external-xlink-embed href)
	    (external-xlink href)))))

;; ======================================================================

(define (SECTLEVEL #!optional (sect (current-node)))
  (if (equal? (gi sect) (normalize "simplesect"))
      ;; SimpleSect is special, it should be level "n+1", where "n" is
      ;; the level of the numbered section that contains it.  If it is
      ;; the *first* sectioning element in a chapter, make it 
      ;; %default-simplesect-level%
      (cond
       ((have-ancestor? (normalize "sect5")) 6)
       ((have-ancestor? (normalize "sect4")) 5)
       ((have-ancestor? (normalize "sect3")) 4)
       ((have-ancestor? (normalize "sect2")) 3)
       ((have-ancestor? (normalize "sect1")) 2)
       ((have-ancestor? (normalize "refsect3")) 5)
       ((have-ancestor? (normalize "refsect2")) 4)
       ((have-ancestor? (normalize "refsect1")) 3)
       (else %default-simplesect-level%))
      (cond
       ((equal? (gi sect) (normalize "sect5")) 6)
       ((equal? (gi sect) (normalize "sect4")) 5)
       ((equal? (gi sect) (normalize "sect3")) 4)
       ((equal? (gi sect) (normalize "sect2")) 3)
       ((equal? (gi sect) (normalize "sect1")) 2)
       ((equal? (gi sect) (normalize "refsect3")) 4)
       ((equal? (gi sect) (normalize "refsect2")) 3)
       ((equal? (gi sect) (normalize "refsect1")) 2)
       ((equal? (gi sect) (normalize "refsynopsisdiv")) 2)
       ((equal? (gi sect) (normalize "reference")) 1)
       (else 1))))

;; ========================================================================

(define %html-pubid% "-//Norman Walsh//DTD DocBook HTML 1.0//EN")

(define (chunk-element-list)
  (list (normalize "homepage") 
	(normalize "webpage")
	(normalize "refentry")))

(define (component-element-list) 
  (list (normalize "homepage") 
	(normalize "webpage")
	(normalize "refentry")))

(define (major-component-element-list) 
  (list (normalize "homepage") 
	(normalize "webpage")
	(normalize "refentry")))

(define (nav-home? elemnode)
  (not (equal? (gi elemnode) (normalize "homepage"))))

(define (nav-home elemnode)
  (select-elements (children (sgml-root-element)) (normalize "homepage")))

(define (division-html-base nd)
  (let* ((number  (number->string (all-element-number nd)))
	 (prefix  (inherited-dbhtml-value nd "prefix"))
	 (base    (cond ((equal? (gi nd) (normalize "article"))      "t")
			((equal? (gi nd) (normalize "refentry"))     "r")
			((equal? (gi nd) (normalize "homepage"))     "h")
			((equal? (gi nd) (normalize "webpage"))      "w")
			;; "x" is section
			(else "z"))))
    (string-append (if prefix prefix "") base number)))

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
		     (else (element-html-base input_nd))))
	 ;; If this chunk-level element isn't a chunk, get the pifile from
	 ;; the parent element.
	 (pifile (if (chunk? nd)
		     (config-value nd "filename")
		     (config-value (parent nd) "filename")))
	 (pidir (inherited-config-value nd "dir")))
    (if (and %root-filename% (node-list=? (sgml-root-element) nd))
	(string-append %root-filename% %html-ext%)
	(if pifile 
	    (if pidir
		(string-append pidir "/" pifile)
		pifile)
	    (if pidir
		(string-append pidir "/" base %html-ext%)
		(string-append base %html-ext%))))))

(define (header-navigation nd #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element nd)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element nd)
		    (list-ref navlist 1)))
	 (prevm (if (null? navlist)
		    (prev-major-component-chunk-element nd)
		    (list-ref navlist 2)))
	 (nextm (if (null? navlist)
		    (next-major-component-chunk-element nd)
		    (list-ref navlist 3)))
	 (rnavlist (list prev next prevm nextm)))
    (make sequence
      ($html-body-start$)
      (cond 
       ((equal? (gi nd) (normalize "homepage"))
	(homepage-header-navigation nd prev next prevm nextm))
       ((equal? (gi nd) (normalize "webpage"))
	(webpage-header-navigation nd prev next prevm nextm))
       ((equal? (gi nd) (normalize "article"))
	(article-header-navigation nd rnavlist))
       ((equal? (gi nd) (normalize "refentry"))
	(refentry-header-navigation nd rnavlist))
       ((equal? (gi nd) (normalize "index"))
	(index-header-navigation nd rnavlist))
       (else (default-header-navigation nd prev next prevm nextm)))
      ($user-header-navigation$ prev next prevm nextm)
      ($html-body-content-start$))))

(define (footer-navigation nd #!optional (navlist '()))
  (let* ((prev  (if (null? navlist)
		    (prev-chunk-element nd)
		    (list-ref navlist 0)))
	 (next  (if (null? navlist)
		    (next-chunk-element nd)
		    (list-ref navlist 1)))
	 (prevm (if (null? navlist)
		    (prev-major-component-chunk-element nd)
		    (list-ref navlist 2)))
	 (nextm (if (null? navlist)
		    (next-major-component-chunk-element nd)
		    (list-ref navlist 3)))
	 (rnavlist (list prev next prevm nextm)))
    (make sequence
      (make-endnotes)
      ($html-body-content-end$)
      ($user-footer-navigation$ prev next prevm nextm)
      (cond 
       ((equal? (gi nd) (normalize "homepage"))
	(homepage-footer-navigation nd prev next prevm nextm))
       ((equal? (gi nd) (normalize "webpage"))
	(webpage-footer-navigation nd prev next prevm nextm))
       ((equal? (gi nd) (normalize "article"))
	(article-footer-navigation nd rnavlist))
       ((equal? (gi nd) (normalize "refentry"))
	(refentry-footer-navigation nd rnavlist))
       ((equal? (gi nd) (normalize "index"))
        (index-footer-navigation nd rnavlist))
       (else (default-footer-navigation nd prev next prevm nextm)))
      (nav-footer nd)
      ($html-body-end$))))

;; ========================================================================

(define (config-node component pname)
  (let ((configs (select-elements (children component) "config")))
    (let loop ((nl configs))
      (if (node-list-empty? nl)
	  (empty-node-list)
	  (if (equal? (attribute-string (normalize "param")
					(node-list-first nl))
		      pname)
	      (node-list-first nl)
	      (loop (node-list-rest nl)))))))

(define (config-value component pname)
  (let ((config (config-node component pname)))
    (if (node-list-empty? config)
	#f
	(attribute-string (normalize "value") config))))

(define (inherited-config-value component pname)
  (let loop ((value #f) (nd component))
    (if (or value (node-list-empty? nd))
	value
	(loop (config-value nd pname) (parent nd)))))

(define (webpage-title-element #!optional (node (current-node)))
  (let* ((head (select-elements (children node) (normalize "head")))
	 (title (select-elements (children head) (normalize "title"))))
    title))

(define (homepage-header-navigation elemnode prev next prevsib nextsib)
  (let* ((navbanner (config-node elemnode "homebanner"))
	 (graphic   (attribute-string (normalize "value") navbanner))
	 (altval    (attribute-string (normalize "altval") navbanner))
	 (grpath   (root-rel-path graphics-dir elemnode)))
    (make sequence
      (make empty-element gi: "IMG"
	    attributes: (list (list "SRC" (string-append 
					   grpath
					   graphic))
			      (list "ALT" altval)))
      (if (equal? (config-value elemnode "homerule") "no")
	  (empty-sosofo)
	  (make empty-element gi: "HR")))))

(define (websection elem)
  (let loop ((node elem))
    (if (or (node-list-empty? node)
	    (node-list=? (parent node) (sgml-root-element)))
	node
	(loop (parent node)))))

(define (make-navbar row elemnode webpages #!optional (startpage #f))
  (let* ((grpath   (root-rel-path graphics-dir elemnode))
	 (iconpath (string-append grpath graphics-icons-subdir))
	 (sep      (if (equal? row 1)
		       (make sequence
			 (make entity-ref name: "nbsp")
			 (literal "|")
			 (make entity-ref name: "nbsp"))
		       (make empty-element gi: "IMG"
			     attributes: (list 
					  (list "SRC" 
						(string-append 
						 iconpath 
						 "iconsmallrightarrow.png"))
					  (list "ALT" "==>")
					  (list "ALIGN" "BOTTOM")
					  (list "BORDER" "0"))))))	       
    (make element gi: "DIV"
	  attributes: '(("CLASS" "NAVBAR"))

	  (case row
	    ((1)
	     (make sequence
	       (make element gi: "A"
		     attributes: (list 
				  (list "HREF" (href-to (nav-home elemnode))))
		     (make empty-element gi: "IMG"
			   attributes: (list (list "SRC" (string-append iconpath "iconhome.png"))
					     (list "ALT" "Home")
					     (list "ALIGN" "BOTTOM")
					     (list "BORDER" "0"))))
	       sep))
	    ((2)
	     (make sequence
	       (webpage-href startpage)
	       sep)))

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
			 title-sosofo)

		     (if isat
			 (make empty-element gi: "IMG"
			       attributes: (list (list "SRC" (string-append iconpath "iconat.png"))
						 (list "ALT" "@")
						 (list "ALIGN" "BOTTOM")
						 (list "BORDER" "0")))
			 (empty-sosofo))

		     (if (node-list-empty? (node-list-rest nl))
			 (empty-sosofo)
			 sep)

		     (loop (node-list-rest nl))))))))))

(define (webpage-header-navigation elemnode prev next prevsib nextsib)
  (let* ((webpages    (select-elements
		       (children (sgml-root-element))
		       (normalize "webpage")))
	 (full-trace  (let loop ((nd (parent elemnode)) (nl (empty-node-list)))
			(if (or (node-list-empty? nd)
				(node-list=? nd (sgml-root-element)))
			    nl
			    (loop (parent nd) (node-list nd nl)))))
	 (trace       (node-list-rest full-trace)))
    (make sequence
      (make-navbar 1 elemnode webpages)
      (if (node-list-empty? full-trace)
	  (empty-sosofo)
	  (make-navbar 2 elemnode (node-list trace elemnode) (node-list-first full-trace)))
      (make empty-element gi: "HR"
	    attributes: (list
			 (list "ALIGN" "LEFT")
			 (list "WIDTH" %gentext-nav-tblwidth%))))))

(define (webpage-footer-navigation elemnode prev next prevsib nextsib)
  (standard-footer elemnode))

(define (homepage-footer-navigation elemnode prev next prevsib nextsib)
  (standard-footer elemnode))

(define (standard-footer elemnode)
  (let* ((page     elemnode)
	 (homepage (if (equal? (gi page) (normalize "homepage"))
		       page
		       (nav-home elemnode)))
	 (head     (select-elements (children page) (normalize "head")))
	 (homehead (select-elements (children homepage) (normalize "head")))
	 (configs  (select-elements (children page) (normalize "config")))
	 (copyright (if (node-list-empty? 
			 (select-elements (children head) (normalize "copyright")))
			(select-elements (children homehead) (normalize "copyright"))
			(select-elements (children head) (normalize "copyright")))))
    (make sequence
      (make empty-element gi: "HR")
      (make element gi: "TABLE"
	    attributes: '(("BORDER" "0")
			  ("CELLPADDING" "0")
			  ("CELLSPACING" "0")
			  ("WIDTH" "100%"))
	    (make element gi: "TR"
		  (make element gi: "TD"
			attributes: '(("WIDTH" "33%")
				      ("ALIGN" "LEFT")
				      ("VALIGN" "TOP"))
			(make element gi: "SPAN"
			      attributes: '(("CLASS" "footer"))
			      (if (config-value elemnode "rcsdate")
				  (literal (config-value elemnode "rcsdate"))
				  (make entity-ref name: "nbsp"))))
		  (make element gi: "TD"
			attributes: '(("WIDTH" "34%")
				      ("ALIGN" "CENTER")
				      ("VALIGN" "TOP"))
			(if (node-list=? page homepage) 
			    (webpage-footers configs)
			    (make element gi: "SPAN"
				  attributes: '(("CLASS" "footer"))
				  (webpage-href homepage (literal "Home")))))
		  (make element gi: "TD"
			attributes: '(("WIDTH" "33%")
				      ("ALIGN" "RIGHT")
				      ("VALIGN" "TOP"))
			(make element gi: "SPAN"
			      attributes: '(("CLASS" "footer"))
			      (make element gi: "A"
				    attributes: (list 
						 (list 
						  "HREF"
						  (string-append
						   feedback-href
						   (url-encode-string
						    (data
						     (webpage-title-element
						      page))))))
				    (literal "Feedback")))))
	    (make element gi: "TR"
		  (make element gi: "TD"
			attributes: '(("COLSPAN" "3")
				      ("ALIGN" "RIGHT")
				      ("VALIGN" "TOP"))
			(if (node-list-empty? copyright)
			    (make entity-ref name: "nbsp")
			    (make element gi: "SPAN"
				  attributes: '(("CLASS" "footer"))
				  (with-mode nav-footer-copyright
				    (process-node-list copyright))))))
	    ))))

(mode nav-footer-copyright
  (element copyright
    (make sequence
      (literal (gentext-element-name-space (normalize "copyright")))
      (make entity-ref name: "copy")
      (literal " ")
      (process-children-trim)))

  (element year
    (if (first-sibling?)
	(process-children-trim)
	(make sequence
	  (literal ", ")
	  (process-children-trim))))

  (element holder
    (make sequence
      (if (first-sibling?)
	  (literal " ")
	  (literal ", "))
      (if (attribute-string "role")
	  (make element gi: "A"
		attributes: (list (list "HREF" (attribute-string "role")))
		(process-children-trim))
	  (process-children-trim))))
)

(define (webpage-footers configs)
  (let ((footers (let loop ((feet (empty-node-list)) (cfg configs))
		   (if (node-list-empty? cfg) 
		       feet
		       (if (equal? (attribute-string 
				    (normalize "param")
				    (node-list-first cfg)) 
				   "footer")
			   (loop (node-list feet (node-list-first cfg))
				 (node-list-rest cfg))
			   (loop feet (node-list-rest cfg)))))))
    (let loop ((cfg footers))
      (if (node-list-empty? cfg)
	  (empty-sosofo)
	  (make sequence
	    (make element gi: "SPAN"
		  attributes: '(("CLASS" "footer"))
		  (make element gi: "A"
			attributes: (list (list "HREF" 
						(attribute-string 
						 (normalize "value")
						 (node-list-first cfg))))
			(literal (attribute-string 
				  (normalize "altval") 
				  (node-list-first cfg))))
		  (if (node-list-empty? (node-list-rest cfg))
		      (empty-sosofo)
		      (literal " | ")))
	    (loop (node-list-rest cfg)))))))

;; ========================================================================

(element website
  (process-children))

(element config
  (empty-sosofo))

(element head
  (empty-sosofo))

(define ($standard-html-header$ #!optional
				(prev  (prev-chunk-element))
				(next  (next-chunk-element))
				(prevm (prev-major-component-chunk-element))
				(nextm (next-major-component-chunk-element)))
  (let ((webpage  (current-node))
	(homepage (nav-home (current-node))))
    (make sequence
      (web-style webpage "local")
      (web-style homepage "global")
      (web-script webpage "local")
      (web-script homepage "global")
      (web-keywords webpage))))

(define (web-style page class)
  (let* ((head    (select-elements (children page) (normalize "head")))
	 (style-elem (select-elements (children head) (normalize "style"))))
    (let loop ((nl style-elem))
      (if (node-list-empty? nl)
	  (empty-sosofo)
	  (make sequence
	    (if (equal? (normalize class)
			(attribute-string (normalize "class")
					  (node-list-first nl)))
		(web-style-element (node-list-first nl))
		(empty-sosofo))
	    (loop (node-list-rest nl)))))))

(define (web-style-element style-elem)
  (let ((href (attribute-string (normalize "src") style-elem))
	(path (root-rel-path "" (current-node)))
	(type (if (attribute-string (normalize "type") style-elem)
		  (attribute-string (normalize "type") style-elem)
		  "text/css")))
    (if href
	(make empty-element gi: "LINK"
	      attributes: (list (list "REL" "STYLESHEET")
				(list "HREF" (string-append path href))
				(list "TYPE" type)))
	(make element gi: "STYLE"
	      attributes: (list (list "TYPE" type))
	      (literal (data style-elem))))))

(define (web-script page class)
  (let* ((head    (select-elements (children page) (normalize "head")))
	 (script-elem (select-elements (children head) (normalize "script"))))
    (let loop ((nl script-elem))
      (if (node-list-empty? nl)
	  (empty-sosofo)
	  (make sequence
	    (if (equal? (normalize class)
			(attribute-string (normalize "class")
					  (node-list-first nl)))
		(web-script-element (node-list-first nl))
		(empty-sosofo))
	    (loop (node-list-rest nl)))))))

(define (web-script-element script-elem)
  (let ((href (attribute-string (normalize "src") script-elem))
	(path (root-rel-path "" (current-node)))
	(lang (if (attribute-string (normalize "language") script-elem)
		  (attribute-string (normalize "language") script-elem)
		  "JavaScript")))
    (if href
	(make element gi: "SCRIPT"
	      attributes: (list (list "SRC" (string-append path href))
				(list "LANGUAGE" lang))
	      (literal (data script-elem)))
	(make element gi: "SCRIPT"
	      attributes: (list (list "LANGUAGE" lang))
	      (literal (data script-elem))))))

(define (web-keywords page)
  (let* ((head     (select-elements (children page) (normalize "head")))
	 (keywords (select-elements (children head) (normalize "keywords"))))
    (if (node-list-empty? keywords)
	(empty-sosofo)
	(make empty-element gi: "META"
	      attributes: (list (list "NAME" "KEYWORDS")
				(list "CONTENT" (data keywords)))))))

(element homepage
  (let* ((homepg (current-node))
	 (desc (config-value homepg "desc")))
    (make sequence
      (html-document
       (make sequence
	 (literal desc " - ")
	 (with-mode title-mode (process-node-list (webpage-title-element))))
       (webpage-body))
      ($sitemap$))))

(element webpage
  (let* ((homepg (nav-home (current-node)))
	 (desc (config-value homepg "desc")))
    (html-document
     (make sequence
       (literal desc " - ")
       (with-mode title-mode (process-node-list (webpage-title-element))))
     (webpage-body))))

(define (webpage-body)
  (make element gi: "DIV"
	attributes: (list (list "CLASS" (gi)))
	(if (equal? (gi) (normalize "homepage"))
	    (empty-sosofo)
	    (webpage-title))
	(process-children)))

(define (webpage-title)
  (let* ((head  (select-elements (children (current-node)) (normalize "head")))
	 (title (select-elements (children head) (normalize "title"))))
    (make element gi: "h1"
	  attributes: (list (list "CLASS" (gi)))
	  (make element gi: "A"
		attributes: (list (list "NAME" (element-id)))
		(process-node-list (children title))))))

(define (webpage-href node #!optional (sosofo #f))
  (make element gi: "A"
	attributes: (list (list "HREF" (href-to node)))
	(if (sosofo? sosofo)
	    sosofo
	    (with-mode title-mode
	      (process-node-list (webpage-title-element node))))))

(define (table-toc elements)
  (make element gi: "TABLE"
	attributes: '(("BORDER" "0"))
	(let loop ((nl elements))
	  (if (node-list-empty? nl)
	      (empty-sosofo)
	      (make sequence
		(make element gi: "TR"
		      (make element gi: "TD"
			    attributes: '(("ALIGN" "LEFT")
					  ("VALIGN" "TOP"))
			    (make element gi: "A"
				  attributes: (list
					       (list "HREF"
						     (href-to 
						      (node-list-first nl))))
				  (with-mode title-mode
				    (process-node-list 
				     (webpage-title-element (node-list-first nl))))))
		      (let* ((head (select-elements 
				    (children (node-list-first nl))
				    (normalize "head")))
			     (abs  (select-elements
				    (children head)
				    (normalize "abstract"))))
			(make element gi: "TD"
			      attributes: '(("ALIGN" "LEFT")
					    ("VALIGN" "TOP"))
			      (process-node-list abs))))
		(loop (node-list-rest nl)))))))

(define (toc elements)
  (make element gi: "UL"
	(let loop ((nl elements))
	  (if (node-list-empty? nl)
	      (empty-sosofo)
	      (make sequence
		(make element gi: "LI"
		      (make element gi: "A"
			    attributes: (list
					 (list "HREF"
					       (href-to 
						(node-list-first nl))))
			    (with-mode title-mode
			      (process-node-list 
			       (webpage-title-element (node-list-first nl)))))
		      (let* ((head (select-elements 
				    (children (node-list-first nl))
				    (normalize "head")))
			     (sum  (select-elements
				    (children head)
				    (normalize "summary"))))
			(if (node-list-empty? sum)
			    (empty-sosofo)
			    (make sequence
			      (literal " - ")
			      (process-node-list (children sum))))))
		(loop (node-list-rest nl)))))))

(element webtoc
  (let* ((component (ancestor-member (current-node) 
				     (component-element-list))))
    (if (equal? (gi component) (normalize "homepage"))
	(toc (select-elements
	      (children (parent component))
	      (normalize "webpage")))
	(toc (select-elements
	      (children component)
	      (normalize "webpage"))))))

;; ======================================================================
;; Handle xbel

(define (dynamic-html) 
  (let ((dyn (config-value (sgml-root-element) "dynamic-html")))
    (if (equal? dyn "yes")
	#t
	#f)))

(element xbel
  (let ((title (select-elements (children (current-node)) "title")))
    (make sequence
      (process-node-list title)
      (make element gi: "UL"
	    (process-links)))))

(element (xbel title)
  (make element gi: "H1"
	(process-children)))

(element (xbel info)
  (empty-sosofo))

(element (xbel desc)
  (empty-sosofo))

(element folder
  (let* ((title  (select-elements (children (current-node)) "title"))
	 (dhtml  (if (and (dynamic-html) (attribute-string "id"))
		     (list (list "STYLE" "display:none")
			   (list "ID" (attribute-string "id")))
		     '())))
    (make sequence
      (process-node-list title)
      (make element gi: "UL"
	    attributes: (append
			 '(("COMPACT" "COMPACT"))
			 dhtml)
	    (process-links)))))

(element (folder title)
  (let* ((id      (attribute-string "id" (parent (current-node))))
	 (onclick (if id
		      (string-append "toggleList('" id "')")
		      #f))
	 (dhtml   (if (and (dynamic-html) onclick)
		      (list (list "onClick" onclick)
			    (list "CLASS" "EXLIST")
			    (list "STYLE" "color: blue"))
		      '())))
    (make sequence
      (make element gi: "LI"
	    (make element gi: "B"
		  (make element gi: "SPAN"
			attributes: dhtml
			(process-children)))))))

(define (process-links #!optional (nodes (children (current-node))))
  (let loop ((nl nodes))
    (if (node-list-empty? nl)
	(empty-sosofo)
	(if (equal? (gi (node-list-first nl)) (normalize "title"))
	    (loop (node-list-rest nl))
	    (make sequence
	      (process-node-list (node-list-first nl))
	      (loop (node-list-rest nl)))))))

(element bookmark
  (process-children))

(element (bookmark title)
  (let* ((href   (attribute-string "href" (parent (current-node))))
	 (info   (select-elements (children (parent (current-node))) "info"))
	 (meta   (select-elements (children info) "metadata"))
	 (author (if (node-list-empty? meta)
		     #f
		     (attribute-string "owner" meta))))
    (make sequence
      (make element gi: "LI"
	    (make element gi: "A"
		  attributes: (list (list "HREF" href))
		  (process-children))
	    (if author
		(literal (string-append ", by " author))
		(empty-sosofo))))))

(element (bookmark desc)
  (make sequence
    (make empty-element gi: "BR")
    (process-children)))

;(element xlink
;  (let* ((href  (attribute-string "href"))
;	 (grove (sgml-parse href))
;	 (xbel  (node-property 'document-element 
;			       (node-property 'grove-root grove))))
;    (process-node-list xbel)))

;; ======================================================================

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

(define ($sitemap-page$ page)
  (let* ((pages (select-elements (children page) "webpage"))
	 (head  (select-elements (children page) "head"))
	 (title (select-elements (children head) "title"))
	 (desc  (config-value page "desc"))
	 (file  (html-file page)))
    (make sequence
      (make element gi: "DT"
	    (make element gi: "A"
		  attributes: (list (list "HREF" file))
		  (with-mode title-mode (process-node-list title))))
      (make element gi: "DD"
	    (make element gi: "DL"
		  (let loop ((nl pages))
		    (if (node-list-empty? nl)
			(empty-sosofo)
			(make sequence
			  ($sitemap-page$ (node-list-first nl))
			  (loop (node-list-rest nl))))))))))

;; ======================================================================
;; forms

(define (conditional-atts attribute-names)
  (let loop ((attnames attribute-names) (attlist '()))
    (if (null? attnames)
	attlist
	(if (attribute-string (normalize (car attnames)))
	    (loop (cdr attnames)
		  (append attlist (list (list (car attnames)
					      (attribute-string 
					       (normalize (car attnames)))))))
	    (loop (cdr attnames) attlist)))))

(element html:form
  (let ((id   (element-id))
	(atts (conditional-atts '("method" "action"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "FORM"
	  attributes: atts
	  (make sequence
	    (make element gi: "A"
		  attributes: (list (list "NAME" id))
		  (empty-sosofo))
	    (process-children)))))

(element html:input
  (let ((id   (element-id))
	(atts (conditional-atts '("type"	"name"		"value"
				  "checked"	"disabled"	"readonly"
				  "size"	"maxlength"	"src"
				  "alt"		"usemap"	"tabindex"
				  "accesskey"	"onfocus"	"onblur"
				  "onselect"	"onchange"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make empty-element gi: "INPUT"
	  attributes: atts)))

(element html:button
  (let ((atts (conditional-atts '("name"	"value"		"type"
				  "disabled"	"tabindex"	"accesskey"
				  "onfocus"	"onblur"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "BUTTON"
	  attributes: atts
	  (process-children))))

(element html:label
  (let ((atts (conditional-atts '("for"		"accesskey"	"onfocus"
				  "onblur"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "LABEL"
	  attributes: atts
	  (process-children))))

(element html:select
  (let ((atts (conditional-atts '("name"	"size"		"multiple"
				  "disabled"	"tabindex"	"onfocus"
				  "onblur"	"onchange"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "SELECT"
	  attributes: atts
	  (process-children))))

(element html:option
  (let ((atts (conditional-atts '("selected"	"disabled"	"value"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "OPTION"
	  attributes: atts
	  (process-children))))

(element html:textarea
  (let ((atts (conditional-atts '("name"	"rows"		"cols"
				  "disabled"	"readonly"	"tabindex"
				  "accesskey"	"onfocus"	"onblur"
				  "onselect"	"onchange"
				  "onclick"	"ondblclick"	"onmousedown"
				  "onmouseup"	"onmouseover"	"onmousemove"
				  "onmouseout"	"onkeypress"	"onkeydown"
				  "onkeyup"))))
    (make element gi: "TEXTAREA"
	  attributes: atts
	  (process-children))))

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook.dsl">
<external-specification id="xpointer" document="xpointer.dsl">

</style-sheet>
