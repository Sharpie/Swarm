<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
]>
<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body> 

(define (article-titlepage-recto-elements)
  (list	
   ;;(normalize "title") 
   (normalize "abstract")
   (normalize "subtitle") 
   (normalize "confgroup")
   ))

(define biblio-xref-title
  ;; Use the titles of bibliography entries in XREFs
  #t)

(element confgroup (make display-group 
		     ($block-container$)))
(element conftitle (make display-group 
		     ($block-container$)))
(element confdates (make display-group
		     (process-children)))
(element confsponsor (make display-group
		       (sosofo-append 
			(literal "Sponsored by: ") (process-children))))
(element confnum     (make display-group
		       (process-children))) 

(element emphasis
  (let ((role (attribute-string (normalize "role"))))
    (if (equal? role "strong")
	($bold-seq$)
	($italic-seq$))))

(mode biblioentry-inline-mode

  (element author
    (let* ((affil (select-elements 
		  (children (current-node))
		  (normalize "affiliation")))
	   (addr (select-elements (children affil) (normalize "address")))
	   (org (select-elements (children affil) (normalize "orgname")))
	   (mail (select-elements (children addr) (normalize "email"))))
      (make sequence
	(literal (author-list-string))
	(literal " ")
	(process-node-list mail)
	(literal " ")
        (if (not (node-list-empty? org))
	    (make sequence
	      (literal "(")
	      (process-node-list affil)
	      (literal ")"))
            (empty-sosofo))
	(literal " ")
	))))

(define %biblioentry-in-entry-order% #t)

(define %biblsep% ". ")

(define bop-footnotes
  ;; Make "bottom-of-page" footnotes?
  #f)

(define %footnote-ulinks%
  ;; Generate footnotes for ULinks?
  #t)

(define %default-quadding%   
  ;; The default quadding
  'justify)

(define %block-start-indent% 
  ;; Extra start-indent for block-elements
  8.0pt)

(define %line-spacing-factor% 
  ;; Factor used to calculate leading
  1.34)

(define %bf-size%
  ;; Defines the body font size
  10pt)

(define %footnote-size-factor% 
  ;; Footnote font scaling factor
  0.5)

(define %header-margin% 
  ;; Height of header margin
  2pi)

(define %top-margin% 
  ;; Height of top margin
  2.7pi)

(define %footer-margin% 
  ;; Height of footer margin
  2.0pi)

(define %bottom-margin% 
  ;; Height of bottom margin
  2.5pi)

(define %left-margin% 
  ;; Width of left margin
  3.3pi)

(define %right-margin% 
  ;; Width of the right margin
  2.5pi)

(define %body-start-indent% 
  ;; The default indent of body text. Some elements may have more or less
  ;; indentation.
  1.5pi)

(define %body-font-family% 
  ;; The font family used in body
  "Arial")

(define %title-font-family% 
  ;; The font family used in titles
  "Arial" ;;"Times New Roman"
  )

(define %generate-article-titlepage-on-separate-page%
  ;; PURP Should the article title page be on a separate page?
  ;; If true, the title page for each 'Article' will occur on its own page.
  #t)

(define %page-number-restart% 
  ;; PURP Restart page numbers in each component?
  #t)

(define %article-page-number-restart% 
  ;; PURP Restart page numbers in each article?
  #t)

(define (toc-depth nd)
  (cond ((string=? (gi nd) (normalize "article")) 2)
        ((string=? (gi nd) (normalize "book")) 7)
        (else 1)))

(define %head-before-factor% 
  ;; PURP Factor used to calculate space above a title
  0.08)

(define %head-after-factor% 
  ;; PURP Factor used to calculate space below a title
  0.04)

(define %block-sep% 
  ;; PURP Distance between block-elements
  (* %para-sep% 0.9))

(define %generate-heading-level%   
  ;; PURP Output RTF heading level characteristics?
  #t)

(define %section-title-quadding% 
  ;; PURP Section title quadding
  'center)

</style-specification-body>
</style-specification>

<external-specification id="docbook" document="docbook">
</style-sheet>
