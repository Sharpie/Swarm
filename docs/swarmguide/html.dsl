<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook HTML Stylesheet//EN" CDATA DSSSL>
<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>
]>

<style-sheet>
<style-specification id="html" use="common docbook">
<style-specification-body> 


(define ($generate-book-lot-list$)
  ;; Which Lists of Titles should be produced for Books?
  (list (normalize "table")
        (normalize "figure")
        (normalize "example")
        (normalize "equation")))

(define %html-ext% ".html")
(define %use-id-as-filename% #t)

(define %generate-legalnotice-link%
  ;; Should legal notices be a link to a separate file?
  #t)

(define %always-format-variablelist-as-table% #t)

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

(define ($verbatim-display$ indent line-numbers?)
  (let ((content (make element gi: "PRE"
		       attributes: (list
				    (list "CLASS" (gi)))
		       (if (or indent line-numbers?)
			   ($verbatim-line-by-line$ indent line-numbers?)
			   (process-children)))))
    (if %shade-verbatim%
        (make element gi: "TABLE"
              attributes: ($shade-verbatim-attr$)
              (make element gi: "TR"
                    (make element gi: "TD"
                          (make element gi: "FONT"
                                attributes: (list (list "SIZE" "-1"))
                                content))))
	content)))

(define (toc-depth nd)
  ;; make table of contents:
  ;; 2-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 2)
        ((string=? (gi nd) (normalize "chapter")) 2)
        (else 3)))

;; Make text that comes from unimplemented tags easy to spot
(element comment
  (make element gi: "FONT"
 	attributes: '(("COLOR" "GREEN"))
 	(process-children)))

(define ($bold-sanserif-seq$ #!optional (sosofo (process-children)))
  (make element gi: "FONT"
	attributes: (list (list "FACE" "Trebuchet, Arial, sans-serif")
                          (list "SIZE" "-1"))
	(make element gi: "B"
	      sosofo)))

(define ($admon-graphic$ #!optional (nd (current-node)))
  ;; Admonition graphic file
  (cond ((equal? (gi nd) (normalize "tip"))
         (string-append %admon-graphics-path% "tip.jpeg"))
        ((equal? (gi nd) (normalize "note"))
         (string-append %admon-graphics-path% "note.jpeg"))
        ((equal? (gi nd) (normalize "important"))
         (string-append %admon-graphics-path% "important.jpeg"))
        ((equal? (gi nd) (normalize "caution"))
         (string-append %admon-graphics-path% "caution.jpeg"))
        ((equal? (gi nd) (normalize "warning"))
         (string-append %admon-graphics-path% "warning.jpeg"))
        (else (error (string-append (gi nd) " is not an admonition.")))))

(define ($callout-bug$ conumber)
  (let ((number (if conumber (format-number conumber "1") "0")))
    (if conumber
	(if %callout-graphics%
	    (if (<= conumber %callout-graphics-number-limit%)
		(make empty-element gi: "IMG"
		      attributes: (list (list "SRC" 
					      (root-rel-path
					       (string-append
						%callout-graphics-path%
						number
						".jpeg")))
					(list "HSPACE" "0")
					(list "VSPACE" "0")
					(list "BORDER" "0")
					(list "ALT"
					      (string-append
					       "(" number ")"))))
		(make element gi: "B"
		      (literal "(" (format-number conumber "1") ")")))
	    (make element gi: "B"
		  (literal "(" (format-number conumber "1") ")")))
	(make element gi: "B"
	      (literal "(??)")))))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">
</style-sheet>
  