<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
<!ENTITY common PUBLIC "-//SFI Hive//DOCUMENT DSSSL Common Definitions//EN" CDATA DSSSL>
]>
<style-sheet>
<style-specification id="print" use="common docbook">
<style-specification-body> 

(define %visual-acuity% "tiny")

(define %bottom-margin% 4pi)
(define %top-margin% 5pi)
(define %left-margin% 4pi)
(define %right-margin% 4pi)
(define %body-start-indent% 1pi)
(define %header-margin% 1pi)
(define %footer-margin% 1pi)
(define %default-quadding% 'justify)

(define %generate-reference-toc% 
  ;; Should a Table of Contents be produced for References?
  #f)

(define (toc-depth nd)
  ;; make table of contents:
  ;; 1-level deep for each book
  ;; 2-levels deep for sets
  ;; and 3-deep for all remaining
  (cond ((string=? (gi nd) (normalize "book")) 1)
        ((string=? (gi nd) (normalize "set")) 2)
        (else 3)))

(define (make-divider)
    (make rule 
          orientation: 'horizontal
          line-thickness: 1pt))

(define (make-linebreak)
    (make paragraph
          space-before: 6pt
          (empty-sosofo)))

(define (printed-link)
    (let* ((id (attribute-string "linkend"))
           (nl (element-with-id id)))
      (sosofo-append
       (let ((indexitem (id-to-indexitem id)))
         (if indexitem
             (literal indexitem)
             (process-children)))
       ($italic-seq$
        (sosofo-append
         (literal " (see page ")
         (element-page-number-sosofo nl)
         (literal ")"))))))

(element link (printed-link))

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

(element (refsect1 para link)
         (make paragraph 
               (printed-link)))

(element primaryie
         (let* ((linkends-string (attribute-string "linkends"))
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
     (make display-group
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
                nop-style))
         space-before: (if (equal? (normalize "compact") spacing)
                           0pt
                           %para-sep%)
         first-line-start-indent: (- indent-step)
         (make sequence
           
           line-field)
         (with-mode listitem-content-mode
	  (process-node-list first-child)))
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

(define ($img$ #!optional (nd (current-node)) (display #f))
  (let* ((fileref (attribute-string (normalize "fileref") nd))
         (entattr (attribute-string (normalize "entityref") nd))
         (gensysid (entity-generated-system-id entattr))
         (entitypathname (if entattr
                             (car (cdr (split-string gensysid #\>)))
                             #f))
         (entityshortpathname
          (if entitypathname
              (string-append
               "figs/"
               (car (reverse (split-string entitypathname #\/))))
              #f))
         (format (attribute-string (normalize "format") nd))
         (align (attribute-string (normalize "align") nd))
         (scale (attribute-string (normalize "scale") nd)))
    (if (or fileref entityshortpathname)
        (let ((scale-val (if scale (/ (string->number scale) 100) 1.0)))
          (make external-graphic
                notation-system-id: (if format format "")
                entity-system-id: (if fileref
                                      (graphic-file fileref)
                                      (if entityshortpathname
                                          (graphic-file entityshortpathname)
                                          ""))
                space-before: 36pt
                keep-with-previous?: #t
                display?: #t
                ;; The [scale=\ScaleX] as an argument to \includegraphics
                ;; errors in TeX.  This forces a preceding \scalebox.
                scale: (list scale-val (+ .01 scale-val))
                display-alignment: 'center))
        (empty-sosofo))))

(mode article-titlepage-recto-mode (element graphic ($img$)))
(mode article-titlepage-verso-mode (element graphic ($img$)))
(mode book-titlepage-recto-mode (element graphic ($img$)))
(mode book-titlepage-verso-mode (element graphic ($img$)))
(mode reference-titlepage-recto-mode (element graphic ($img$)))
(mode reference-titlepage-verso-mode (element graphic ($img$)))
(mode set-titlepage-recto-mode (element graphic ($img$)))
(mode set-titlepage-verso-mode (element graphic ($img$)))

(define (corpauthor)
  (make paragraph
    font-family-name: %title-font-family%
    font-weight: 'bold
    font-size: (HSIZE 3)
    space-before: 1pt
    space-after: 1pt
    quadding: 'center
    keep-with-previous?: #t
    (process-children)))

(define (infotitle)
  (make paragraph
    font-family-name: %title-font-family%
    font-weight: 'bold
    font-size: (HSIZE 5)
    space-before: 1pt
    space-after: 1pt
    quadding: 'center
    break-before: 'page
    keep-with-next?: #t
    (process-children-trim)))

(mode book-titlepage-recto-mode 
  (element corpauthor (corpauthor))
  (element title (infotitle))
  )

(mode set-titlepage-recto-mode
  (element corpauthor (corpauthor))
  (element title (infotitle))
  )

(mode article-titlepage-recto-mode
  (element corpauthor (corpauthor))
  (element title (infotitle))
  )

(define (example-title text)
    (make sequence
          font-weight: 'bold
          (literal text)))

(define (example-entry-text example-node)
    (make-listitem 2
                   (literal (example-label example-node #f))
                   (sosofo-append
                    (make leader (literal "."))
                    (make link
                          destination: (node-list-address example-node)
                          (with-mode toc-page-number-mode
                            (process-node-list example-node))))))

(define (make-list sosofo)
    (make display-group
          start-indent: (if (INBLOCK?)
                            (inherited-start-indent)
                            (+ %block-start-indent% (inherited-start-indent)))
          space-before: (if (INLIST?) %para-sep% %block-sep%)
          space-after:  (if (INLIST?) %para-sep% %block-sep%)
          sosofo))

(define (make-listitem level title contents)
    (let ((indent-step (ILSTEP))
          (override #f)
          (mark #f)
          (ilevel level))
      (make sequence
            start-indent: (+ (inherited-start-indent) indent-step)
            (make paragraph
                  use: default-text-style
                  first-line-start-indent: (- indent-step)
                  (make line-field
                        font-size: (BULLTREAT BULLSIZE ilevel override mark)
                        position-point-shift: (BULLTREAT BULLSHIFT ilevel override mark)
                        field-width: (ILSTEP)
                        (literal (BULLTREAT BULLSTR ilevel override mark)))
                  title
                  contents))))

;; identical title expansion, except for wrap of display-group with
;; break-before: 'page which in modified TeX backend (\Break@page)
;; means a \cleardoublepage.
(mode reference-titlepage-recto-mode
  (element title
    (make display-group
          break-before: 'page
          (let ((division (ancestor-member (current-node) (division-element-list))))
            (make paragraph
                  font-family-name: %title-font-family%
                  font-weight: 'bold
                  font-size: (HSIZE 5)
                  line-spacing: (* (HSIZE 5) %line-spacing-factor%)
                  space-before: (* (HSIZE 5) %head-before-factor%)
                  quadding: %division-title-quadding%
                  keep-with-next?: #t
                  (if (string=? (element-label division) "")
                      (empty-sosofo)
                      (literal (element-label division)
                               (gentext-label-title-sep (gi division))))
                  (process-children)))))
  )

(define %appendix-font-factor% 0.6)
(define %appendix-block-factor% 0.1)
(define appendix-text-style
  (style
      font-size: (* (inherited-font-size) %appendix-font-factor%)
      font-weight: (inherited-font-weight)
      font-posture: (inherited-font-posture)
      font-family-name: (inherited-font-family-name)
      line-spacing: (* (inherited-line-spacing) %appendix-font-factor%)
      ))

(element appendix
  (with-mode appendix-mode
    ($appendix$)))

(mode appendix-mode
  (element sect1 ($appendix-section$))
  (element blockquote
    (make paragraph
      font-size: (* (inherited-font-size) %smaller-size-factor%)
      line-spacing: (* (inherited-font-size) %line-spacing-factor%
		       %smaller-size-factor%)
      space-before: (* %para-sep% %appendix-block-factor%)
      start-indent: (+ (inherited-start-indent) 1em)
      end-indent: 1em
      (process-children)))
)

(define ($appendix-section$)
  (if (node-list=? (current-node) (sgml-root-element))
      (make simple-page-sequence
	page-n-columns: %page-n-columns%
	page-number-restart?: (or %page-number-restart% 
				  (book-start?) 
				  (first-chapter?))
	page-number-format: ($page-number-format$)
	use: default-text-style
	left-header:   ($left-header$)
	center-header: ($center-header$)
	right-header:  ($right-header$)
	left-footer:   ($left-footer$)
	center-footer: ($center-footer$)
	right-footer:  ($right-footer$)
	start-indent: %body-start-indent%
	input-whitespace-treatment: 'collapse
	quadding: %default-quadding%
	(make sequence
	  ($section-title$)
	  (process-children)))
      (make display-group
	space-before: (* %block-sep% %appendix-block-factor%)
	space-after: (* %block-sep% %appendix-block-factor%)
	start-indent: %body-start-indent%
	(make sequence
	  ($section-title$)
	  (process-children)))))

(define ($appendix$)
  (make simple-page-sequence
    page-n-columns:  %page-n-columns%
    page-number-restart?: (or %page-number-restart% 
			      (book-start?) 
			      (first-chapter?))
    page-number-format: ($page-number-format$)
    use: appendix-text-style
    left-header:   ($left-header$)
    center-header: ($center-header$)
    right-header:  ($right-header$)
    left-footer:   ($left-footer$)
    center-footer: ($center-footer$)
    right-footer:  ($right-footer$)
    start-indent:  %body-start-indent%
    input-whitespace-treatment: 'collapse
    quadding: %default-quadding%
    (make sequence
      ($component-title$)
      (process-children))
    (make-endnotes)))

</style-specification-body>
</style-specification>

<external-specification id="common" document="common">
<external-specification id="docbook" document="docbook">

</style-sheet>
