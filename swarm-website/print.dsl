<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY docbook PUBLIC "-//Norman Walsh//DOCUMENT DocBook Print Stylesheet//EN" CDATA DSSSL>
<!ENTITY org SYSTEM "org.dsl" CDATA DSSSL>
]>
<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body> 

(define ($webpage-component$)
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
      ($webpage-component-title$)
      (process-children))
    (make-endnotes)))

(define ($webpage-component-title$)
  (make sequence
      (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-size: (HSIZE 4)
	line-spacing: (* (HSIZE 4) %line-spacing-factor%)
	space-before: (* (HSIZE 4) %head-before-factor%)
	start-indent: 0pt
	first-line-start-indent: 0pt
	quadding: %component-title-quadding%
	heading-level: (if %generate-heading-level% 1 0)
	keep-with-next?: #t

	(if (string=? (element-label) "")
	    (empty-sosofo)
	    (literal (gentext-element-name-space (current-node))
		     (element-label)
		     (gentext-label-title-sep (gi))))

	(if (node-list-empty? titles)
	    (element-title-sosofo) ;; get a default!
	    (with-mode component-title-mode
	      (make sequence
		(process-node-list titles)))))

      (make paragraph
	font-family-name: %title-font-family%
	font-weight: 'bold
	font-posture: 'italic
	font-size: (HSIZE 3)
	line-spacing: (* (HSIZE 3) %line-spacing-factor%)
	space-before: (* 0.5 (* (HSIZE 3) %head-before-factor%))
	space-after: (* (HSIZE 4) %head-after-factor%)
	start-indent: 0pt
	first-line-start-indent: 0pt
	quadding: %component-subtitle-quadding%
	keep-with-next?: #t
        
	(with-mode component-title-mode
	  (make sequence
	    (process-node-list subtitles))))))

(element website ($webpage-component$))

(element homepage 
  (let* ((homepg (current-node))
         (desc (config-value hompg "desc")))
    ($webpage-component$)))

(element webpage ($webpage-component$))

(element webtoc (empty-sosofo))

</style-specification-body>
</style-specification>

<!-- <external-specification id="org" document="org"> -->
<external-specification id="docbook" document="docbook">
</style-sheet>
