<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

;; bibliographic entry stuff....
(define %biblioentry-in-entry-order% #f)
(define bibltable #t)

(define %show-comments%
  ;; Display Comment elements?
  #t)

(define %gentext-usen-by% "")

(define %funcsynopsis-decoration%
  ;; Decorate elements of a FuncSynopsis?
  #t)

(define %section-autolabel% 
  ;; Are sections enumerated?
  #t)

(define %example-rules%
  ;; Specify rules before and after an Example
  #t)

(define %admon-graphics%
  ;; Use graphics in admonitions?
  #t)

(define %admon-graphics-path%
  ;; Path to admonition graphics
  "figs/")

(element classname ($mono-seq$))
(element type ($mono-seq$))
(element function ($bold-mono-seq$))
(element varname ($italic-mono-seq$))
(element application ($bold-sanserif-seq$))

(element (artheader citetitle)
  (let ((work (attribute-string (normalize "pubwork"))))
    (cond 
     ((equal? work (normalize "article"))
      (make sequence
        (literal (gentext-start-quote))
        (process-children)
        (literal (gentext-end-quote))))
     (else ($italic-seq$)))))

</style-specification-body>
</style-specification>
</style-sheet>
