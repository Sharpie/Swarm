<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

;; bibliographic entry stuff....
(define bibltable #t)

(define %show-comments%
  ;; Display Comment elements?
  #f)

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


(define %callout-graphics-path%
  ;; Path to callout graphics
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

(element informaltable
  (let ((role (attribute-string (normalize "role"))))
    (if (equal? role "proglist-objc-java")
        (with-mode proglist-objc-java-mode
          (process-children))
        (process-children))))

(mode proglist-objc-java-mode
  ;; don't indent in this mode!!
  (element programlisting ($verbatim-display$
                    #f
                    %number-programlisting-lines%))
  )



</style-specification-body>
</style-specification>
</style-sheet>
