<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

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

</style-specification-body>
</style-specification>
</style-sheet>
