<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY % html "IGNORE">
<![%html;[
<!ENTITY % print "IGNORE">
<!ENTITY docbook.dsl SYSTEM "/net/wijiji/disk2/src/docbook/html/docbook.dsl" CDATA DSSSL>
]]>
<!ENTITY % print "INCLUDE">
<![%print;[
<!ENTITY docbook.dsl SYSTEM "/net/wijiji/disk2/src/docbook/print/docbook.dsl" CDATA DSSSL>
]]>
]>

<style-sheet>
<style-specification id="print" use="docbook">
<style-specification-body>

;; customize the print stylesheet

(define %generate-lot-list%
  ;; Should a List of Titles be produced?
  (list "TABLE" "FIGURE" "EXAMPLE" "EQUATION"))

(define %graphic-extensions% 
  ;; List of graphic filename extensions
  '("gif" "jpg" "jpeg"  "ps" ))

</style-specification-body>
</style-specification>

<style-specification id="html" use="docbook">
<style-specification-body> 

;; customize the html stylesheet

(define %graphic-extensions% 
  ;; List of graphic filename extensions
  '("gif" "jpg" "jpeg"  "ps" ))

;;(define %shade-verbatim%  
  ;; Should verbatim environments be shaded?
;;  #t)

(define %shade-verbatim-attr% 
  ;; Attributes used to create a shaded verbatim environment.
  (list
   (list "BORDER" "2")
   (list "BGCOLOR" "#EOEOEO")
   (list "WIDTH" "70%")))

(define %section-autolabel%
  ;; Are sections enumerated?
  #f)

(define %use-id-as-filename%
  ;; Use ID attributes as name for component HTML files?
  #t)

(define (expand-method signature paramdefs)
    (let ((signature-length (string-length signature)))
      (let next-pos ((start-pos 0) (pos 0) (paramdefs paramdefs))
           (if (< pos signature-length)
               (if (char=? (string-ref signature pos) #\:)
                   (sosofo-append
                    (literal (substring signature start-pos (+ pos 1)))
                    (process-node-list (node-list-first paramdefs))
                    (next-pos (+ 1 pos) (+ 1 pos) (node-list-rest paramdefs)))
                   (next-pos start-pos (+ 1 pos) paramdefs))
               (if (= start-pos 0)
                   (literal (substring signature 0 signature-length))
                   (empty-sosofo))))))

(define (chars-p nl)
    (let loop ((kl nl))
         (if (node-list-empty? kl)
             #f
             (let ((c (node-list-first kl)))
               (if (char? (node-property 'char c default: #f))
                   #t
                   (loop (node-list-rest kl)))))))

(define (type-expand nl)
         (if (chars-p nl)
             (sosofo-append
              (literal "(")
              (let loop ((kl nl))
                   (if (node-list-empty? kl)
                       (empty-sosofo)
                       (let ((c (node-list-first kl)))
                         (sosofo-append
                          (if (char? (node-property 'char c default: #f))
                              (process-node-list c)
                              (empty-sosofo))
                          (loop (node-list-rest kl))))))
              (literal ")"))
             (empty-sosofo)))

(element FUNCDEF (type-expand (children (current-node))))
         
;(element FUNCTION (empty-sosofo))

(element PARAMDEF
         (sosofo-append
          (literal " ")
          (type-expand (children (current-node)))
          (process-matching-children "PARAMETER")
          (literal " ")))

(element FUNCPROTOTYPE
         (let* ((funcdef (select-elements (children (current-node)) "FUNCDEF"))
                (function (select-elements (children funcdef) "FUNCTION"))
                (function-data (data function)))
           (sosofo-append
            (literal (substring function-data 0 1))
            (literal " ")
            (process-node-list funcdef)
            (expand-method
             (substring function-data 1 (string-length function-data))
             (select-elements (children (current-node)) "PARAMDEF")))))
         

</style-specification-body>
</style-specification>
<external-specification id="docbook" document="docbook.dsl">
</style-sheet>

