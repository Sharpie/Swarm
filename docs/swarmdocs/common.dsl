<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

(element indexentry 
         (make display-group 
               keep: 'page
               (process-children)))

(define (expand-method signature paramdefs)
    (let ((signature-length (string-length signature)))
      (let next-pos ((start-pos 0) (pos 0) (paramdefs paramdefs))
           (if (< pos signature-length)
               (if (char=? (string-ref signature pos) #\:)
                   (sosofo-append
                    ($bold-seq$ (literal (substring signature start-pos (+ pos 1))))
                    (process-node-list (node-list-first paramdefs))
                    (next-pos (+ 1 pos) (+ 1 pos) (node-list-rest paramdefs)))
                   (next-pos start-pos (+ 1 pos) paramdefs))
               (if (= start-pos 0)
                   ($bold-seq$ (literal (substring signature 0 signature-length)))
                   (empty-sosofo))))))

(define (some-chars-p nl)
    (let loop ((kl nl))
         (if (node-list-empty? kl)
             #f
             (let ((c (node-list-first kl)))
               (if (char? (node-property 'char c default: #f))
                   #t
                   (loop (node-list-rest kl)))))))

(define (type-expand nl)
    (if (some-chars-p nl)
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

(define (expand-paragraphs text-nl)
    (let loop ((last-nl text-nl)
               (nl text-nl))
         (if (node-list-empty? nl)
             (sosofo-append
               (make-linebreak)
               (process-node-list last-nl))
             (let* ((node (node-list-first nl))
                    (ch (node-property 'char node)))
               (if (char=? ch #\U-000D)
                   (sosofo-append
                    (let ((last-line-nl (previous-nl last-nl node)))
                      (if (node-list-empty? last-line-nl)
                          (empty-sosofo)
                          (sosofo-append
                           (make-linebreak)
                           (process-node-list last-line-nl))))
                    (let ((next-nl (node-list-rest nl)))
                      (loop next-nl next-nl)))
                   (loop last-nl (node-list-rest nl)))))))

(define (skip-nonchars nl)
    (let loop ((nl nl))
         (if (node-list-empty? nl)
             (empty-node-list)
             (node-list
              (let ((c (node-list-first nl)))
                (if (node-property 'char c default: #f)
                    (node-list c)
                    (empty-node-list)))
              (loop (node-list-rest nl))))))

(define (immediate-data nl)
    (let* ((l
            (let loop ((nl nl))
                 (if (node-list-empty? nl)
                     '()
                     (let* ((c (node-list-first nl))
                            (ch (node-property 'char c default: #f)))
                       (if ch
                           (cons ch (loop (node-list-rest nl)))
                           (loop (node-list-rest nl))))))))
      (list->string l)))

(define (get-classname funcsynopsis-node)
  (data (select-elements
         (children
          (select-elements (children funcsynopsis-node)
                           "FUNCSYNOPSISINFO"))
         "CLASSNAME")))

(element FUNCDEF
         (let ((classname (get-classname (parent (parent)))))
           (cond ((string=? classname "(MACRO)")
                  (process-children))
                 ((string=? classname "(FUNCTION)")
                  (process-children))
                 (#t (type-expand (children (current-node)))))))

(define (paramdef)
    (let ((param (select-elements (children (current-node)) (normalize "parameter"))))
      (make sequence
            (if (equal? (child-number (current-node)) 1)
                (literal "(")
                (empty-sosofo))
            (if (equal? %funcsynopsis-style% 'ansi)
                (process-children)
                (process-node-list param))
            (if (equal? (gi (ifollow (current-node))) (normalize "paramdef"))
                (literal ", ")
                (literal ")")))))

(element PARAMDEF
         (let ((classname (get-classname (parent (parent)))))
           (cond ((string=? classname "(MACRO)")
                  (paramdef))
                 ((string=? classname "(FUNCTION)")
                  (paramdef))
                 (#t
                  (sosofo-append
                   (literal " ")
                   (type-expand (children (current-node)))
                   (process-matching-children "PARAMETER")
                   (literal " "))))))

(define (methodprototype)
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

(element FUNCPROTOTYPE
         (let ((classname (get-classname (parent))))
           (cond ((string=? classname "(MACRO)")
                  (funcprototype))
                 ((string=? classname "(FUNCTION)")
                  (funcprototype))
                 (#t (methodprototype)))))

(element FUNCSYNOPSISINFO
         (make paragraph
               first-line-start-indent: 0pt
               (expand-paragraphs (skip-nonchars
                                   (children (current-node))))))

(element CLASSNAME (empty-sosofo))

(define (previous-nl last-nl end-node)
  (let loop ((nl last-nl))
        (let ((node (node-list-first nl)))
          (if (node-list=? node end-node)
              (empty-node-list)
              (node-list node
                         (loop (node-list-rest nl)))))))

(define (embed-split string delimiter)
    (let loop ((last-ch #\U-0000) (l (string->list string)))
         (if (null? l)
             '()
             (let* ((ch (car l))
                    (next (loop ch (cdr l))))
               (if (and (char=? ch delimiter) (not (char=? ch last-ch)))
                   (list next)
                   (cons ch next))))))

(define (split-string str delimiter)
    (let flatten-arg ((l (embed-split str delimiter)) (out-l '()))
         (let ((last-string
                (lambda ()
                  (if (null? out-l)
                      '()
                      (list (list->string (reverse out-l)))))))
           (if (null? l)
               (last-string)
               (let ((item (car l)))
                 (if (list? item)
                     (append
                      (last-string)
                      (flatten-arg item '())
                      (flatten-arg (cdr l) '()))
                     (flatten-arg (cdr l) (cons item out-l))))))))

(define (reference-id-p id)
    (let* ((id-split-list (split-string id #\.)))
      (not (string=? (car (cdr id-split-list)) "SRC"))))

(define (type-id-p id type)
    (if (reference-id-p id)
        (let* ((id-split-list (split-string id #\.)))
          (string=? (car (cdr (cdr (cdr id-split-list)))) type))
        #f))

(define (char-change-case ch upcase)
    (let loop ((lc-pos
                '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                  #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
               (uc-pos 
                '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                  #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z)))
         (if (null? lc-pos)
             ch
             (if upcase
                 (if (char=? (car lc-pos) ch)
                     (car uc-pos)
                     (loop (cdr lc-pos) (cdr uc-pos)))
                 (if (char=? (car uc-pos) ch)
                     (car lc-pos)
                     (loop (cdr lc-pos) (cdr uc-pos)))))))

(define (string-change-case str upcase)
    (let loop ((l (string->list str)))
         (if (null? l)
             ""
             (string-append 
              (string (char-change-case (car l) upcase))
              (loop (cdr l))))))
         
(define (module-for-id id)
    (let* ((id-elements (split-string id #\.)))
      (string-change-case (car (cdr id-elements)) #f)))

(define (title-for-refentry refentry)
    (data
     (select-elements
      (children (select-elements (children refentry) "REFMETA"))
      "REFENTRYTITLE")))

(define (protocol-title-for-id id)
    (let* ((id-elements (split-string id #\.))
           (module-name (car (cdr id-elements)))
           (protocol-name (car (cdr (cdr id-elements))))
           (refentry (element-with-id
                      (string-append
                       "SWARM."
                       module-name
                       "."
                       protocol-name
                       ".PROTOCOL"))))
      (title-for-refentry refentry)))

(define (refentry-title-for-description id)
    (title-for-refentry (element-with-id id)))
             
(define (method-signature-title-for-id method-signature-id)
    (let* ((id-elements (split-string method-signature-id #\.))
           (phase-abbrev (car (cdr (cdr (cdr (cdr id-elements)))))))
      (string-append 
       (module-for-id method-signature-id)
       "/"
       (protocol-title-for-id method-signature-id)
       "/"
       (cond ((string=? phase-abbrev "PC") "Creating")
             ((string=? phase-abbrev "PS") "Setting")
             ((string=? phase-abbrev "PU") "Using")))))

(define (typedef-title-for-id id)
    (immediate-data (children (element-with-id id))))

(define (function-title-for-id id)
    (data 
     (select-elements
      (children
       (select-elements
        (children 
         (select-elements (children (element-with-id id)) "FUNCPROTOTYPE"))
        "FUNCDEF"))
      "FUNCTION")))

(define (macro-title-for-id id)
    (let ((node (element-with-id id)))
      (if (string=? (gi node) "FUNCSYNOPSIS")
          (function-title-for-id id)
          (data node))))

(define (global-title-for-id id)
    (data (node-list-last (select-elements (children (element-with-id id)) "TERM"))))

(define (id-to-indexitem id)
    (let ((id-split-list (split-string id #\.)))
      (if (string=? (car (cdr id-split-list)) "SRC")
          (data (select-elements (children (element-with-id id)) "TITLE"))
          (cond ((type-id-p id "METHOD") (method-signature-title-for-id id))
                ((type-id-p id "PROTOCOL") (refentry-title-for-description id))
                ((type-id-p id "MODULE") (refentry-title-for-description id))
                ((type-id-p id "TYPEDEF") (typedef-title-for-id id))
                ((type-id-p id "FUNCTION") (function-title-for-id id))
                ((type-id-p id "MACRO") (macro-title-for-id id))
                ((type-id-p id "GLOBAL") (global-title-for-id id))))))

(define (block-element-list)
  (list (normalize "example")
        (normalize "figure")
        (normalize "table")
        (normalize "equation")
        (normalize "procedure")
        (normalize "itemizedlist")
        (normalize "listitem")))

(define (example-label example-node #!optional (show-number #t))
    (let ((label (attribute-string "LABEL" example-node)))
      (if label
          (let* ((parts (split-string label #\/))
                 (afterprotocol (cdr (cdr parts))))
            (if (null? (cdr afterprotocol))
                (if show-number
                    (string-append " #" (car afterprotocol))
                    (string-append "Example #" (car afterprotocol)))
                (string-append (car afterprotocol)
                               (if show-number
                                   (string-append " #" (car (cdr afterprotocol)))
                                   ""))))
          (element-label example-node))))

(define (example-text text level)
    (make paragraph
          use: para-style
          start-indent: (+ %body-start-indent%
                           (* %toc-indent% level))
          first-line-start-indent: (* -1 %toc-indent%)
          font-weight: 'medium
          space-before: 0pt
          space-after: 0pt
          quadding: 'start
          (literal text)))

(define (example-title protocol #!optional module)
    (sosofo-append
     (if module
         (example-text module 1)
         (empty-sosofo))
     (example-text protocol 2)))

(define (example-entry example-node last-node)
    (let ((label (attribute-string "LABEL" example-node)))
      (if label
          (let* ((parts (split-string label #\/))
                 (afterprotocol (cdr (cdr parts)))
                 (module (car parts))
                 (protocol (car (cdr parts)))
                 (text-sosofo (make paragraph
                                    use: para-style
                                    start-indent: (+ %body-start-indent% (* %toc-indent% 3))
                                    first-line-start-indent: (* -1 %toc-indent%)
                                    font-weight: 'medium
                                    space-before: 0pt
                                    space-after: 0pt
                                    quadding: 'start
                                    (literal (example-label example-node #f))
                                    (make leader (literal "."))
                                    (make link
                                          destination: (node-list-address example-node)
                                          (with-mode toc-page-number-mode
                                            (process-node-list example-node))))))
            (let ((last-label (if last-node (attribute-string "LABEL" last-node) #f)))
              (if last-label
                  (let* ((last-parts (split-string last-label #\/))
                         (last-afterprotocol (cdr (cdr last-parts)))
                         (last-module (car last-parts))
                         (last-protocol (car (cdr last-parts))))
                    (if (string=? module last-module)
                        (if (string=? protocol last-protocol)
                            text-sosofo
                            (sosofo-append
                             (example-title protocol)
                             text-sosofo))
                        (sosofo-append
                         (example-title protocol module)
                         text-sosofo)))
                  (sosofo-append
                   (example-title protocol module)
                   text-sosofo))))
          ($lot-entry$ example-node))))

(define (second l)
  (car (cdr l)))

(define (build-lot nd lotgi)
    (make sequence
          (lot-title #t lotgi)
          (second
           (let accum-lot ((nd nd) (last-node #f) (sosofos (empty-sosofo)))
                (let* ((lotlist (node-list-filter-by-gi (children nd)
                                                        (append (division-element-list)
                                                                (component-element-list)
                                                                (section-element-list)
                                                                (block-element-list)
                                                                (list (normalize "para")
                                                                      (normalize "itemizedlist")
                                                                      (normalize "listitem"))))))
                  (if (node-list-empty? lotlist)
                      (list last-node sosofos)
                      (let loop ((nl lotlist) (last-node last-node) (sosofos sosofos))
                           (if (node-list-empty? nl)
                               (list last-node sosofos)
                               (let ((node (node-list-first nl))
                                     (rest (node-list-rest nl)))
                                 (if (string=? (gi node) lotgi)
                                     (if (string=? lotgi "EXAMPLE")
                                         (let* ((labeled-node
                                                 (if (attribute-string "LABEL" node)
                                                     node
                                                     last-node)))
                                           (apply loop
                                                  rest
                                                  (accum-lot
                                                   node
                                                   labeled-node 
                                                   (sosofo-append sosofos (example-entry node last-node)))))
                                         (apply loop
                                                rest
                                                (accum-lot
                                                 node
                                                 last-node
                                                 (sosofo-append sosofos ($lot-entry$ node)))))
                                     (apply loop rest (accum-lot node last-node sosofos))))))))))))

(element (varlistentry term) 
         (sosofo-append
          (process-children)
          (literal " ")))

(define (reference-titlepage-verso-elements)
  (list (normalize "revhistory")))

(mode reference-titlepage-verso-mode
  (element revhistory
           (make sequence
                 ($lowtitlewithsosofo$ 1 (literal "Revision History"))
                 (process-children)))

  (element (revhistory revision)
           (let ((revnumber (select-elements (descendants (current-node)) (normalize "revnumber")))
                 (revdate   (select-elements (descendants (current-node)) (normalize "date")))
                 (revauthor (select-elements (descendants (current-node)) (normalize "authorinitials")))
                 (revremark (select-elements (descendants (current-node)) (normalize "revremark"))))
             (make sequence
                   (sosofo-append
                    (make-linebreak)
                    (process-node-list revdate)
                    (literal " ")
                    (if (string=? (data revnumber) "")
                        (empty-sosofo)
                        (sosofo-append
                         (literal " ")
                         (process-node-list revnumber)
                         (literal " ")))
                    (process-node-list revauthor)
                    (make paragraph
                          (process-node-list revremark))
                    (make-linebreak)))))
  (element (revision revnumber) 
           ($bold-seq$ (process-children))) 
  (element (revision date) 
           ($bold-seq$ (process-children)))
  (element (revision authorinitials)  
           ($italic-seq$ (process-children)))
  (element (revision revremark)
           (make sequence
                 font-posture: 'upright
                 (process-children)))
  ) 


</style-specification-body>
</style-specification>
</style-sheet>
