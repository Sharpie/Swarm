<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

(define %gentext-usen-by% "")

(define %generate-set-toc% 
  ;; Should a Table of Contents be produced for Sets?
  #t)

(define %generate-article-titlepage% 
  ;; Should an article title page be produced?
  #f)

(define %funcsynopsis-decoration%
  ;; Decorate elements of a FuncSynopsis?
  #t)

(define %section-autolabel% 
  ;; Are sections enumerated?
  #t)

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

(define (type-id-p id type)
    (let* ((id-split-list (split-string id #\.)))
      (string=? (car (cdr (cdr (cdr id-split-list)))) type)))

(define (module-for-id id)
    (let* ((id-elements (split-string id #\.)))
      (case-fold-down (car (cdr id-elements)))))

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

(define (revhistory-title-for-id id)
    (let ((id-elements (split-string id #\.)))
      (string-append "Revision History (" 
                     (case-fold-down (car (cdr id-elements)))
                     ")")))

(define (id-to-indexitem id)
    (let ((id-split-list (split-string id #\.)))
      (cond ((type-id-p id "METHOD") (method-signature-title-for-id id))
            ((type-id-p id "PROTOCOL") (refentry-title-for-description id))
            ((type-id-p id "MODULE") (refentry-title-for-description id))
            ((type-id-p id "TYPEDEF") (typedef-title-for-id id))
            ((type-id-p id "FUNCTION") (function-title-for-id id))
            ((type-id-p id "MACRO") (macro-title-for-id id))
            ((type-id-p id "GLOBAL") (global-title-for-id id))
            ((type-id-p id "REVHISTORY") (revhistory-title-for-id id))
            ((or (type-id-p id "SECT1")
                 (type-id-p id "REFERENCE")
                 (type-id-p id "EXAMPLE")
                 (type-id-p id "APPENDIX"))
             (data (select-elements (children (element-with-id id))
                                    "TITLE")))
            ((type-id-p id "SET") "This is a set")
            (#t (let ((msg (debug id))) "unknown")))))

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

(define (example-entry example-node match-protocol)
    (let ((label (attribute-string "LABEL" example-node)))
      (if label
          (let* ((parts (split-string label #\/))
                 (afterprotocol (cdr (cdr parts)))
                 (module (car parts))
                 (protocol (car (cdr parts))))
            (if (and match-protocol (string=? match-protocol protocol))
                (example-entry-text example-node)
                (empty-sosofo)))
          (if match-protocol
              (empty-sosofo)
              ($lot-entry$ example-node)))))

(mode formal-object-title-mode
      (element (example title)
               (let ((example-node (parent (current-node))))
                 (make paragraph
                       font-weight: 'bold
                       keep-with-next?: #t
                       (sosofo-append
                        (literal "Example")
                        (literal " ")
                        (literal (example-label example-node))
                        (literal " ")
                        (process-children))))))

(define (collect-example-protocols nl)
    (let loop ((nl nl))
         (if (node-list-empty? nl)
             '()
             (let* ((node (node-list-first nl))
                    (node-gi (gi node))
                    (child-protocols (collect-example-protocols (children node))))
               (append
                child-protocols
                (if (and node-gi (string=? (gi node) "EXAMPLE"))
                    (let ((label (attribute-string "LABEL" node)))
                      (if label
                          (let* ((parts (split-string label #\/))
                                 (module (car parts))
                                 (protocol (car (cdr parts))))
                            (cons (cons module protocol) (loop (node-list-rest nl))))
                          (loop (node-list-rest nl))))
                    (loop (node-list-rest nl))))))))

(define (uniqify duplicate-list)
    (let loop ((l duplicate-list))
         (if (null? l)
             '()
             (let ((item (car l)))
               (if (equal? (member item duplicate-list) l)
                   (cons item (loop (cdr l)))
                   (loop (cdr l)))))))

(define (extract-module-list protocol-list)
    (reverse
     (let loop ((l protocol-list) (last-module #f))
          (if (null? l)
              '()
              (let ((module (car (car l))))
                (if last-module
                    (if (string=? last-module module)
                        (loop (cdr l) module)
                        (cons module (loop (cdr l) module)))
                    (cons module (loop (cdr l) module))))))))

(define (filter-protocols protocol-list match-module)
    (reverse
     (let loop ((l protocol-list))
          (if (null? l)
              '()
              (let* ((pair (car l))
                     (module (car pair))
                     (protocol (cdr pair)))
               (if (string=? module match-module)
                   (cons protocol (loop (cdr l)))
                   (loop (cdr l))))))))

(define (lots-for-protocol nd lotgi match-protocol)
    (car (cdr
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
                                                  (sosofo-append sosofos (example-entry node match-protocol)))))
                                        (apply loop
                                               rest
                                               (accum-lot
                                                node
                                                last-node
                                                (sosofo-append sosofos 
                                                               (if match-protocol
                                                                   (empty-sosofo)
                                                                   ($lot-entry$ node))))))
                                    (apply loop rest (accum-lot node last-node sosofos))))))))))))

(define (build-lot nd lotgi)
    (let* ((protocol-list (reverse (uniqify (collect-example-protocols nd))))
           (module-list (extract-module-list protocol-list)))
      (make sequence
            (lot-title #t lotgi)
            (if (string=? lotgi "EXAMPLE")
                (make-list 
                 (let module-loop ((ml module-list))
                      (if (null? ml)
                          (empty-sosofo)
                          (sosofo-append
                           (make-listitem 0
                                          (example-title (car ml))
                                          (make-list
                                           (let protocol-loop ((pl (filter-protocols protocol-list (car ml))))
                                                (if (null? pl)
                                                    (empty-sosofo)
                                                    (sosofo-append
                                                     (make-listitem 1
                                                                    (example-title (car pl))
                                                                    (make-list (lots-for-protocol nd lotgi (car pl))))
                                                     (protocol-loop (cdr pl)))))))
                           (module-loop (cdr ml))))))
                (empty-sosofo))
            (lots-for-protocol nd lotgi #f))))
  
(element (varlistentry term) 
         (sosofo-append
          (process-children)
          (literal " ")))

(define ($revision$)
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
                    (make-linebreak)
                    (process-node-list revremark)
                    (make-linebreak)))))

(element (revhistory revision) ($revision$))
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

(define (common-titlepage-recto-elements)
    (list (normalize "title") 
          (normalize "subtitle")
          (normalize "graphic")
          (normalize "corpauthor")))
          
(define (common-titlepage-verso-elements)
 (list (normalize "copyright")
       (normalize "legalnotice")
       (normalize "pubdate")         
       (normalize "releaseinfo")
       (normalize "biblioset")
       (normalize "bookbiblio")
       (normalize "abstract")
       (normalize "revhistory")))

(define article-titlepage-recto-elements common-titlepage-recto-elements)
(define article-titlepage-verso-elements common-titlepage-verso-elements)
(define book-titlepage-recto-elements common-titlepage-recto-elements)
(define book-titlepage-verso-elements common-titlepage-verso-elements)
(define reference-titlepage-recto-elements common-titlepage-recto-elements)
(define reference-titlepage-verso-elements common-titlepage-verso-elements)
(define set-titlepage-recto-elements common-titlepage-recto-elements)
(define set-titlepage-verso-elements common-titlepage-verso-elements)

(define (releaseinfo)
    (sosofo-append
     (literal "Release ")
     (process-children)
     (make-linebreak)))

(mode article-titlepage-recto-mode (element revhistory (empty-sosofo)))
(mode book-titlepage-recto-mode (element revhistory (empty-sosofo)))
(mode reference-titlepage-recto-mode
      (element revhistory (empty-sosofo))
      (element abstract (empty-sosofo)))
(mode set-titlepage-recto-mode (element revhistory (empty-sosofo)))

(mode article-titlepage-verso-mode
      (element revhistory (revhistory))
      (element abstract (process-children))
      (element releaseinfo (releaseinfo)))
(mode book-titlepage-verso-mode
      (element revhistory (revhistory))
      (element abstract (process-children))
      (element releaseinfo (releaseinfo)))
(mode reference-titlepage-verso-mode
      (element revhistory (revhistory))
      (element abstract (process-children))
      (element releaseinfo (releaseinfo)))
(mode set-titlepage-verso-mode
      (element revhistory (revhistory))
      (element copyright (copyright))
      (element abstract (process-children))
      (element releaseinfo (releaseinfo)))

(element revhistory (empty-sosofo))
(element abstract (empty-sosofo))

</style-specification-body>
</style-specification>
</style-sheet>
