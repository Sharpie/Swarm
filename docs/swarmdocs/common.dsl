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
                    (literal (substring signature start-pos (+ pos 1)))
                    (process-node-list (node-list-first paramdefs))
                    (next-pos (+ 1 pos) (+ 1 pos) (node-list-rest paramdefs)))
                   (next-pos start-pos (+ 1 pos) paramdefs))
               (if (= start-pos 0)
                   (literal (substring signature 0 signature-length))
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

(element FUNCDEF (type-expand (children (current-node))))
         
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

(element CLASSNAME (empty-sosofo))

(define (get-classname funcsynopsisinfo-node)
    (data (select-elements (children funcsynopsisinfo-node)
                           "CLASSNAME")))

(define (previous-nl last-nl end-node)
  (let loop ((nl last-nl))
        (let ((node (node-list-first nl)))
          (if (node-list=? node end-node)
              (empty-node-list)
              (node-list node
                         (loop (node-list-rest nl)))))))

(define (expand-paragraphs text-nl)
    (let loop ((last-nl text-nl)
               (nl text-nl))
         (if (node-list-empty? nl)
             (sosofo-append
               (make-linebreak)
               (process-node-list last-nl))
             (let ((node (node-list-first nl)))
               (if (char=? (node-property 'char node) #\U-000D)
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

(element FUNCSYNOPSISINFO
     (sosofo-append
	(make-linebreak)
        (expand-paragraphs (skip-nonchars
                            (children (current-node))))))

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

(define (has-phase-p id)
    (not (null? (cdr (cdr (cdr (split-string id #\.)))))))

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

(define (protocol-title-for-id id)
    (let* ((id-elements (split-string id #\.))
           (module-name (car (cdr id-elements)))
           (protocol-name (car (cdr (cdr id-elements))))
           (refentry (element-with-id
                      (string-append
                       "SWARM."
                       module-name
                       "."
                       protocol-name))))
      (data
       (select-elements
        (children (select-elements (children refentry) "REFMETA"))
        "REFENTRYTITLE"))))
          
(define (protocol-id-to-description protocol-id)
    (string-append
     (module-for-id protocol-id)
     "/"
     (protocol-title-for-id protocol-id)))
             
(define (method-signature-id-to-description method-signature-id)
    (let* ((id-elements (split-string method-signature-id #\.))
           (phase-abbrev (car (cdr (cdr (cdr id-elements))))))
      (string-append 
       (module-for-id method-signature-id)
       "/"
       (protocol-title-for-id method-signature-id)
       "/"
       (cond ((string=? phase-abbrev "PC") "Creating")
             ((string=? phase-abbrev "PS") "Setting")
             ((string=? phase-abbrev "PU") "Using")))))

(define (id-to-indexitem id)
    (if (has-phase-p id)
        (method-signature-id-to-description id)
        (protocol-id-to-description id)))

</style-specification-body>
</style-specification>
</style-sheet>
