<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
<style-sheet>
<style-specification id="common">
<style-specification-body>

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

(define (new-class-p funcsynopsisinfo-node)
    (let ((classname (get-classname funcsynopsisinfo-node))
          (previous-classname
           ;; ipreced is implemented by DocBook
           (let ((listitem (ipreced (parent (parent funcsynopsisinfo-node)))))
             (if (node-list-empty? listitem)
                 #f
                 (data
                  (select-elements
                   (children
                    (select-elements
                     (children (select-elements (children listitem)
                                                "FUNCSYNOPSIS"))
                     "FUNCSYNOPSISINFO"))
                   "CLASSNAME"))))))
      (or (not previous-classname)
          (not (string=? previous-classname classname)))))

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
             (make-paragraph last-nl)
             (let ((node (node-list-first nl)))
               (if (char=? (node-property 'char node) #\U-000D)
                   (sosofo-append
                    (let ((last-line-nl (previous-nl last-nl node)))
                      (if (node-list-empty? last-line-nl)
                          (empty-sosofo)
                          (make-paragraph last-line-nl)))
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
         (expand-paragraphs (skip-nonchars
                             (children (current-node)))))

(define (embed-split string)
    (let loop ((last-ch #\U-0000) (l (string->list string)))
         (if (null? l)
             '()
             (let* ((ch (car l))
                    (next (loop ch (cdr l))))
               (if (and (char=? ch #\space) (not (char=? ch last-ch)))
                   (list next)
                   (cons ch next))))))

(define (split str)
    (let flatten-arg ((l (embed-split str)) (out-l '()))
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
  
</style-specification-body>
</style-specification>
</style-sheet>