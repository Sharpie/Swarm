<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">

<style-sheet> 
<style-specification>
<style-specification-body>

;; Derived from http://www.jclark.com/xml/xpointer.zip
;; Modified to (sort of) understand the new XPointer syntax.

(define (xpointer s #!optional (here (current-node)))
  (let* ((lst (xpointer-start (xpointer-parse s))))
    (if (xpointer-valid? lst)
	(xpointer-ref lst here)
	(empty-node-list))))

(define (xpointer-ref lst here)
  (case (car lst)
    ((root) (xpointer-loc-terms (document-element here)
				#f
				(cddr lst))) ;; cddr to skip ()
    ((here) (xpointer-loc-terms here
				#f
				(cddr lst))) ;; cddr to skip ()
    ((id) (xpointer-loc-terms (element-with-id (caadr lst) here)
			      #f
			      (cddr lst)))))

(define (document-element #!optional (nd (current-node)))
  (node-property 'document-element
		 (node-property 'grove-root
				nd)))

(define (xpointer-loc-terms locsrc key lst)
  (cond ((node-list-empty? locsrc)
	 locsrc)
	((null? lst)
	 locsrc)
	((symbol? (car lst))
	 (xpointer-loc-terms locsrc (car lst) (cdr lst)))
	(else
	 (xpointer-loc-terms
	  (apply (case key
		   ((child) (stepper children))
		   ((descendant) (stepper descendants))
		   ((previous) (stepper preced #t))
		   ((next) (stepper follow))
		   ((ancestor) (stepper ancestors #t))
		   ((preceding) (stepper tree-before #t))
		   ((following) (stepper tree-after)))
		 (cons locsrc
		       (car lst)))
	  key
	  (cdr lst)))))

(define (tree-before nl)
  (node-list-map (lambda (snl)
		   (let ((pa (parent snl)))
		     (if (node-list-empty? pa)
			 (empty-node-list)
			 (node-list (tree-before pa)
				    pa
				    (subtree (preced snl))))))
		 nl))

(define (tree-after nl)
  (node-list-map (lambda (snl)
		   (let ((pa (parent snl)))
		     (if (node-list-empty? pa)
			 (empty-node-list)
			 (node-list (subtree (follow snl))
				    (tree-after pa)))))
		 nl))

(define (subtree nl)
  (node-list-map (lambda (snl)
		   (node-list snl
			      (descendants snl)))
		 nl))

(define (stepper f #!optional (reverse? #f))
  (lambda (locsrc occur #!optional (element '#t) #!rest attvals)
    (let ((nl
	   (select-elements (f locsrc)
			    (list element attvals))))
      (let ((occ (if reverse? (- occur) occur)))
	(if (> occ 0)
	    (node-list-ref nl (- occ 1))
	    (node-list-ref (node-list-reverse nl) (- (- occ) 1)))))))

(define (xpointer-start lst)
  (and lst
       (let loop ((lst lst)
		  (start '()))
	 (if (or (null? lst)
		 (equal? (car lst) '...))
	     (reverse start)
	     (loop (cdr lst)
		   (cons (car lst) start))))))

(define (xpointer-valid? lst)
  (and lst
       (not (null? lst))
       (case (car lst)
	 ((here root)
	  (xpointer-valid-terms? (cddr lst))) ;; cddr to skip ()
	 ((id)
	  (let ((rest (cdr lst)))
	    (and (not (null? rest))
		 (let ((arg (car rest)))
		   (and (pair? arg)
			(string? (car arg))
			(null? (cdr arg))))
		 (xpointer-valid-terms? (cdr rest)))))
	 (else
	  #f))))

;; 0 or more terms

(define (xpointer-valid-terms? lst)
  (or (null? lst)
      (case (car lst)
	((child descendant preceding following ancestor previous next)
	 (xpointer-valid-key-follow? (cdr lst)))
	(else
	 #f))))

;; 1 or more steps then 0 or more terms

(define (xpointer-valid-key-follow? lst)
  (and (not (null? lst))
       (xpointer-valid-step? (car lst))
       (xpointer-valid-step-follow? (cdr lst))))

;; 0 or more steps than 0 or more terms

(define (xpointer-valid-step-follow? lst)
  (or (null? lst)
      (let ((first (car lst)))
	(if (pair? first)
	    (and (xpointer-valid-step? first)
		 (xpointer-valid-step-follow? (cdr lst)))
	   (xpointer-valid-terms? lst)))))

(define (xpointer-valid-step? lst)
  (and (pair? lst)
       (integer? (car lst))
       (not (= (car lst) 0))
       (let ((rest (cdr lst)))
	 (or (null? rest)
	     (xpointer-valid-element? (car rest))
	     (xpointer-valid-attr-vals? (cdr rest))))))

(define (xpointer-valid-element? elem)
  (or (string? elem)
      (equal? elem #t)
      (equal? elem 'CDATA)))

(define (xpointer-valid-attr-vals? lst)
  (or (null? lst)
      (and (xpointer-valid-attr-name? (car lst))
	   (not (null? (cdr lst)))
	   (xpointer-valid-attr-val? (cadr lst))
	   (xpointer-valid-attr-vals? (cddr lst)))))

(define (xpointer-valid-attr-name? obj)
  (or (string? obj)
      (equal? obj #t)))

(define (xpointer-valid-attr-val? obj)
  (or (string? obj)
      (equal? obj #t)
      (equal? obj 'IMPLIED)))

(define (xpointer-parse str)
  (let ((len (string-length str)))
    (let loop ((i 0)
	       (in-paren? #f)
	       (tokens '()))
      (if (= i len)
	  (if in-paren?
	      #f
	      (reverse tokens))
	  (case (string-ref str i)
	    ((#\()
	     (if in-paren?
		 #f
		 (let ((ret (loop (+ i 1)
				  #t
				  '())))
		   (if ret
		       (append (reverse tokens) ret)
		       ret))))
	    ((#\))
	     (if in-paren?
		 (let ((ret
		       (loop (+ i 1)
			     #f
			     '())))
		   (if ret
		       (cons (reverse tokens)
			     ret)
		       ret))
		 #f))
	    ((#\, #\.)
	     (loop (+ i 1) in-paren? tokens))
	    ((#\" #\')
	     (let inner ((j (+ i 1)))
	       (cond ((>= j len)
		      #f)
		     ((equal? (string-ref str j)
			      (string-ref str i))
		      (loop (+ j 1)
			    in-paren?
			    (cons (substring str (+ i 1) j)
				  tokens)))
		     (else
		      (inner (+ j 1))))))
	    (else
	     (let ((convert
		    (lambda (s)
		      (let ((n (string->number s)))
			(if n
			    n
			    (if in-paren?
				(let ((len (string-length s)))
				  (if (and (> len 0)
					   (equal? (string-ref s 0)
						   #\*))
				      (if (= len 1)
					  #t
					  (string->symbol
					   (general-name-normalize
					    (substring s 1 len))))
				      s))
				(string->symbol
				 (general-name-normalize s))))))))
	       (let inner ((j (+ i 1)))
		 (if (>= j len)
		     (loop j
			   in-paren?
			   (cons (convert (substring str i j))
				 tokens))
		     (case (string-ref str j)
		       ((#\( #\, #\) #\.)
			(loop j
			      in-paren?
			      (cons (convert (substring str i j))
				    tokens)))
		       (else
			(inner (+ j 1)))))))))))))

(define (ancestors nl)
  (node-list-map (lambda (snl)
                   (let loop ((cur (parent snl))
                              (result (empty-node-list)))
                     (if (node-list-empty? cur)
                         result
                         (loop (parent snl)
                               (node-list cur result)))))
                 nl))


(define (cadr obj) (car (cdr obj)))
(define (caadr obj) (car (car (cdr obj))))
(define (cddr obj) (cdr (cdr obj)))

</style-specification-body>
</style-specification>
</style-sheet> 
