(require 'cl)

(defvar *protocol-hash-table* (make-hash-table :test #'equal))

(defvar *module-hash-table* (make-hash-table))

(defconst *phases* '(:creating :setting :using))

(defvar *protocol-list*)

(defvar *method-signature-hash-table* (make-hash-table :test #'equal))

(defvar *method-signature-list*)

(defvar *general-example-counter-hash-table* (make-hash-table :test #'eq))

(defvar *method-example-counter-hash-table* (make-hash-table :test #'equal))

(defconst *swarm-modules* '(activity
                            analysis
                            collections
                            defobj
                            gui
                            objectbase
                            simtools
                            simtoolsgui
                            space))

(defstruct protocol
  module
  name
  summary
  description-list
  included-protocol-list
  example-list
  method-list
  expanded-methodinfo-list)

(defstruct method
  phase
  factory-flag
  return-type
  arguments
  description-list
  example-list)

(defconst *doc-types* '(:method-doc :summary-doc :description-doc))

(defun find-protocol ()
  (interactive)
  (re-search-forward "^@\\(protocol\\|deftype\\)" nil t))

(defun skip-whitespace ()
  (skip-chars-forward " \t\r\n"))

(defun skip-name ()
  (skip-chars-forward "[a-zA-Z_.][a-zA-Z0-9_.]")
  (point))

(defun next-paren-expr ()
  (when (looking-at "(")
    (let ((beg (point)))
      (forward-sexp)
      (buffer-substring (1+ beg) (- (point) 1)))))

(defun next-expr ()
  (list
   (progn
     (skip-whitespace)
     (next-paren-expr))
   (progn
     (skip-whitespace)
     (next-name))))

(defun end-of-line-position ()
  (save-excursion
    (end-of-line)
    (point)))

(defun parse-included-protocol-list ()
  (let ((eolpos (end-of-line-position)))
    (loop
     with beg = (search-forward "<" eolpos t)
     while beg
     for end = (re-search-forward "[ \t,>]" eolpos t)
     unless end do (error "Bad protocol syntax")
     do (backward-char)
     for next = (cond ((looking-at "[ \t,>]") 
                       (skip-chars-forward ", \t")
                       (if (looking-at ">")
                           nil
                         (point)))
                      (t (point)))
     collect (buffer-substring beg (- end 1))
     and do (setq beg next))))

(defun next-name ()
  (let* ((beg (point))
         (end (skip-name)))
    (prog1
        (buffer-substring beg end)
      (skip-whitespace))))

(defun parse-method (protocol-name
                     phase
                     factory-flag
                     method-description-list
                     method-example-list)
  (forward-char)
  (skip-whitespace)
  (let* ((return-type (next-paren-expr))
         arguments name)
    (loop
     unless (looking-at ":")
     do
     (setq name (next-name))
     (when (looking-at ";")
       (push (cons name nil) arguments))
     
     when (looking-at ":")
     do
     (forward-char)
     (push (cons name (next-expr)) arguments)
     (while (looking-at ",")
       (forward-char)
       (push (cons nil (next-expr)) arguments))

     until (looking-at ";"))
    (unless phase
      (error "No phase in protocol: " protocol-name))
    (make-method
     :phase phase
     :factory-flag factory-flag
     :arguments (reverse arguments)
     :return-type return-type
     :description-list method-description-list
     :example-list method-example-list)))

(defun test-parse-method ()
  (interactive)
  (let ((method (parse-method nil :creating nil nil)))
    (princ (list (method-return-type method)
                 (method-arguments method)
                 (method-example-list method)
                 ))))

(defun line-text ()
  (buffer-substring (point) (end-of-line-position)))

(defun general-example-counter (protocol)
  (let ((val (gethash protocol *general-example-counter-hash-table*)))
    (if val
        (progn
          (incf (gethash protocol *general-example-counter-hash-table*))
          val)
        (progn
          (setf (gethash protocol *general-example-counter-hash-table*) 1)
          0))))

(defun method-example-counter (protocol method)
  (let* ((key (cons protocol method))
         (val (gethash key *general-example-counter-hash-table*)))
    (if val
        (progn
          (incf (gethash key *general-example-counter-hash-table*))
          val)
        (progn
          (setf (gethash key *general-example-counter-hash-table*) 1)
          0))))

(defun load-protocol (module)
  (interactive)

  (skip-whitespace)
  (let* ((name
          (let ((beg (point)))
            (skip-name)
            (buffer-substring beg (point))))
         (included-protocol-list
          (parse-included-protocol-list))
         (last-tag nil)
         (description-doc-list nil)
         (summary-doc nil)
         (method-doc-list nil)
         (method-list nil)
         (global-example-list nil)
         (example-list nil)
         (phase nil)
         (tag nil)
         (buf nil))
    (beginning-of-line 1)
    (while (and (zerop (forward-line 1)) (not (eq tag :end)))
      (beginning-of-line)
      (cond ((looking-at "^CREATING")
             (setq tag :creating)
             (setq phase :creating))
            ((looking-at "^SETTING")
             (setq phase :setting)
             (setq tag :setting))
            ((looking-at "^USING")
             (setq phase :using)
             (setq tag :using))
            ((looking-at "^[ \t]*$") (setq tag :newline))
            ((looking-at "^-")
             (setq tag :method))
            ((looking-at "^+")
             (setq tag :factory-method))
            ((looking-at "^//M:") 
             (setq tag :method-doc))
            ((looking-at "^//S:") (setq tag :summary-doc))
            ((looking-at "^//D:") (setq tag :description-doc))
            ((looking-at "^//E:") (setq tag :example-doc))
            ((looking-at "^#if 0")
               (c-forward-conditional 1)
             (beginning-of-line 0))
            ((looking-at "^@end") (setq tag :end))
            ((looking-at "^// ") (setq tag :comment))
            ((looking-at "^///M:") (setq tag :bogus-method))
            (t (error "Unknown text: [%s]"
                      (buffer-substring (point) (end-of-line-position)))))
      (let ((line (line-text))
            (is-doc-type (and (member tag *doc-types*)
                              (not (looking-at "^#")))))
        (flet ((extract-doc-string (str) (substring str 5)))
          (if (eq tag last-tag)
              (if is-doc-type
                  (setq buf (concat 
                             (if (string-match " $" buf) 
                                 buf
                                 (concat buf " "))
                             (extract-doc-string line)))
                  (when (eq tag :example-doc)
                    (setq buf
                          (concat buf "\n" (extract-doc-string line)))))
              (progn
                (case last-tag
                  (:example-doc
                   (push buf example-list)
                   (unless (or method-list method-doc-list)
                     (setq global-example-list example-list)
                     (setq example-list nil)))
                  (:method-doc
                   (push buf method-doc-list))
                  (:summary-doc (if summary-doc
                                    (error "summary already set")
                                    (setq summary-doc buf)))
                  (:description-doc (push buf description-doc-list)))
                (when (or is-doc-type (eq tag :example-doc))
                  (setq buf (extract-doc-string line)))))))
      
      (when (member tag '(:method :factory-method))
        (push (parse-method name
                            phase
                            (eq tag :factory-method)
                            (reverse method-doc-list)
                            (reverse example-list))
              method-list)
        (setq method-doc-list nil))
      (setq last-tag tag))
    (make-protocol
     :module module
     :name name
     :example-list (reverse global-example-list)
     :included-protocol-list included-protocol-list
     :summary summary-doc
     :description-list (reverse description-doc-list)
     :method-list (reverse method-list))))

(defun load-protocols (module)
  (interactive)
  (loop
   while (find-protocol)
   collect (load-protocol module)))
   
(defun get-swarmhome ()
  (let ((swarmhome-env (getenv "SWARMHOME")))
    (if swarmhome-env
        swarmhome-env
        (if (> (length command-line-args 1))
            (last command-line-args)
            (error "Can't find SWARMHOME")))))

(defun pathname-for-module (module)
  (let ((module-name (symbol-name module)))
    (concat (get-swarmhome) "/src/" module-name "/" module-name ".h")))

(defun create-included-protocol-list (protocol)
  (loop for included-protocol-name in (protocol-included-protocol-list protocol)
        for included-protocol = (get-protocol included-protocol-name)
        unless included-protocol do (error "Could not find protocol %s"
                                           included-protocol-name)
        collect included-protocol))

(defun CREATABLE-protocol ()
  (let ((description "Declare that a defined type supports creation."))
    (make-protocol
     :module 'defobj
     :name "CREATABLE"
     :included-protocol-list nil
     :summary description
     :description-list (list description)
     :method-list nil)))

(defun add-protocol (module protocol)
  (setf (gethash (protocol-name protocol) *protocol-hash-table*) protocol)
  (push protocol (gethash module *module-hash-table*)))

(defun load-protocols-for-all-modules ()
  (interactive)

  (let ((old-push-mark (symbol-function 'push-mark)))

    (when noninteractive
      (setf (symbol-function 'push-mark)
            #'(lambda () 
                (funcall old-push-mark nil t))))

    (clrhash *protocol-hash-table*)
    (clrhash *module-hash-table*)
    (add-protocol 'defobj (CREATABLE-protocol))
    (loop for module in *swarm-modules*
          do
          (find-file-read-only (pathname-for-module module))
          (loop for protocol in (load-protocols module)
                for name = (protocol-name protocol)
                for exist = (gethash name *protocol-hash-table*)
                when exist do (error "Protocol %s already exists" name)
                do (add-protocol module protocol))
          (kill-buffer (current-buffer)))
    
    (when noninteractive
      (setf (symbol-function 'push-mark) old-push-mark))
    
    (loop for protocol being each hash-value of *protocol-hash-table*
          do
          (setf (protocol-included-protocol-list protocol)
                (create-included-protocol-list protocol)))))
  
(defun get-protocol (name)
  (gethash name *protocol-hash-table*))

(defun compare-string-lists (a b)
  (let ((diff
         (loop for a-arg in a
               for b-arg in b
               if (string< a-arg b-arg) return -1
               else if (not (string= a-arg b-arg)) return 1
               finally return 0)))
    (if (zerop diff)
        (< (length a) (length b))
        diff)))

(defun expand-protocol (protocol)
  (let ((expanded-protocols-hash-table (make-hash-table))
        (method-hash-table (make-hash-table)))
    (flet ((expand-protocol-level (protocol level)
             (setf (gethash protocol expanded-protocols-hash-table) t)
             (loop for method in (protocol-method-list protocol)
                   do (setf (gethash method method-hash-table) (cons level protocol)))
             (loop for included-protocol in
                   (protocol-included-protocol-list protocol)
                   do
                   (unless (gethash included-protocol expanded-protocols-hash-table)
                     (expand-protocol-level included-protocol (1+ level))))))
      (expand-protocol-level protocol 0))
    (sort 
     (loop for method being each hash-key of method-hash-table using (hash-value level.protocol)
           collect (list (car level.protocol)
                         (cdr level.protocol)
                         method))
     #'(lambda (a b)
         (flet ((phase-pos (phase)
                  (case phase
                    (:creating 0)
                    (:setting 1)
                    (:using 2)))
                (compare-arguments (a b)
                  (flet ((get-key-list (item) (mapcar #'first item)))
                    (compare-string-lists
                     (get-key-list a)
                     (get-key-list b)))))
           (let ((level-diff (- (first a) (first b))))
             (if (zerop level-diff)
                 (let* ((method-a (third a))
                        (method-b (third b))
                        (phase-diff (- (phase-pos (method-phase method-a))
                                       (phase-pos (method-phase method-b)))))
                   (if (zerop phase-diff)
                       (compare-arguments (method-arguments method-a)
                                          (method-arguments method-b))
                       (< phase-diff 0)))
                 (< level-diff 0))))))))

(defun expand-protocols ()
  (interactive)
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        ; (message (concat "Processing: " (protocol-name protocol)))
        (setf (protocol-expanded-methodinfo-list protocol)
              (expand-protocol protocol))))

(defun external-protocol-name (protocol)
  (let ((raw-protocol-name (protocol-name protocol)))
    (if (string= (substring raw-protocol-name 0 1)
                 "_")
        (substring raw-protocol-name 1)
        raw-protocol-name)))

(defun sgml-protocol-id (protocol)
  (let* ((cooked-protocol-name (external-protocol-name protocol)))
    (insert "\"SWARM.")
    (insert (upcase (symbol-name (protocol-module protocol))))
    (insert ".")
    (insert (upcase cooked-protocol-name))
    (insert "\"")))

(defun sgml-refentry-start (protocol)
  (insert "<REFENTRY ID=")
  (sgml-protocol-id protocol)
  (insert ">\n"))

(defun sgml-refmeta (protocol)
  (insert "<REFMETA>\n")
  (insert "<REFENTRYTITLE>")

  (insert (protocol-name protocol))
  
  (insert "</REFENTRYTITLE>\n")
  (insert "<REFMISCINFO>")
  (insert (symbol-name (protocol-module protocol)))
  (insert "</REFMISCINFO>\n")
  (insert "</REFMETA>\n"))

(defun sgml-namediv (protocol)
  (insert "<REFNAMEDIV>\n")
  (insert "<REFNAME>")
  (insert (protocol-name protocol))
  (insert "</REFNAME>\n")
  (insert "<REFPURPOSE>\n")
  (insert (protocol-summary protocol))
  (insert "\n</REFPURPOSE>\n")
  (insert "</REFNAMEDIV>\n"))

(defun count-methods-for-phase (protocol phase)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        count (eq (method-phase (third methodinfo)) phase)))

(defun count-methods-for-all-phases (protocol)
  (loop for phase in *phases*
        sum (count-methods-for-phase protocol phase)))

(defun insert-text (text)
  (when text
    (let ((beg (point)))
      (insert text)
      (let ((end (point)))
        (save-excursion
          (save-restriction
            (narrow-to-region beg end)
            (beginning-of-line)
            (save-excursion
              (while (search-forward "<" nil t)
                (replace-match "&lt;")))
            (save-excursion
              (while (search-forward ">" nil t)
                (replace-match "&gt;")))))))))

(defun sgml-refsect1 (protocol)
  (let ((have-methods (> (count-methods-for-all-phases protocol) 0))
        (description-list (protocol-description-list protocol)))
    (when (or description-list have-methods)
      (insert "<REFSECT1>\n")
      (insert "<TITLE>Description</TITLE>\n")
      (loop for description in description-list
            do 
            (insert "<PARA>\n")
            (insert-text description)
            (insert "\n</PARA>\n"))
      (when have-methods
        (sgml-refsect2-method-list protocol))
      (insert "</REFSECT1>\n"))))

(defun sgml-method-description (protocol method)
  (let ((descriptions (method-description-list method)))
    (insert "<FUNCSYNOPSISINFO>\n")
    (insert "<CLASSNAME>")
    (insert (protocol-name protocol))
    (insert "</CLASSNAME>\n")
    (loop for description in descriptions
          do
          (insert-text description)
          (insert "\n"))
    (insert "</FUNCSYNOPSISINFO>\n")))

(defun print-method-signature (method &optional stream)
  (if (method-factory-flag method)
      (princ "+" stream)
      (princ "-" stream))
  (loop for arguments in (method-arguments method)
        for key = (first arguments)
        when key 
        do
        (princ key stream)
        (when (third arguments)
          (princ ":" stream))))

(defun sgml-method-funcsynopsis (owner-protocol method)
  (insert "<FUNCSYNOPSIS>\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (let ((return-type (method-return-type method)))
    (when return-type
      (insert-text return-type)))
  (insert "<FUNCTION>")
  (print-method-signature method (current-buffer))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (let ((arguments (method-arguments method)))
    (if (and (eql (length arguments) 1)
             (null (third (first arguments))))
        (insert "<VOID>\n")
        (loop for arg in arguments
              do
              (let ((argname (third arg)))
                ;; In the no-argument case, argname will be nil.
                (when argname
                  (insert "<PARAMDEF>")
                  (insert-text (second arg))
                  (insert "<PARAMETER>")
                  (insert argname)
                  (insert "</PARAMETER>")
                  (insert "</PARAMDEF>\n"))))))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-method-description owner-protocol method)
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-link-to-protocol (protocol)
  (insert "<LINK LINKEND=")
  (sgml-protocol-id protocol)
  (insert ">")
  (insert (external-protocol-name protocol))
  (insert "</LINK>"))

(defun methodinfo-list-for-phase (protocol phase)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        when (eq (method-phase (third methodinfo)) phase)
        collect methodinfo))

(defun sgml-method-definitions (protocol phase)
  (let ((methodinfo-list (methodinfo-list-for-phase protocol phase))
        have-list have-item)
    (when methodinfo-list
      (insert "<ITEMIZEDLIST>\n")
      (loop with last-protocol = nil
            for methodinfo in methodinfo-list
            for level = (first methodinfo)
            for owner-protocol = (second methodinfo)
            for method = (third methodinfo)
            for new-group-flag = (not (eq owner-protocol last-protocol))
            
            when new-group-flag do
            (when have-list
              (insert "</ITEMIZEDLIST>\n")
              (setq have-list nil))
            (when have-item
              (insert "</LISTITEM>\n")
              (setq have-item nil))
            (insert "<LISTITEM>\n")
            (setq have-item t)
            (insert "<PARA>")
            (if (= level 0)
                (insert (external-protocol-name owner-protocol))
                (sgml-link-to-protocol owner-protocol))
            (insert "</PARA>\n")
            (when (= level 0)
              (setq have-list t)
              (insert "<ITEMIZEDLIST>\n"))
            
            do
            (when (= level 0)
              (insert "<LISTITEM>\n")
              (sgml-method-funcsynopsis owner-protocol method)
              (sgml-method-examples owner-protocol method)
              (insert "</LISTITEM>\n"))
            
            for last-protocol = owner-protocol)
      (when have-list
        (insert "</ITEMIZEDLIST>\n"))
      (when have-item
        (insert "</LISTITEM>\n"))
      (insert "</ITEMIZEDLIST>\n"))))

(defun protocol-index (protocol)
  (position protocol *protocol-list*))

(defun sgml-examples (protocol)
  (let ((example-list (protocol-example-list protocol)))
    (when example-list
      (insert "<EXAMPLE LABEL=\"")
      (insert (symbol-name (protocol-module protocol)))
      (insert " / ")
      (insert (external-protocol-name protocol))
      (insert (format " / %d" (general-example-counter protocol)))
      (insert "\">")
      (insert "<TITLE>\n")
      (insert "</TITLE>")
      (loop for example in example-list
            do
            (insert "<PROGRAMLISTING>\n")
            (insert example)
            (insert "</PROGRAMLISTING>\n"))
      (insert "</EXAMPLE>\n"))))

(defun count-method-examples (protocol phase)
  (loop for methodinfo in (methodinfo-list-for-phase protocol phase)
        for method = (third methodinfo)
        count (method-example-list method)))

(defun compare-method-signatures (method-a method-b)
  (let* ((method-a-signature 
          (with-output-to-string (print-method-signature method-a)))
         (method-b-signature
          (with-output-to-string (print-method-signature method-b))))
    (string< method-a-signature method-b-signature)))

(defun compare-methodinfo (a b)
  (let ((protocol-name-a (protocol-name (second a)))
        (protocol-name-b (protocol-name (second b))))
    (if (string= protocol-name-a protocol-name-b)
        (compare-method-signatures (third a) (third b))
        (string< protocol-name-a protocol-name-b))))

(defun method-signature-index (method-signature)
  (let ((method-signature 
         (with-output-to-string (print-method-signature method))))
    (position method-signature *method-signature-list* :test #'string=)))

(defun sgml-method-examples (protocol method)
  (when (method-example-list method)
    (insert "<EXAMPLE LABEL=\"")
    (insert (symbol-name (protocol-module protocol)))
    (insert " / ")
    (insert (external-protocol-name protocol))
    (insert " / ")
    (print-method-signature method (current-buffer))
    (insert (format " / %d" (method-example-counter protocol method)))
    (insert "\">")
    (insert "<TITLE>")
    (insert "</TITLE>\n")

    (insert "<PROGRAMLISTING>\n")

    (loop for example in (method-example-list method)
          do
          (insert example)
          (insert "\n"))
    (insert "</PROGRAMLISTING>\n")
    (insert "</EXAMPLE>\n")))

(defun sgml-methods-for-phase (protocol phase)
  (insert "<REFSECT3>\n")
  (insert "<TITLE>Phase: ")
  (insert (capitalize (substring (prin1-to-string phase) 1)))
  (insert "</TITLE>\n")
  (sgml-method-definitions protocol phase)
  (sgml-examples protocol)
  (insert "</REFSECT3>\n"))

(defun sgml-refsect2-method-list (protocol)
  (insert "<REFSECT2><TITLE>Methods</TITLE>\n")
  (loop for phase in *phases*
        do
        (unless (zerop (count-methods-for-phase protocol phase))
          (sgml-methods-for-phase protocol phase)))
  (insert "</REFSECT2>\n"))

(defun generate-refentry-for-protocol (protocol)
  (unless (string= (substring (protocol-name protocol) 0 1) "_")
    (sgml-refentry-start protocol)
    (sgml-refmeta protocol)
    (sgml-namediv protocol)
    (sgml-refsect1 protocol)
    (insert "</REFENTRY>\n")))

(defun generate-refentries-for-module (module)
  (let* ((module-name (symbol-name module))
         (filename (concat (get-swarmhome)
                           "/../swarmdocs/src/"
                           module-name
                           "/"
                           module-name
                           "pages.sgml")))
    (with-temp-file filename
      (loop for protocol in (sort 
                             (gethash module *module-hash-table*)
                             (lambda (protocol-a protocol-b)
                               (string< (protocol-name protocol-a)
                                        (protocol-name protocol-b))))
            do
            (generate-refentry-for-protocol protocol)))))

(defun generate-defobj ()
  (interactive)
  (generate-refentries-for-module 'defobj))

(defun generate-modules ()
  (interactive)
  (loop for module in *swarm-modules*
        do
        (generate-refentries-for-module module)))

(defun build-method-signature-hash-table ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (loop for method in (protocol-method-list protocol)
              do
              (setf (gethash 
                     (with-output-to-string (print-method-signature method))
                     *method-signature-hash-table*) method))))

(defun build-protocol-vector ()
  (setq *protocol-list*
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               collect protocol)
         #'(lambda (protocol-a protocol-b)
             (string< (protocol-name protocol-a)
                      (protocol-name protocol-b))))))

(defun build-method-vector ()
  (setq *method-signature-list*
        (sort
         (loop for method-signature being each hash-key of
               *method-signature-hash-table*
               collect method-signature)
         #'string<)))

(defun run-all ()
  (load-protocols-for-all-modules)
  (expand-protocols)
  (build-method-signature-hash-table)
  (build-protocol-vector)
  (build-method-vector)
  (generate-modules))

