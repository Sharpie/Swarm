(require 'cl)
(load (concat (getenv "SWARMDOCS") "common.el"))

(defvar *protocol-hash-table* (make-hash-table :test #'equal))

(defvar *module-hash-table* (make-hash-table))

(defconst *phases* '(:creating :setting :using))

(defvar *protocol-list*)

(defvar *method-signature-hash-table* (make-hash-table :test #'equal))

(defvar *method-signature-list*)

(defvar *general-example-counter-hash-table* (make-hash-table :test #'eq))

(defvar *method-example-counter-hash-table* (make-hash-table :test #'equal))

(defstruct module
  name
  summary
  description-list
  global-list
  macro-list
  typedef-list
  example-list)

(defstruct protocol
  module
  name
  summary
  description-list
  included-protocol-list
  macro-list
  function-list
  global-list
  typedef-list
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

(defstruct global
  name
  module
  protocol
  type
  description-list)

(defstruct macro
  name
  module
  protocol
  arguments
  description-list)

(defstruct typedef
  name
  module
  protocol
  type
  description-list)

(defstruct function
  name
  module
  protocol
  return-type
  arguments
  description-list
  example-list)

(defstruct parse-state
  tag
  last-tag
  phase

  buf

  summary-doc
  description-doc-list

  scratch-doc-list ; used for global, macro and typedef

  global-type
  global-name
  global-names
  global-list

  function-return-type
  function-name
  function-list

  macro-name
  macro-list

  item-doc-list ; used for methods and functions
  method-list

  typedef-list

  scratch-example-list
  example-list)

(defconst *doc-types* '(:method-doc :summary-doc :description-doc
                        :macro-doc
                        :global-doc :global-begin :global-end :global-break
                        :example-doc))

(defconst *protocol-regexp* "^@\\(protocol\\|deftype\\)")

(defun find-protocol ()
  (interactive)
  (re-search-forward *protocol-regexp* nil t))

(defun skip-whitespace ()
  (skip-chars-forward " \t\r\n"))

(defun skip-whitespace-backward ()
  (skip-chars-backward " \t\r\n")
  (when (looking-at "\\s-")
    (backward-char)))

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

(defun parse-method (protocol
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
     (when (looking-at "//")
       (beginning-of-line 2))
     until (looking-at ";"))
    (unless phase
      (error "No phase in protocol: %s" (protocol-name protocol)))
    (make-method
     :phase phase
     :factory-flag factory-flag
     :arguments (reverse arguments)
     :return-type return-type
     :description-list method-description-list
     :example-list method-example-list)))

(defun test-parse-method ()
  (interactive)
  (let ((method (parse-method nil :creating nil nil nil)))
    (princ (list (method-return-type method)
                 (method-arguments method)
                 (method-example-list method)
                 ))))

(defun parse-function (module
                       protocol
                       function-return-type
                       function-name
                       item-list
                       example-list)
  (make-function
   :name function-name
   :module module
   :protocol protocol
   :return-type function-return-type
   :description-list item-list
   :example-list example-list
   :arguments
   (progn
     (search-forward "(")
     (loop do (skip-whitespace)
           for start-pos = (point)
           do
           (re-search-forward "[),]")
           (backward-char 2)
           (skip-whitespace-backward)
           for arg = (buffer-substring start-pos (1+ (point)))
           collect
           (if (string-match "\\(.*[^a-zA-Z_]\\)\\([a-zA-Z_]+\\)" arg)
               (cons (match-string 1 arg) (match-string 2 arg))
               (cons arg nil))
           do
           (re-search-forward "[),]")
           (backward-char)
           until (looking-at ")")
           do
           (forward-char)))))

(defun test-parse-function ()
  (interactive)
  (let ((func (parse-function nil nil nil nil nil nil)))
    (princ (list (function-return-type method)
                 (function-arguments method)))))

(defun handle-function (module protocol parse-state)
  (push (parse-function protocol
                        module
                        (parse-state-function-return-type parse-state)
                        (parse-state-function-name parse-state)
                        (reverse (parse-state-item-doc-list parse-state))
                        (reverse
                         (parse-state-scratch-example-list parse-state)))
        (parse-state-function-list parse-state)))

(defun line-text ()
  (buffer-substring (point) (end-of-line-position)))

(defun general-example-counter (protocol)
  (let ((index
         (let ((val (gethash protocol *general-example-counter-hash-table*)))
           (if val
               (progn
                 (incf (gethash protocol *general-example-counter-hash-table*))
                 val)
               (progn
                 (setf (gethash protocol *general-example-counter-hash-table*) 1)
                 0)))))
    (1+ index)))
        
(defun method-example-counter (protocol method)
  (let ((index
         (let* ((key (cons protocol method))
                (val (gethash key *general-example-counter-hash-table*)))
           (if val
               (progn
                 (incf (gethash key *general-example-counter-hash-table*))
                 val)
               (progn
                 (setf (gethash key *general-example-counter-hash-table*) 1)
                 0)))))
    (1+ index)))

(defun parse-global-using-parse-state (module protocol parse-state)
  (make-global
   :name (parse-state-global-name parse-state)
   :module module
   :protocol protocol
   :type (parse-state-global-type parse-state)
   :description-list
   (if (eq (parse-state-tag parse-state) :global)
       (parse-state-scratch-doc-list parse-state)
       (list (extract-doc-string line)))))
   
(defun immediate-global-tag-processed (module protocol parse-state line)
  (when (member (parse-state-tag parse-state)
                '(:global-begin :global-end :global-break))
    (prog1
        (parse-global-using-parse-state module protocol parse-state)
      (setf (parse-state-scratch-doc-list parse-state) nil))))
    
(defun is-doc-type (parse-state)
  (member (parse-state-tag parse-state) *doc-types*))

(defun extract-doc-string (str)
  (if (> (length str) 5)
      (substring str 5)
      ""))

(defun set-buf (parse-state line)
  (setf (parse-state-buf parse-state)
        (extract-doc-string line)))

(defun append-buf (parse-state line)
  (let ((tag (parse-state-tag parse-state)))
    (when (is-doc-type parse-state)
      (let ((buf (parse-state-buf parse-state)))
        (setf (parse-state-buf parse-state)
              (if (eq tag :example-doc)
                  (concat buf "\n" (extract-doc-string line))
                  (concat
                   (if (string-match " $" buf) buf (concat buf " "))
                   (extract-doc-string line))))))))


(defun update-global-name (parse-state)
  (unless (save-excursion
            (beginning-of-line)
            (looking-at "\\s-+//G:"))
    (setf (parse-state-global-name parse-state)
          (save-excursion
            (backward-word 1) 
            (let ((beg (point)))
              (forward-word 1)
              (buffer-substring beg (point)))))))

(defun check-global-doc-post (parse-state)
  (search-forward "//G:")
  (backward-char 4)
  (let ((last-extern (parse-state-global-name parse-state)))
    (update-global-name parse-state)
    (cond ((save-excursion
             (beginning-of-line)
             (looking-at ".*;.*//G:"))
           :global-end)
          ((not (string= last-extern
                         (parse-state-global-name parse-state)))
           :global-break)
          (t :global-doc))))

(defun check-global (parse-state)
  (let ((tag :global))
    (re-search-forward "\\s-+")
    (setf (parse-state-global-type parse-state)
          (let ((beg (point)))
            (re-search-forward "\\(;\\|//G:\\|,\\)")
            (backward-char)
            (let ((have-global-tag (looking-at ":")))
              (if have-global-tag
                  (setq tag :global-begin)
                  (progn
                    (setq have-global-tag
                          (search-forward "//G:"
                                          (end-of-line-position) t))
                    (when have-global-tag
                      (setq tag :global-end)
                      (backward-char))))
              (cond (have-global-tag
                     (backward-char 3)
                     (update-global-name parse-state)
                     (backward-word 1)
                     (skip-whitespace-backward)
                     (forward-char))
                    ((looking-at ",")
                     (backward-char)
                     (skip-whitespace-backward)
                     (backward-word 1)
                     (setf (parse-state-global-name parse-state) nil)
                     (setq tag :global-names)
                     (save-excursion
                       (let ((beg (point)))
                         (search-forward ";")
                         (backward-char)
                         (setf (parse-state-global-names parse-state)
                               (buffer-substring beg (point))))))))
            (buffer-substring beg (point))))
    (when (or (eq tag :global-begin) (eq tag :global-end))
      (search-forward "//G: ")
      (backward-char 5))
    (when (eq :global-names tag)
      (search-forward ";")
      (setq tag :global))
    tag))

(defun skip-c-comment ()
  (interactive)
  (search-forward "*/")
  (beginning-of-line 2))

(defun check-common-tags (parse-state)
  (cond ((looking-at "[ \t]*$") :newline)
        ((looking-at "//S:") :summary-doc)
        ((looking-at "//D:") :description-doc)
        ((looking-at "//#:") :macro-doc)
        ((looking-at "//G:") :global-doc)
        ((looking-at ".+//G:") (check-global-doc-post parse-state))
        ((looking-at "#if 0")
         (c-forward-conditional 1)
         (beginning-of-line 0)
         :ifdef-comment)
        ((looking-at "#ifndef")
         :ifndef)
        ((looking-at "#if [^0]")
         :if)
        ((looking-at "#endif")
         :endif)
        ((looking-at "#else")
         :else)
        ((looking-at "#undef")
         :undef)
        ((looking-at "/\\*")
         (skip-c-comment)
         :c-comment)
        ((looking-at "#import")
         :import)
        ((looking-at "")
         :page-break)
        ((looking-at "typedef\\s-+")
         :typedef)
        ((or (looking-at "// ") (looking-at "//$")) :objc-comment)
        ((looking-at "#define\\s-+[^(]+$")
         :define)
        ((looking-at "#define\\s-+\\([^(]+\\)\\(([^)]*)\\)")
         (setf (parse-state-macro-name parse-state) (match-string 1))
         :macro)
        ((looking-at "@class")
         :class)
        ((looking-at "\\(extern\\s-+\\)?\\(Class\\s-+\\|int\\s-+\\|void\\s-\\|id\\s-+<.*\\s-\\|id\\s-\\|const\\s-+char\\s-*\\*\\)\\s-*\\([^ (]+\\)\\s-*(")
         (setf (parse-state-function-return-type parse-state) (match-string 2))
         (setf (parse-state-function-name parse-state) (match-string 3))
         (search-forward ";")
         :function)
        ((looking-at "extern\\s-[^(]*$") (check-global parse-state))
        (t nil)))
  
(defun check-protocol-tags (parse-state)
  (let ((tag
         (cond ((looking-at "CREATING") :creating)
               ((looking-at "SETTING") :setting)
               ((looking-at "USING") :using)
               ((looking-at "-")  :method)
               ((looking-at "+") :factory-method)
               ((looking-at "//M:") :method-doc)
               ((looking-at "//F:") :function-doc)
               ((looking-at "@end") :protocol-end)
               ((looking-at "//E:") :example-doc)
               ((looking-at "///M:") :bogus-method)
               (t nil))))
    (when (member tag '(:creating :setting :using))
      (setf (parse-state-phase parse-state) tag))
    tag))

(defun protocol-tag-change (parse-state)
  (let ((buf (parse-state-buf parse-state)))
    (case (parse-state-last-tag parse-state)
      (:example-doc
       (push (concat buf "\n") (parse-state-scratch-example-list parse-state))
       (unless (or (parse-state-method-list parse-state)
                   (parse-state-item-doc-list parse-state))
         (setf (parse-state-example-list parse-state)
               (parse-state-scratch-example-list parse-state))
         (setf (parse-state-scratch-example-list parse-state) nil)))
      ((:method-doc :function-doc)
       (push buf (parse-state-item-doc-list parse-state))))))

(defun common-tag-change (parse-state)
  (let ((buf (parse-state-buf parse-state)))
    (case (parse-state-last-tag parse-state)
      (:macro-doc
       (push buf (parse-state-scratch-doc-list parse-state)))
      ((:global-doc :global)
       (push buf (parse-state-scratch-doc-list parse-state)))
      (:function-doc
       (push buf (parse-state-item-doc-list parse-state)))
      (:summary-doc
       (if (parse-state-summary-doc parse-state)
           (error "summary already set")
           (setf (parse-state-summary-doc parse-state) buf)))
      (:description-doc
       (push buf (parse-state-description-doc-list parse-state))))))

(defun handle-method (protocol factory-flag parse-state)
  (push (parse-method protocol
                      (parse-state-phase parse-state)
                      factory-flag
                      (reverse (parse-state-item-doc-list parse-state))
                      (reverse (parse-state-scratch-example-list parse-state)))
        (parse-state-method-list parse-state))
  (setf (parse-state-scratch-example-list parse-state) nil)
  (setf (parse-state-item-doc-list parse-state) nil)
  t)
       
(defun parse-typedef (module protocol description-list)
  (forward-char 7)
  (skip-whitespace)
  (let ((type-beg (point)))
    (if (looking-at ".*\\(union\\|struct\\)")
        (progn
          (search-forward "{")
          (backward-char)
          (forward-sexp)
          (let ((type-end (point)))
            (skip-whitespace)
            (let ((name-beg (point)))
              (search-forward ";")
              (backward-char)
              (skip-whitespace-backward)
              (make-typedef
               :name (buffer-substring name-beg (1+ (point)))
               :module module
               :protocol protocol
               :type (buffer-substring type-beg type-end)
               :description-list description-list))))
        (progn
          (search-forward ";")
          (backward-char 2)
          (skip-whitespace-backward)
          (let ((name-end (point)))
            (backward-char)
            (backward-word 1)
            (let ((name-beg (point)))
              (backward-char)
              (skip-whitespace-backward)
              (let ((type-end (1+ (point))))
                (make-typedef
                 :name (buffer-substring name-beg name-end)
                 :module module
                 :protocol protocol
                 :type (buffer-substring type-beg type-end)
                 :description-list description-list))))))))
                                         
(defun test-parse-typedef ()
  (interactive)
  (let ((typedef (parse-typedef nil nil nil)))
    (message "%s/%s"
             (typedef-name typedef)
             (typedef-type typedef))))
             
(defun handle-typedef (module protocol parse-state)
  (push (parse-typedef module 
                       protocol 
                       (parse-state-scratch-doc-list parse-state))
        (parse-state-typedef-list parse-state))
  (setf (parse-state-scratch-doc-list parse-state) nil)
  t)

(defun parse-macro (module protocol parse-state)
  (prog1
      (let ((dl (parse-state-scratch-doc-list parse-state))
            (name (parse-state-macro-name parse-state)))
        (search-forward "(")
        (let ((arguments
               (loop do (skip-whitespace)
                     for start-pos = (point)
                     do
                     (re-search-forward "[),]")
                     (backward-char 2)
                     (skip-whitespace-backward)
                     for arg = (buffer-substring start-pos (1+ (point)))
                     collect arg
                     do
                     (re-search-forward "[),]")
                     (backward-char)
                     until (looking-at ")")
                     do
                     (forward-char))))
          (make-macro
           :name name
           :module module
           :protocol protocol
           :arguments arguments
           :description-list dl)))
    (while (looking-at ".*\\\\\\s-*$")
      (forward-line))))

(defun handle-macro (module protocol parse-state)
  (push (parse-macro module protocol parse-state)
        (parse-state-macro-list parse-state))
  (setf (parse-state-scratch-doc-list parse-state) nil)
  t)

(defun handle-global (module protocol parse-state)
  (let ((names (parse-state-global-names parse-state)))
    (if names
        (progn
          (loop for name in (split-string names ",")
                for stripped-name = (strip-regexp name "\\s-+")
                do
                (push
                 (make-global
                  :name stripped-name
                  :module module
                  :protocol protocol
                  :type (parse-state-global-type parse-state)
                  :description-list
                  (parse-state-scratch-doc-list parse-state))
                 (parse-state-global-list parse-state)))
          (setf (parse-state-global-names parse-state) nil))
        (push (parse-global-using-parse-state module protocol parse-state)
              (parse-state-global-list parse-state)))
    (setf (parse-state-scratch-doc-list parse-state) nil))
  t)
  
(defun handle-protocol-tag (protocol parse-state)
  (let ((tag (parse-state-tag parse-state)))
    (case tag
      ((:method :factory-method)
       (handle-method protocol
                      (eq (parse-state-tag parse-state) :factory-method)
                      parse-state))
      (:global
       (handle-global nil protocol parse-state))
      (:macro
       (handle-macro nil protocol parse-state))
      (:typedef
       (handle-typedef nil protocol parse-state))
      (:function
       (handle-function nil protocol parse-state))
      (:protocol-end t)
      (otherwise nil))))

(defun handle-common-tag (module protocol parse-state)
  (let ((tag (parse-state-tag parse-state)))
    (case tag
      (:global
       (handle-global module protocol parse-state))
      (:macro
       (handle-macro module protocol parse-state))
      (:typedef
       (handle-typedef module protocol parse-state))
      (:function
       (handle-function module protocol parse-state))
      (otherwise nil))))

(defun same-tag-p (parse-state)
  (eq (parse-state-tag parse-state)
      (parse-state-last-tag parse-state)))

(defun end-tag-p (parse-state)
  (eq (parse-state-tag parse-state) :protocol-end))

(defun process-header-file (module protocol)
  (let ((parse-state (make-parse-state)))
    (beginning-of-line 1)
      (while (and (zerop (forward-line 1))
                  (not (and protocol (end-tag-p parse-state))))
        (beginning-of-line)
        (let ((tag (check-common-tags parse-state)))
          (unless tag
          (if protocol
              (progn
                (setq tag (check-protocol-tags parse-state))
                (unless tag
                  (error "Unrecognized text (protocol): [%s]"
                         (line-text))))
              (if (looking-at *protocol-regexp*)
                  (progn
                    (re-search-forward "^@end")
                    (setq tag :skipped-protocol))
                  (error "Unrecognized text (non-protocol): [%s]"
                         (line-text)))))
        (setf (parse-state-tag parse-state) tag))
      (let ((line (line-text)))
        (let ((immediate-object
               (immediate-global-tag-processed module
                                               protocol
                                               parse-state line)))
          (if immediate-object
              (push immediate-object (parse-state-global-list parse-state))
              (progn
                (if (same-tag-p parse-state)
                    (append-buf parse-state line)
                    (progn
                      (if protocol
                          (unless (protocol-tag-change parse-state)
                            (common-tag-change parse-state))
                          (common-tag-change parse-state))
                      (when (is-doc-type parse-state)
                        (set-buf parse-state line))))))))
      (if protocol
          (unless (handle-protocol-tag protocol parse-state)
            (handle-common-tag module protocol parse-state))
          (handle-common-tag module protocol parse-state))
      (setf (parse-state-last-tag parse-state)
            (parse-state-tag parse-state)))
    parse-state))
        
(defun load-protocol (module)
  (interactive)
  (skip-whitespace)
  (let* ((protocol-name
          (let ((beg (point)))
            (skip-name)
            (buffer-substring beg (point))))
         (included-protocol-list
          (parse-included-protocol-list))
         (protocol (make-protocol
                    :module module
                    :name protocol-name
                    :included-protocol-list included-protocol-list)))
    (let ((parse-state (process-header-file module protocol)))
      (setf (protocol-summary protocol)
            (parse-state-summary-doc parse-state)
            
            (protocol-description-list protocol)
            (reverse (parse-state-description-doc-list parse-state))
            
            (protocol-macro-list protocol)
            (reverse (parse-state-macro-list parse-state))

            (protocol-global-list protocol)
            (reverse (parse-state-global-list parse-state))

            (protocol-method-list protocol)
            (reverse (parse-state-method-list parse-state))

            (protocol-typedef-list protocol)
            (reverse (parse-state-typedef-list parse-state))

            (protocol-example-list protocol)
            (reverse (parse-state-example-list parse-state))))
    protocol))
            
(defun load-protocols (module)
  (interactive)
  (goto-char (point-min))
  (loop
   while (find-protocol)
   collect (load-protocol module)))

(defun load-module (module)
  (goto-char (point-min))
  (let ((parse-state (process-header-file module nil)))
    (make-module
     :name (symbol-name module)
     :summary (parse-state-summary-doc parse-state)
     :description-list
     (reverse (parse-state-description-doc-list parse-state))
     :example-list (reverse (parse-state-example-list parse-state))
     :macro-list
     (reverse (parse-state-macro-list parse-state))
     :global-list
     (reverse (parse-state-global-list parse-state))
     :typedef-list
     (reverse (parse-state-typedef-list parse-state)))))

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

(defun get-module (module-spec)
  (if (consp module-spec) (car module-spec) module-spec))

(defun load-all-modules ()
  (interactive)

  (let ((old-push-mark (symbol-function 'push-mark)))

    (when noninteractive
      (setf (symbol-function 'push-mark)
            #'(lambda () 
                (funcall old-push-mark nil t))))

    (clrhash *protocol-hash-table*)
    (clrhash *module-hash-table*)
    (add-protocol 'defobj (CREATABLE-protocol))
    (loop for module-spec in *swarm-modules*
          for module = (get-module module-spec)
          do
          (if (consp module-spec)
              (find-file-read-only 
               (pathname-for-module module (cdr module-spec)))
              (find-file-read-only (pathname-for-module module)))
          (load-module module)
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

(defun generate-expanded-methodinfo-list (protocol)
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

(defun generate-expanded-methodinfo-lists ()
  (interactive)
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (setf (protocol-expanded-methodinfo-list protocol)
              (generate-expanded-methodinfo-list protocol))))

(defun external-protocol-name (protocol)
  (let ((raw-protocol-name (protocol-name protocol)))
    (if (internal-protocol-p protocol)
        (substring raw-protocol-name 1)
        raw-protocol-name)))

(defun get-method-signature (method)
  (with-output-to-string (print-method-signature method)))

(defun protocol-index (protocol)
  (position protocol *protocol-list*))

(defun method-signature-index (method-signature)
  (position method-signature *method-signature-list* :test #'string=))

(defun sgml-object-id (type module protocol &optional name)
  (cook-id
   (let ((base-id
          (if protocol
              (let* ((cooked-protocol-name (external-protocol-name protocol)))
                (concat "SWARM."
                        (upcase (symbol-name (protocol-module protocol)))
                        "."
                        (upcase cooked-protocol-name)
                        "."
                        type))
              (concat "SWARM."
                      (upcase (symbol-name module))
                      ".GENERIC."
                      type))))
     (if name
         (concat base-id "." (upcase name))
         base-id))))

(defun sgml-protocol-id (protocol)
  (sgml-object-id "PROTOCOL"
                  (protocol-module protocol)
                  protocol))

(defun sgml-method-signature-id (protocol phase method-signature)
  (sgml-object-id "METHOD"
                  (protocol-module protocol)
                  protocol
                  (format "P%s.M%d" 
                          (case phase
                            (:creating "C")
                            (:setting "S")
                            (:using "U")
                            (otherwise (error "bad phase")))
                          (method-signature-index method-signature))))

(defun sgml-refentry-start (protocol)
  (insert "<REFENTRY ID=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">\n"))

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

(defun sgml-refsect1-text-list (title text-list)
  (when text-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (loop for text in text-list
          do 
          (insert "<PARA>\n")
          (insert-text text)
          (insert "\n</PARA>\n"))
    (insert "</REFSECT1>\n")))

(defun sgml-refsect1-description (protocol)
  (sgml-refsect1-text-list "Description"
                           (protocol-description-list protocol)))


(defun sgml-funcsynopsisinfo (class-name description-list)
  (insert "<FUNCSYNOPSISINFO>\n")
  (insert "<CLASSNAME>")
  (insert class-name)
  (insert "</CLASSNAME>\n")
  (loop for description in description-list
          do
          (insert-text description)
          (insert "\n"))
    (insert "</FUNCSYNOPSISINFO>\n"))

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
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-method-signature-id owner-protocol
                                    (method-phase method)
                                    (get-method-signature method)))
  (insert "\">\n")
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
  (sgml-funcsynopsisinfo (protocol-name owner-protocol)
                         (method-description-list method))
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-link-to-protocol (protocol)
  (insert "<LINK LINKEND=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</LINK>"))

(defun methodinfo-list-for-phase (protocol phase)
  (loop for methodinfo in (protocol-expanded-methodinfo-list protocol)
        when (eq (method-phase (third methodinfo)) phase)
        collect methodinfo))

(defun include-p (level protocol owner-protocol)
  (or (zerop level)
      (let ((owner-protocol-name (protocol-name owner-protocol)))
        (when (internal-protocol-p owner-protocol)
          (string=
           (substring (protocol-name owner-protocol) 1)
           (protocol-name protocol))))))

(defun count-included-methodinfo-entries (protocol phase)
  (loop for methodinfo in (methodinfo-list-for-phase protocol phase)
        count (include-p (first methodinfo)
                         protocol
                         (second methodinfo))))

(defun count-included-methodinfo-entries-for-all-phases (protocol)
  (loop for phase in *phases*
        sum (count-included-methodinfo-entries protocol phase)))

(defun sgml-method-definitions (protocol
                                phase
                                &optional protocol-listitem-flag)
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (let ((methodinfo-list (methodinfo-list-for-phase protocol phase))
          have-list have-item)
      (when protocol-listitem-flag
        (insert "<ITEMIZEDLIST>\n"))
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
            (when protocol-listitem-flag
              (when (include-p level protocol owner-protocol)
                (insert "<LISTITEM>\n")
                (setq have-item t)
                (insert "<PARA>")
                (insert (external-protocol-name owner-protocol))
                (insert "</PARA>\n")))
            (when (include-p level protocol owner-protocol)
              (setq have-list t)
              (insert "<ITEMIZEDLIST>\n"))
              
            do
            (when (include-p level protocol owner-protocol)
              (insert "<LISTITEM>\n")
              (sgml-method-funcsynopsis owner-protocol method)
              (sgml-method-examples owner-protocol method)
              (insert "</LISTITEM>\n"))
            
            for last-protocol = owner-protocol)
      (when have-list
        (insert "</ITEMIZEDLIST>\n"))
      (when protocol-listitem-flag
        (when have-item
          (insert "</LISTITEM>\n"))
        (insert "</ITEMIZEDLIST>\n")))))

(defun sgml-macro (macro)
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-object-id "MACRO"
                          (macro-module macro)
                          (macro-protocol macro)
                          (macro-name macro)))
  (insert "\">\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (insert "<FUNCTION>")
  (insert-text (macro-name macro))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (loop for arg in (macro-arguments macro)
        do
        (when arg
          (insert "<PARAMDEF>")
          (insert "<PARAMETER>")
          (insert arg)
          (insert "</PARAMETER>")
          (insert "</PARAMDEF>\n")))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-funcsynopsisinfo "(MACRO)"
                         (macro-description-list macro))
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-function (function)
  (insert "<FUNCSYNOPSIS ID=\"")
  (insert (sgml-object-id "FUNCTION"
                          (function-module function)
                          (function-protocol function)
                          (function-name function)))
  (insert "\">\n")
  (insert "<FUNCPROTOTYPE>\n")
  (insert "<FUNCDEF>")
  (insert-text (function-return-type function))
  (insert "<FUNCTION>")
  (insert-text (function-name method))
  (insert "</FUNCTION>")
  (insert "</FUNCDEF>\n")
  (loop for type.name in (function-arguments function)
        do
        (when arg
          (insert "<PARAMDEF>")
          (insert-text (car type.name))
          (insert "<PARAMETER>")
          (insert-text (cdr type.name))
          (insert "</PARAMETER>")
          (insert "</PARAMDEF>\n")))
  (insert "</FUNCPROTOTYPE>\n")
  (sgml-funcsynopsisinfo "(FUNCTION)"
                         (function-description-list function))
  (insert "</FUNCSYNOPSIS>\n"))

(defun sgml-typedef (typedef)
  (insert "<PARA ID=\"")
  (insert (sgml-object-id "TYPEDEF"
                          (typedef-module typedef)
                          (typedef-protocol typedef)
                          (typedef-name typedef)))
  (insert "\">\n")
  (insert (typedef-name typedef))
  (insert "<TYPE>\n")
  (insert (typedef-type typedef))
  (insert "</TYPE>\n")
  (insert "</PARA>\n"))

(defun sgml-refsect1-object-list (title
                                  object-list
                                  print-object-func)
  (when object-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert-text title)
    (insert "</TITLE>\n")
    (insert "<ITEMIZEDLIST>\n")
    (loop for object in object-list
          do
          (insert "<LISTITEM>\n")
          (funcall print-object-func object)
          (insert "</LISTITEM>\n"))
    (insert "</ITEMIZEDLIST>\n")))

(defun sgml-refsect1-macro-list (macro-list)
  (sgml-refsect1-object-list "Macros"
                             macro-list
                             #'sgml-macro))

(defun sgml-refsect1-typedef-list (typedef-list)
  (sgml-refsect1-object-list "Typedefs"
                             typedef-list
                             #'sgml-typedef))

(defun sgml-refsect1-function-list (function-list)
  (sgml-refsect1-object-list "Functions"
                             function-list
                             #'sgml-function))

(defun sgml-refsect1-global-list (global-list)
  (when global-list
    (insert "<REFSECT1>\n")
    (insert "<TITLE>")
    (insert "Globals")
    (insert "</TITLE>\n")
    (insert "<VARIABLELIST>\n")
    (loop for global in global-list
          do
          (insert "<VARLISTENTRY ID=\"")
          (insert (sgml-object-id "GLOBAL"
                                  (global-module global)
                                  (global-protocol global)
                                  (global-name global)))
          (insert "\">\n")
          (insert "<TERM>")
          (insert-text (global-type global))
          (insert "</TERM>\n")
          (insert "<TERM>")
          (insert-text (global-name global))
          (insert "</TERM>\n")
          (insert "<LISTITEM>\n")
          (loop for text in (global-description-list global)
                do 
                (insert "<PARA>\n")
                (insert-text text)
                (insert "</PARA>\n"))
          (insert "</LISTITEM>\n")
          (insert "</VARLISTENTRY>\n"))
    (insert "</VARIABLELIST>\n")))

(defun sgml-examples (protocol)
  (let ((example-list (protocol-example-list protocol)))
    (when example-list
      (insert "<EXAMPLE LABEL=\"")
      (insert (symbol-name (protocol-module protocol)))
      (insert "/")
      (insert (external-protocol-name protocol))
      (insert (format "/%d" (general-example-counter protocol)))
      (insert "\">")
      (insert "<TITLE>\n")
      (insert "</TITLE>\n")
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

(defun count-noninternal-protocols (protocol)
  (loop for included-protocol in (protocol-included-protocol-list protocol)
        count (not (internal-protocol-p included-protocol))))

(defun compare-method-signatures (method-a method-b)
  (let* ((method-a-signature (get-method-signature method-a))
         (method-b-signature (get-method-signature method-b)))
    (string< method-a-signature method-b-signature)))

(defun compare-methodinfo (a b)
  (let ((protocol-name-a (protocol-name (second a)))
        (protocol-name-b (protocol-name (second b))))
    (if (string= protocol-name-a protocol-name-b)
        (compare-method-signatures (third a) (third b))
        (string< protocol-name-a protocol-name-b))))

(defun sgml-method-examples (protocol method)
  (when (method-example-list method)
    (insert "<EXAMPLE LABEL=\"")
    (insert (symbol-name (protocol-module protocol)))
    (insert "/")
    (insert (external-protocol-name protocol))
    (insert "/")
    (print-method-signature method (current-buffer))
    (insert (format "/%d" (method-example-counter protocol method)))
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
  (unless (zerop (count-included-methodinfo-entries protocol phase))
    (insert "<REFSECT2>\n")
    (insert "<TITLE>Phase: ")
    (insert (capitalize (substring (prin1-to-string phase) 1)))
    (insert "</TITLE>\n")
    (sgml-method-definitions protocol phase)
    (insert "</REFSECT2>\n")))

(defun sgml-refsect1-protocol-list (protocol &optional expand-flag)
  (insert "<REFSECT1>\n")
  (insert "<TITLE>Protocols adopted by ")
  (insert (protocol-name protocol))
  (insert "</TITLE>\n")
  (if (zerop (count-noninternal-protocols protocol))
      (insert "<PARA>None</PARA>\n")
      (flet ((print-expanded-protocol-list (protocol)
               (insert "<ITEMIZEDLIST>\n")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert "<LISTITEM>\n")
                       (insert "<PARA>")
                       (sgml-link-to-protocol included-protocol)
                       (insert "</PARA>\n")
                       (print-expanded-protocol-list included-protocol)
                       (insert "</LISTITEM>\n")))
               (insert "</ITEMIZEDLIST>\n"))
             (print-unexpanded-protocol-list (protocol)
               (insert "<PARA>")
               (loop for included-protocol in
                     (protocol-included-protocol-list protocol)
                     do
                     (unless (internal-protocol-p protocol)
                       (insert " ")
                       (sgml-link-to-protocol included-protocol)))
               (insert "</PARA>\n")))
        (if expand-flag
            (print-expanded-protocol-list protocol)
            (print-unexpanded-protocol-list protocol))))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-method-list (protocol)
  (insert "<REFSECT1><TITLE>Methods</TITLE>\n")
  (if (zerop (count-included-methodinfo-entries-for-all-phases protocol))
      (insert "<PARA>None</PARA>\n")
      (loop for phase in *phases*
            do
            (sgml-methods-for-phase protocol phase)))
  (insert "</REFSECT1>\n"))

(defun sgml-refsect1-examples (protocol)
  (when (protocol-example-list protocol)
    (insert "<REFSECT1><TITLE>Examples</TITLE>\n")
    (sgml-examples protocol)
    (insert "</REFSECT1>\n")))

(defun internal-protocol-p (protocol)
  (string= (substring (protocol-name protocol) 0 1) "_"))

(defun generate-refentry-for-protocol (protocol)
  (unless (internal-protocol-p protocol)
    (sgml-refentry-start protocol)
    (sgml-refmeta protocol)
    (sgml-namediv protocol)
    (sgml-refsect1-description protocol)
    (sgml-refsect1-protocol-list protocol)
    (sgml-refsect1-method-list protocol)
    (sgml-refsect1-macro-list (protocol-macro-list protocol))
    (sgml-refsect1-function-list (protocol-function-list protocol))
    (sgml-refsect1-typedef-list (protocol-typedef-list protocol))
    (sgml-refsect1-global-list (protocol-global-list protocol))
    (sgml-refsect1-examples protocol)
    (insert "</REFENTRY>\n")))

(defun sgml-generate-refentries-for-module (module)
  (loop for protocol in (sort 
                         (gethash module *module-hash-table*)
                         (lambda (protocol-a protocol-b)
                           (string< (protocol-name protocol-a)
                                    (protocol-name protocol-b))))
        do
        (generate-refentry-for-protocol protocol)))

(defun sgml-create-refentries-for-module (module)
  (let ((module-name (symbol-name module)))
    (with-temp-file (pathname-for-swarmdocs-pages-output module)
      (sgml-generate-refentries-for-module module))))

(defun sgml-create-refentries-for-all-modules ()
  (interactive)
  (loop for module being each hash-key of *module-hash-table*
        do
        (sgml-create-refentries-for-module module)))

(defun build-method-signature-hash-table ()
  (loop for protocol being each hash-value of *protocol-hash-table*
        do
        (loop for method in (protocol-method-list protocol)
              do
              (push (cons protocol method)
                    (gethash (get-method-signature method)
                             *method-signature-hash-table*)))))

(defun build-protocol-vector ()
  (setq *protocol-list*
        (sort
         (loop for protocol being each hash-value of *protocol-hash-table*
               collect protocol)
         #'(lambda (protocol-a protocol-b)
             (string< (protocol-name protocol-a)
                      (protocol-name protocol-b))))))

(defun build-method-signature-vector ()
  (setq *method-signature-list*
        (sort
         (loop for method-signature being each hash-key of
               *method-signature-hash-table*
               collect method-signature)
         #'string<)))

(defun sgml-protocol-indexentry (protocol)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (insert (sgml-protocol-id protocol))
  (insert "\">")
  (insert (external-protocol-name protocol))
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-protocol-index ()
  (insert "<INDEX ID=\"PROTOCOL.INDEX\">\n")
  (insert "<TITLE>Protocol Index</TITLE>\n")
  (loop for protocol in *protocol-list*
        unless (internal-protocol-p protocol)
        do (sgml-protocol-indexentry protocol))
  (insert "</INDEX>\n"))

(defun sgml-method-signature-indexentry (method-signature)
  (insert "<INDEXENTRY>\n")
  (insert "<PRIMARYIE LINKENDS=\"")
  (let ((protocol.method-list (gethash method-signature
                                       *method-signature-hash-table*))
        (space ""))
    (loop for protocol.method in protocol.method-list
          do
          (insert space)
          (insert (sgml-method-signature-id
                   (car protocol.method)
                   (method-phase (cdr protocol.method))
                   method-signature))
          (setq space " ")))
  (insert "\">")
  (insert method-signature)
  (insert "</PRIMARYIE>\n")
  (insert "</INDEXENTRY>\n"))

(defun sgml-generate-method-signature-index ()
  (insert "<INDEX ID=\"METHOD.INDEX\">\n")
  (insert "<TITLE>Method Index</TITLE>\n")
  (loop for method-signature in *method-signature-list*
        do (sgml-method-signature-indexentry method-signature))
  (insert "</INDEX>\n"))

(defun sgml-generate-indices ()
  (with-temp-file (concat (get-swarmdocs-build-area) "src/refindex.sgml")
    (sgml-generate-protocol-index)
    (sgml-generate-method-signature-index)))

(defun load-and-process-modules ()
  (interactive)
  (load-all-modules)
  (generate-expanded-methodinfo-lists)
  (build-method-signature-hash-table)
  (build-protocol-vector)
  (build-method-signature-vector))

(defun run-all ()
  (interactive)
  (load-and-process-modules)
  (sgml-create-refentries-for-all-modules)
  (sgml-generate-indices)
  nil)
