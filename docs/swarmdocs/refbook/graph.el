(require 'cl) ; push
(eval-and-compile
 (push (getenv "BUILDDIR") load-path))
(require 'protocol)  

(defun vcg-graph-all-protocols ()
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-values module-items)
        for module-name = (concat "M:" (symbol-name module-sym))
        do
        (insert "graph: {\nfolding: 1 color: blue title: \"")
        (insert (capitalize module-name))
        (insert "\"\n")
        (insert "node: { title: \"")
        (insert module-name)
        (insert "\" }\n")
        (loop for module-item in module-items do
              (cond ((and (protocol-p module-item)
                          (not (internal-protocol-p module-item)))
                     (let ((protocol-name (protocol-name module-item)))
                       (insert "node: { title: \"")
                       (insert protocol-name)
                       (insert "\" shape: ellipse color: red }\n")
                       (insert "edge: { targetname: \"")
                       (insert protocol-name)
                       (insert "\" sourcename: \"")
                       (insert module-name)
                       (insert "\" }\n"))
                     (loop for included-protocol in
                           (protocol-included-protocol-list module-item)
                           unless (internal-protocol-p included-protocol)
                           do
                           (insert "backedge: { targetname: \"")
                           (insert (protocol-name module-item))
                           (insert "\" sourcename: \"")
                           (insert (protocol-name included-protocol))
                           (insert "\" }\n")))))
        (insert "}\n")))

(defun vcg-output-protocols-graph ()
  (interactive)
  (with-temp-file "protocols.vcg"
    (insert "graph: {\n")
    (insert "orientation: left_to_right\n")
    (vcg-graph-all-protocols)
    (insert "}\n")))

(defun dot-graph-all-protocols ()
  (loop for module-sym being each hash-key of *module-hash-table*
        using (hash-values module-items)
        for module-name = (symbol-name module-sym)
        do
        (loop for module-item in module-items do
              (cond ((and (protocol-p module-item)
                          (not (internal-protocol-p module-item)))
                     (loop for included-protocol in
                           (protocol-included-protocol-list module-item)
                           unless (internal-protocol-p included-protocol)
                           do
                           (insert "\"")
                           (insert (protocol-name module-item))
                           (insert "\" -> \"")
                           (insert (protocol-name included-protocol))
                           (insert "\"\n")))))
        (insert "subgraph cluster_")
        (insert module-name)
        (insert " { label=\"")
        (insert (capitalize module-name))
        (insert "\"\n")
        (loop for module-item in module-items do
              (cond ((and (protocol-p module-item)
                          (not (internal-protocol-p module-item)))
                     (insert "\"")
                     (insert (protocol-name module-item))
                     (insert "\"; "))))
        (insert "}\n")))

(defun dot-output-protocols-graph ()
  (interactive)
  (with-temp-file "protocols.dot"
    (insert "digraph \"Swarm Protocols\" {\n");
    (insert "page=\"10,7.5\"\n")
    (insert "ratio=auto\n")
    (dot-graph-all-protocols)
    (insert "}\n")))

(defun dot-graph-module (edge-hash-table module-sym &optional module-items)
  (unless module-items 
    (setq module-items (gethash module-sym *module-hash-table*)))
  (let ((module-name (symbol-name module-sym))
        (included-module-hash-table (make-hash-table))
        local-protocols)
    (loop for module-item in module-items do
          (cond ((and (protocol-p module-item)
                      (not (internal-protocol-p module-item)))
                 (loop for included-protocol in
                       (protocol-included-protocol-list module-item)
                       for included-module-sym = 
                       (module-sym (protocol-module included-protocol))
                       unless (internal-protocol-p included-protocol)
                       do
                       (let ((edge-key (cons module-item included-protocol)))
                         (unless (gethash edge-key edge-hash-table)
                           (insert "\"")
                           (insert (protocol-name module-item))
                           (insert "\" -> \"")
                           (insert (protocol-name included-protocol))
                           (insert "\"\n")
                           (setf (gethash edge-key edge-hash-table) t)))
                       (if (eq included-module-sym module-sym)
                           (push included-protocol local-protocols)
                           (push included-protocol
                                 (gethash included-module-sym
                                          included-module-hash-table))
                           )))))
    (insert "subgraph cluster_")
    (insert module-name)
    (insert " { label=\"")
    (insert (capitalize module-name))
    (insert "\"\n")
    (loop for module-item in (append module-items local-protocols) do
          (cond ((and (protocol-p module-item)
                      (not (internal-protocol-p module-item)))
                 (insert "\"")
                 (insert (protocol-name module-item))
                 (insert "\"; "))))
    (insert "}\n")
    (loop for included-module-sym being each hash-key of
          included-module-hash-table
          do
          (dot-graph-module edge-hash-table
                            included-module-sym 
                            (gethash included-module-sym
                                     included-module-hash-table)))))

(defun dot-output-module-graph (module-sym &optional compress-flag)
  (let ((edge-hash-table (make-hash-table :test #'equal)))
    (with-temp-file (concat (symbol-name module-sym)
                            (if compress-flag "-singlepage" "")
                            ".dot")
      (insert "digraph ")
      (insert (capitalize (symbol-name module-sym)))
      (insert " {\n")
      (if compress-flag
          (progn
            (insert "size=\"10,7.5\"\n")
            (insert "ratio=compress\n")
            (insert "rotate=90\n"))
          (progn
            (insert "ratio=auto\n")
            (insert "page=\"7.5,10\"\n")))
      (dot-graph-module edge-hash-table module-sym)
      (insert "}\n"))))

(defun dot-output-each-module-graph (&optional compress-flag)
  (interactive "P")
  (loop for module-sym being each hash-key of *module-hash-table* do
        (dot-output-module-graph module-sym compress-flag)))

(defun create-graphs ()
  (load-and-process-modules :uniquify-method-lists nil)    
  (vcg-output-protocols-graph)
  (dot-output-protocols-graph)
  (dot-output-each-module-graph nil)
  (dot-output-each-module-graph t))
