(define (init app-name)
  (initSwarmBatch app-name "0.0" "bug-swarm@swarm.org"))

(define (print-record obj)
  (let* ((class (invoke obj 'getClass))
         (probeLibrary :: <swarm.objectbase.ProbeLibrary> *probeLibrary*)
         (probeMap :: <swarm.objectbase.ProbeMap>
                   (invoke probeLibrary 'getCompleteVarMapFor class))
         (index :: <swarm.collections.Index>
                (invoke probeMap 'begin *scratchZone*)))
    (let loop ((varprobe :: <swarm.objectbase.VarProbe> (invoke index 'next)))
      (if (eq? (invoke index 'getLoc) *Member*)
          (let ((name (invoke varprobe 'getProbedVariable)))
            (if (not (or (string=? name "isa")
                         (string=? name "zbits")))
                (let ((string :: <swarm.collections.String> 
                              (invoke varprobe 'probeAsString obj)))
                  (display " ")
                  (display (invoke string 'getC))
                  (let ((drop-string :: <swarm.defobj.Drop> string))
                    (invoke drop-string 'drop))))
            (loop (invoke index 'next)))))))

(define (print-record-for-key key archiver-sym)
  (let ((archiver :: <swarm.defobj.Archiver> 
                  (field *swarm-environment* archiver-sym))
        (string :: <java.lang.String> key))
    (let* ((list :: <swarm.collections.Collection>
                 (invoke archiver 'getObject string))
           (index :: <swarm.collections.Index>
                  (invoke list 'begin *scratchZone*)))
      (let loop ((obj (invoke index 'next)))
        (if (eq? (invoke index 'getLoc) *Member*)
            (begin
              (print-record obj)
              (newline)
              (loop (invoke index 'next))))))))

(define (print archiver-sym)
  (let ((args (vector->list command-line-arguments)))
    (init (car args))
    (print-record-for-key (cadr args) archiver-sym)))


