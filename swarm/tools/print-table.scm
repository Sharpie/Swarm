;; Copyright © 2000 Swarm Development Group
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA
;; 
;; The Swarm Development Group can be reached via our website at:
;; http://www.swarm.org/

(define (init)
  (initSwarmBatch "print-table" "0.0" "bug-swarm@swarm.org" #t))

(define (print-record-using-probemap probeMap :: <swarm.objectbase.ProbeMap> obj)
  (let* ((index :: <swarm.collections.Index>
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

(define (print-record-for-key archiver :: <swarm.defobj.Archiver> key)
  (let ((string :: <java.lang.String> key))
    (let* ((list :: <swarm.collections.Collection>
                 (invoke archiver 'getObject string))
           (index :: <swarm.collections.Index>
                  (invoke list 'begin *scratchZone*))
           (probeLibrary :: <swarm.objectbase.ProbeLibrary> *probeLibrary*)
           (start-obj (invoke index 'next))
           (class (invoke start-obj 'getClass))
           (probeMap :: <swarm.objectbase.ProbeMap>
                     (invoke probeLibrary 'getCompleteVarMapFor class)))
      (let loop ((obj start-obj))
        (if (eq? (invoke index 'getLoc) *Member*)
            (begin
              (print-record-using-probemap probeMap obj)
              (newline)
              (loop (invoke index 'next))))))))

(define (print archiver-procedure)
  (let ((args (vector->list command-line-arguments)))
    (init)
    (print-record-for-key (archiver-procedure (car args))
                          (cadr args))))

(define (make-open-hdf5-archive-procedure)
  (lambda (path)
    (make <swarm.defobj.HDF5ArchiverImpl> *globalZone* path)))

