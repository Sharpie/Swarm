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

(define *swarm-environment*
    ((primitive-get-static <swarm.Globals> "env" <swarm.SwarmEnvironment>)))

(define *globalZone* :: <swarm.defobj.Zone> #!null)
(define *scratchZone* :: <swarm.defobj.Zone> #!null)
(define *Member* :: <swarm.defobj.Symbol> #!null)
(define *probeLibrary* :: <swarm.objectbase.ProbeLibrary> #!null)

(define (initSwarmBatch app-name version bug-address no-init-flag)
    (let ((ary ((primitive-array-new <String>) 2)))
      ((primitive-array-set <String>) ary 0 "-b")
      ((primitive-array-set <String>) ary 1 (if no-init-flag 
                                                "--no-init"
                                                ""))
      ((primitive-virtual-method
        <swarm.SwarmEnvironment>
        "initSwarm"
        <void>
        (<String>
         <String>
         <String>
         <java.lang.String[]>))
       *swarm-environment*
       app-name version bug-address ary))
    (set! *globalZone* (field *swarm-environment* "globalZone"))
    (set! *scratchZone* (field *swarm-environment* "scratchZone"))
    (set! *Member* (field *swarm-environment* "Member"))
    (set! *probeLibrary* (field *swarm-environment* "probeLibrary")))
