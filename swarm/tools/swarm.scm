(define *swarm-environment*
    ((primitive-get-static <swarm.Globals> "env" <swarm.SwarmEnvironment>)))

(define *globalZone* :: <swarm.defobj.Zone> #!null)
(define *scratchZone* :: <swarm.defobj.Zone> #!null)
(define *Member* :: <swarm.defobj.Symbol> #!null)
(define *probeLibrary* :: <swarm.objectbase.ProbeLibrary> #!null)

(define (initSwarmBatch app-name version bug-address)
    (let ((ary ((primitive-array-new <String>) 1)))
      ((primitive-array-set <String>) ary 0 "-b")
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
