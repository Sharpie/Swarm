(list
 (
  cons 'displaySwarm
       (
	make-instance 'ObserverSwarm
		      #:displayFrequency 1
		      #:zoomFactor 4
		      )
       )
 (
  cons 'modelSwarm
       (
	make-instance 'ModelSwarm
		      #:worldXSize 80
		      #:worldYSize 80
		      #:seedProb 0.90
		      #:bugDensity 0.01
		      #:endTime 625
		      )
       )
 )
