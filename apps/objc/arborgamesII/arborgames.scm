(list
  (cons 'batchSwarm
     (make-instance 'ForestBatchSwarm 
	#:metricFrequency 1 
	#:forestFrequency 100 
	#:experimentDuration 1000 
	#:speciesNumber 0 
      ))
  (cons 'modelSwarm
     (make-instance 'ForestModelSwarm 
	#:speciesNumber 8 
	#:worldSize 100 
	#:freqLStrikes 5 
   )))

