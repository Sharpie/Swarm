(list
 (cons 'model
       (make-instance 'ModelSwarm 
		      #:numAgents 400 
		      #:alpha 999999 
		      #:replacement 0 
		      #:maxVision 6 
		      #:maxMetabolism 4 
		      #:minInitialSugar 5 
		      #:maxInitialSugar 25 
		      #:deathAgeMin 9999997 
		      #:deathAgeMax 9999999 
		      #:worldXSize 50 
		      #:worldYSize 50 
		      #:datafile "./sugarspace.pgm"
		      )
       )
 )
