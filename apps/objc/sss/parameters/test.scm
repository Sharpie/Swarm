(list
 (cons 'model
       (make-instance 'ModelSwarm 
		      #:numAgents 250
		      #:alpha 1 
		      #:replacement 1 
		      #:maxVision 6 
		      #:maxMetabolism 4 
		      #:minInitialSugar 5 
		      #:maxInitialSugar 25 
		      #:deathAgeMin 60 
		      #:deathAgeMax 100 
		      #:worldXSize 50 
		      #:worldYSize 50 
		      #:datafile "./sugarspace.pgm"
		      )
       )
 )
