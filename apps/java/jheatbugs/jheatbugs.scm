(list
 (cons 'batchSwarm
       (make-instance 'HeatbugBatchSwarm
                      #:loggingFrequency 1
                      #:experimentDuration 49))
 (cons 'modelSwarm
       (make-instance 'HeatbugModelSwarm
                      #:numBugs 77
                      #:minIdealTemp 10000
                      #:maxIdealTemp 20000
                      #:minOutputHeat 10000
                      #:maxOutputHeat 20000
                      #:randomMoveProbability 0.6)))
