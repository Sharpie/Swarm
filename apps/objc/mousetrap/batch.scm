(list
 (cons 'batchSwarm
       (make-instance 'MousetrapBatchSwarm
                      #:loggingFrequency 1))
 (cons 'modelSwarm
       (make-instance 'MousetrapModelSwarm
                      #:gridSize 40
                      #:triggerLikelihood 1.0
                      #:numberOutputTriggers 4
                      #:maxTriggerDistance 4
                      #:maxTriggerTime 16
                      #:trapDensity 1.0)))



