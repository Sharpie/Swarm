// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Simple Graph Object -> encapsulates many of the low-level objects 
//                        required to get graphs/file traces to work.

#import <swarmobject.h>

@class EZGraph;
@class EZSequence;
@class EZAverageSequence;

@interface EZGraph : SwarmObject {
  int graphics ;
  id theGraph ;

  int fileOutput ;

  id sequenceList ;

  char * xLabel ;
  char * yLabel ;
  char *  title ;
  unsigned colorIdx;
}

-setGraphics: (int) state ;
-setFileOutput: (int) state ;

-setTitle: (char *) aTitle ;
-setAxisLabelsX: (char *) xl Y: (char *) yl ;

-createEnd ;

-getGraph ;

-createGraphSequence: (char *) aName 
	 forSequence: aSeq
	withFeedFrom: anObj 
	 andSelector: (SEL) aSel ;

-createSequence: (char *) aName  
   withFeedFrom:          anObj 
    andSelector: (SEL) aSel ;

-createAverageSequence: (char * ) aName 
          withFeedFrom: aCollection 
           andSelector: (SEL) aSel ;

-createTotalSequence: (char *) aName 
        withFeedFrom: aCollection 
         andSelector: (SEL) aSel ;

-createMinSequence: (char *) aName 
      withFeedFrom: aCollection 
       andSelector: (SEL) aSel ;

-createMaxSequence: (char *) aName 
      withFeedFrom: aCollection 
       andSelector: (SEL) aSel ;

-createCountSequence: (char *) aName 
        withFeedFrom: aCollection 
         andSelector: (SEL) aSel ;

-step ;

@end

@interface EZSequence : SwarmObject {
  id theActiveOutFile ;
  id theActiveGrapher ;
}

-setActiveOutFile: anActiveOutFile ;
-setActiveGrapher: aGrapher ;

-step ;

@end

@interface EZAverageSequence : EZSequence {
  id theAverager ;
}

-setAverager: anAverager ;

@end

