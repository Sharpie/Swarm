// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Simple Graph Object -> encapsulates many of the low-level objects 
//                        required to get graphs/file traces to work.

#import <simtoolsgui/GUIComposite.h>

@class EZGraph;
@class EZSequence;
@class EZAverageSequence;

@interface EZGraph: GUIComposite
{
  int graphics;
  id theGraph;

  int fileOutput;

  id sequenceList;

  const char *xLabel;
  const char *yLabel;
  const char *title;
  unsigned colorIdx;
}

- setGraphics: (BOOL)state;
- setFileOutput: (BOOL)state;

- setTitle: (const char *)aTitle;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- createEnd;

- setRangesXMin: (double)xmin Max: (double)xmax;
- setRangesYMin: (double)ymin Max: (double)ymax;
- setScaleModeX: (BOOL)xs Y: (BOOL)ys;

- getGraph;

- createGraphSequence: (const char *)aName 
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel;

- createSequence: (const char *)aName
    withFeedFrom: anObj 
     andSelector: (SEL) aSel;

- createAverageSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- createTotalSequence: (const char *)aName 
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- createMinSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- createMaxSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- createCountSequence: (const char *)aName
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- step;

@end

@interface EZSequence : SwarmObject
{
  id theActiveOutFile;
  id theActiveGrapher;
}

- setActiveOutFile: anActiveOutFile;
- setActiveGrapher: aGrapher;

- step;

@end

@interface EZAverageSequence : EZSequence
{
  id theAverager;
}

- setAverager: anAverager;

@end

