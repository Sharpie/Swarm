// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <simtools.h>
#import <analysis.h>

#define NUMCOLORS 10
const char graphColors[NUMCOLORS][16] = { "Red", "Green", "Yellow", 
                                          "Pink", "SeaGreen", "Magenta", 
                                          "Purple", "DarkGreen", "Goldenrod", 
                                          "Black" };

@implementation EZGraph

+ createBegin: aZone
{
  EZGraph * obj;

  obj = [super createBegin: aZone];
  obj->graphics = 1;
  obj->fileOutput = 0;
  obj->title = NULL;
  obj->xLabel = NULL;
  obj->yLabel = NULL;
  return obj;
}

- setGraphics: (int)state
{
  graphics = state;
  return self;
}

- setFileOutput: (int)state
{
  fileOutput = state;
  return self;
}

- setAxisLabelsX: (const char *)xl Y:(const char *)yl
{ 
  xLabel = xl;
  yLabel = yl;
  return self;
}

- setTitle: (const char *)aTitle
{
  title = aTitle;
  return self;
}

- createEnd
{

  if (graphics)
    {
      theGraph = [Graph createBegin: [self getZone]];
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (theGraph);
      theGraph = [theGraph createEnd];
      [theGraph setTitle: title];
      [theGraph setAxisLabelsX: xLabel Y: yLabel];
      [theGraph pack];
    }
  
  sequenceList = [List create: [self getZone]];
  
  return self;
}

- getGraph
{
  return theGraph;
}

- createGraphSequence: (const char *)aName
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel
{
  id aGrapher;
  
  if (graphics)
    {
      id anElement;
      
      anElement = [theGraph createElement];
      [anElement setLabel: aName];
      [anElement setColor: graphColors[ colorIdx++ % NUMCOLORS ] ];
      aGrapher = [ActiveGraph createBegin: [self getZone]];
      [aGrapher setElement: anElement];
      [aGrapher setDataFeed: anObj]; 
      [aGrapher setProbedSelector: aSel];
      aGrapher = [aGrapher createEnd];
      
      [aSeq setActiveGrapher: aGrapher];    
    }
 
  if (fileOutput)
    {
      id aFileObj;
      
      aFileObj = [OutFile create: [self getZone] withName: aName];
      
      aGrapher = [ActiveOutFile createBegin: [self getZone]];
      [aGrapher setFileObject: aFileObj];
      [aGrapher setDataFeed: anObj]; 
      [aGrapher setProbedSelector: aSel];
      aGrapher = [aGrapher createEnd];
      
      [aSeq setActiveOutFile: aGrapher];    
    }
  
  [sequenceList addLast: aSeq];
  
  return self;
}

- createSequence: (const char *)aName
    withFeedFrom: anObj 
     andSelector: (SEL) aSel
{
  id aSeq;
  
  aSeq = [EZSequence create: [self getZone]];

  [self createGraphSequence: aName forSequence: aSeq
	withFeedFrom: anObj andSelector: aSel];
  
  return self;
}

- createAverageSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL)aSel
{
  id aSeq;
  id anAverager;
  
  aSeq = [EZAverageSequence create: [self getZone]];
  
  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName forSequence: aSeq
	withFeedFrom: anAverager 
	andSelector: M(getAverage)];

  return self;
}

- createTotalSequence: (const char *)aName
         withFeedFrom: aCollection 
          andSelector: (SEL)aSel
{
  
  id aSeq;
  id anAverager;
  
  aSeq = [EZAverageSequence create: [self getZone]];
  
  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName forSequence: aSeq
	withFeedFrom: anAverager 
	andSelector: M(getTotal)];

  return self;
}

- createMinSequence: (const char *)aName 
       withFeedFrom: aCollection 
        andSelector: (SEL)aSel
{
  id aSeq;
  id anAverager;

  aSeq = [EZAverageSequence create: [self getZone]];

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName forSequence:aSeq
	withFeedFrom: anAverager 
	andSelector: M(getMin)];

  return self;

}

- createMaxSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL)aSel
{
  id aSeq;
  id anAverager;

  aSeq = [EZAverageSequence create: [self getZone]];

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName forSequence: aSeq
	withFeedFrom: anAverager 
	andSelector: M(getMax)];

  return self;

}

- createCountSequence: (const char *)aName
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel
{
  id aSeq;
  id anAverager;
  
  aSeq = [EZAverageSequence create: [self getZone]];
  
  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName
        forSequence: aSeq
	withFeedFrom: anAverager 
	andSelector: M(getCount)];
  
  return self;

}

- step
{
  [sequenceList forEach: M(step)];
  return self;
}

- (void)drop
{
  id index, aSequence;
  
  [theGraph drop];

  index = [sequenceList begin: [self getZone]];
  while( (aSequence = [index next]) ){
    [index remove];
    [aSequence drop];
  }
  [index drop];
  [super drop];
}

@end


@implementation EZSequence

- setActiveOutFile: anActiveOutFile
{
  theActiveOutFile = anActiveOutFile;
  return self;
}

- setActiveGrapher: aGrapher
{
  theActiveGrapher = aGrapher;
  return self;
}

- step
{
  if (theActiveGrapher)
    [theActiveGrapher step];
  
  if (theActiveOutFile)
    [theActiveOutFile step];
  
  return self;
}

- (void)drop
{
  [theActiveGrapher drop];
  [theActiveOutFile drop];
  [super drop];
}

@end 


@implementation EZAverageSequence

- setAverager: anAverager
{
  theAverager = anAverager;
  return self;
}

- step
{
  [theAverager update];
  [super step];
  
  return self;
}

- (void)drop
{
  [theAverager drop];
  [super drop];
}

@end 
