// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZGraph.h>
#import <gui.h> // Graph
#import <simtools.h> // OutFile
#import <analysis.h> // ActiveGraph, ActiveOutFile

#define NUMCOLORS 12
const char graphColors[NUMCOLORS][16] =
//    { "Red",     "Green",  "Yellow",    "Pink",      "SeaGreen",
//      "Magenta", "Purple", "DarkGreen", "Goldenrod", "Black" };
        { "Red",   "Blue",   "Orange", "DarkGreen", "Magenta",   "Purple",
        "Green", "Yellow", "Cyan",   "SeaGreen",  "Goldenrod", "Black" };  

@implementation EZGraph

PHASE(Creating)

+ createBegin: aZone
{
  EZGraph *obj = [super createBegin: aZone];

  obj->graphics = 1;
  obj->fileOutput = 0;
  obj->title = NULL;
  obj->fileName = NULL;
  obj->xLabel = NULL;
  obj->yLabel = NULL;

  return obj;
}

- setGraphics: (BOOL)state
{
  graphics = state;

  return self;
}

- setFileOutput: (BOOL)state
{
  fileOutput = state;

  return self;
}

- setTitle: (const char *)aTitle
{
  title = aTitle;

  return self;
}

- setFileName: (const char *)aFileName
// In case of file output, this file name is 
// prepended to the name of each data sequence
{
  fileName = aFileName;

  return self;
}

- setAxisLabelsX: (const char *)xl Y:(const char *)yl
{ 
  xLabel = xl;
  yLabel = yl;

  return self;
}

- createEnd
{

  if (graphics)
    {
      graph = [Graph createBegin: [self getZone]];
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (graph);
      graph = [graph createEnd];
      [graph setTitle: title];
      [graph setAxisLabelsX: xLabel Y: yLabel];
      [graph pack];
    }
  
  sequenceList = [List create: [self getZone]];
  
  return self;
}

PHASE(Using)

- setScaleModeX: (BOOL)xs Y: (BOOL)ys
{
  [graph setScaleModeX: xs Y: ys];

  return self;
}

- setRangesXMin: (double)xmin Max: (double)xmax
{
  [graph setRangesXMin: xmin Max: xmax];

  return self;
}

- setRangesYMin: (double)ymin Max: (double)ymax
{
  [graph setRangesYMin: ymin Max: ymax];

  return self;
}

- (id <Graph>)getGraph
{
  return graph;
}

- dropSequence: aSeq
{
  if ([sequenceList contains: aSeq])
  {
    [sequenceList remove: aSeq];
    [aSeq drop];
    return aSeq;
  } else {
    // Say nothing, do nothing, but return nil as signal
    return nil;
  }
}

- (const char *)getTitle
{
  return title;
}

- (const char *)getFileName
{
  return fileName;
}

// internal method called by createSequence:withFeedFrom:andSelector
- createGraphSequence: (const char *)aName
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel
{
  id aGrapher;
  char fName[128];
  
  if (graphics)
    {
      id anElement;
      
      anElement = [graph createElement];
      [anElement setLabel: aName];
      [anElement setColor: graphColors[colorIdx++ % NUMCOLORS]];

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
      char *p;

      p = stpcpy (fName, fileName);
      p = stpcpy (p, ".");
      p = stpcpy (p, aName);      
      aFileObj = [OutFile create: [self getZone] withName: fName];
      
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
     andSelector: (SEL)aSel
{
  id aSeq;
  
  aSeq = [EZSequence create: [self getZone]];

  [self createGraphSequence: aName forSequence: aSeq
	withFeedFrom: anObj andSelector: aSel];
  
  return aSeq;
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

  return aSeq;
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

  return aSeq;
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

  return aSeq;

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

  return aSeq;

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
  
  return aSeq;

}

- step
{
  [sequenceList forEach: M(step)];

  return self;
}

- update
{

  [sequenceList forEach: M(update)];

  return self;
}

- outputGraph
{

  [sequenceList forEach: M(outputGraph)];

  return self;
}

- outputToFile
{

  [sequenceList forEach: M(outputToFile)];

  return self;
}

- (void)drop
{
  id index, aSequence;
  
  [graph drop];

  index = [sequenceList begin: [self getZone]];
  while ((aSequence = [index next]))
    {
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
  activeOutFile = anActiveOutFile;

  return self;
}

- setActiveGrapher: aGrapher
{
  activeGrapher = aGrapher;

  return self;
}

- step
{
  if (activeGrapher)
    [activeGrapher step];
  
  if (activeOutFile)
    [activeOutFile step];
  
  return self;
}

- update
{
  // No update to be done 
  // when there's only a single object connected

  return self;
}

- outputGraph
{
  if (activeGrapher)
    [activeGrapher step];

  return self;
}

- outputToFile
{
  if (activeOutFile)
    [activeOutFile step];

  return self;
}

- (void)drop
{
  [activeGrapher drop];
  [activeOutFile drop];
  [super drop];
}

@end 


@implementation EZAverageSequence

- setAverager: anAverager
{
  averager = anAverager;

  return self;
}

- step
{
  [averager update];
  [super step];
  
  return self;
}

- update
{
  [averager update];
  return [super update];
}

- (void)drop
{
  [averager drop];
  [super drop];
}

@end 
