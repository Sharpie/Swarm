// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <analysis/EZGraph.h>
#import <analysis.h> // ActiveGraph, ActiveOutFile
#import <gui.h> // Graph
#import <simtools.h> // OutFile
#import <defobj/defalloc.h> // getZone
#include <misc.h> // strlen, stpcpy

#define NUMCOLORS 12

static const char * defaultGraphColors[NUMCOLORS] = {
  "blue", "orange", "yellow", "green",
  "red", "purple", "violet", "cyan",
  "grey50", "darkgreen", "goldenrod", "seagreen"
};

@implementation EZGraph

PHASE(Creating)

+ create: aZone setTitle: (const char *)aTitle setAxisLabelsX: (const char *)xl Y: (const char *)yl setWindowGeometryRecordName: (const char *)windowGeometryRecordName
{
  EZGraph *obj = [super createBegin: aZone];

  obj->graphics = 1;
  obj->fileOutput = 0;
  obj->title = aTitle;
  obj->fileName = NULL;
  obj->xLabel = xl;
  obj->yLabel = yl;
  obj->graphColors = defaultGraphColors;
  obj->colorCount = NUMCOLORS;
  obj->colorIdx = 0;
  [obj setWindowGeometryRecordName: windowGeometryRecordName];

  return [obj createEnd];
}

+ create: aZone setFileOutput: (BOOL)fileOutputFlag
{
    EZGraph *obj = [super createBegin: aZone];    
    obj->graphics = 0;
    obj->fileOutput = 1;
    obj->title = NULL;
    obj->fileName = NULL;
    obj->xLabel = NULL;
    obj->yLabel = NULL;
    obj->graphColors = defaultGraphColors;
    obj->colorCount = NUMCOLORS;
    obj->colorIdx = 0;

    return [obj createEnd];
}


+ create: aZone setFileName: (const char *)aFileName
{
    EZGraph *obj = [super createBegin: aZone];    
    obj->graphics = 0;
    obj->fileOutput = 1;
    obj->title = NULL;
    obj->fileName = ZSTRDUP (aZone, aFileName);
    obj->xLabel = NULL;
    obj->yLabel = NULL;
    obj->graphColors = defaultGraphColors;
    obj->colorCount = NUMCOLORS;
    obj->colorIdx = 0;

    return [obj createEnd];
}

+ createBegin: aZone
{
  EZGraph *obj = [super createBegin: aZone];

  obj->graphics = 1;
  obj->fileOutput = 0;
  obj->title = NULL;
  obj->fileName = NULL;
  obj->xLabel = NULL;
  obj->yLabel = NULL;
  obj->graphColors = defaultGraphColors;
  obj->colorCount = NUMCOLORS;
  obj->colorIdx = 0;

  return obj;
}

- setColors: (const char * const *)colors count: (unsigned)nc
{
  colorCount = nc;
  graphColors = colors;

  return self;
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
  title = STRDUP (aTitle);

  return self;
}

- setFileName: (const char *)aFileName
// In case of file output, this file name is 
// prepended to the name of each data sequence
{
  fileName = STRDUP (aFileName);

  return self;
}

- setAxisLabelsX: (const char *)xl Y:(const char *)yl
{ 
  xLabel = STRDUP (xl);
  yLabel = STRDUP (yl);

  return self;
}

- createEnd
{

  if (graphics)
    {
      graph = [Graph createBegin: getZone (self)];
      [graph setSaveSizeFlag: saveSizeFlag];
      SET_COMPONENT_WINDOW_GEOMETRY_RECORD_NAME (graph);
      graph = [graph createEnd];
      [graph setTitle: title];
      [graph setAxisLabelsX: xLabel Y: yLabel];
      [graph pack];
    }
  
  sequenceList = [List create: getZone (self)];

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

static const char *
sequence_graph_filename (id aZone, const char *fileName, const char *aName)
{ 
  if (fileName == NULL)
    {
      char *buf = [aZone alloc: strlen (aName) + 1], *p;
      p = stpcpy (buf, aName);            
      return (buf);
    }
  else
    {      
      const char *delim = ".";
      char *buf =
        [aZone alloc: strlen (fileName) + strlen (delim) + strlen (aName) + 1];
      char *p = buf;
      
      p = stpcpy (buf, fileName);
      p = stpcpy (p, delim);
      p = stpcpy (p, aName);  
      return (buf);
    }
}

// internal method called by createSequence:withFeedFrom:andSelector
- createGraphSequence: (const char *)aName
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel
{
  id aGrapher;
  id aZone = getZone (self);
  
  if (graphics)
    {
      id anElement;
      
      anElement = [graph createElement];
      [anElement setLabel: aName];
      [anElement setColor: graphColors[colorIdx % colorCount]];
      colorIdx++;

      aGrapher = [ActiveGraph createBegin: aZone];
      [aGrapher setElement: anElement];
      [aGrapher setDataFeed: anObj]; 
      [aGrapher setProbedSelector: aSel];
      aGrapher = [aGrapher createEnd];
      
      [aSeq setActiveGrapher: aGrapher];    
    }
 
  if (fileOutput)
    {
      id aFileObj;
      const char *fName;

      fName = sequence_graph_filename (aZone, fileName, aName);
      aFileObj = [OutFile create: aZone setName: fName];
      
      aGrapher = [ActiveOutFile createBegin: aZone];
      [aGrapher setFileObject: aFileObj];
      [aGrapher setDataFeed: anObj]; 
      [aGrapher setProbedSelector: aSel];
      aGrapher = [aGrapher createEnd];
      
      [aSeq setActiveOutFile: aGrapher];    
    }
  
  [sequenceList addLast: aSeq];
  
  return self;
}

- (id <EZSequence>)createSequence: (const char *)aName
                     withFeedFrom: anObj 
                      andSelector: (SEL)aSel
{
  id aSeq;
  
  aSeq = [EZSequence create: getZone (self)];

  [self createGraphSequence: aName
        forSequence: aSeq
	withFeedFrom: anObj
        andSelector: aSel];
  
  return aSeq;
}

- (id <EZAverageSequence>)_createAveragerSequence_: (const char *)aName 
                                      withFeedFrom: aCollection 
                                     probeSelector: (SEL)probeSel
                                     graphSelector: (SEL)graphSel
                                             width: (unsigned)width
{
  id aSeq;
  id anAverager;
  
  aSeq = [EZAverageSequence create: getZone (self)];
  
  anAverager = [Averager createBegin: getZone (self)];
  [anAverager setCollection: aCollection];
  [anAverager setWidth: width];
  [anAverager setProbedSelector: probeSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager];

  [self createGraphSequence: aName
        forSequence: aSeq
	withFeedFrom: anAverager 
	andSelector: graphSel];

  return aSeq;
}

- (id <EZAverageSequence>)createAverageSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getAverage)
               width: 0];
}

- (id <EZAverageSequence>)createMovingAverageSequence: (const char *)aName 
           withFeedFrom: aTarget
            andSelector: (SEL)aSel
               andWidth: (unsigned)width
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aTarget
               probeSelector: aSel
               graphSelector: M(getMovingAverage)
               width: width];
}

- (id <EZAverageSequence>)createVarianceSequence: (const char *)aName 
                                    withFeedFrom: aCollection 
                                     andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getVariance)
               width: 0];
}

- (id <EZAverageSequence>)createMovingVarianceSequence: (const char *)aName 
                                          withFeedFrom: aTarget
                                           andSelector: (SEL)aSel
                                              andWidth: (unsigned)width
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aTarget
               probeSelector: aSel
               graphSelector: M(getMovingVariance)
               width: width];
}

- (id <EZAverageSequence>)createStdDevSequence: (const char *)aName 
                                  withFeedFrom: aCollection 
                                   andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getStdDev)
               width: 0];
}

- (id <EZAverageSequence>)createMovingStdDevSequence: (const char *)aName 
                                        withFeedFrom: aCollection 
                                         andSelector: (SEL)aSel
                                            andWidth: (unsigned)width
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getMovingStdDev)
               width: width];
}

- (id <EZAverageSequence>)createTotalSequence: (const char *)aName
                                 withFeedFrom: aCollection 
                                  andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getTotal)
               width: 0];
}

- (id <EZAverageSequence>)createMinSequence: (const char *)aName 
                               withFeedFrom: aCollection 
                                andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getMin)
               width: 0];
}

- (id <EZAverageSequence>)createMaxSequence: (const char *)aName
                               withFeedFrom: aCollection 
                                andSelector: (SEL)aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getMax)
               width: 0];
}

- (id <EZAverageSequence>)createCountSequence: (const char *)aName
                                 withFeedFrom: aCollection 
                                  andSelector: (SEL) aSel
{
  return [self _createAveragerSequence_: aName
               withFeedFrom: aCollection
               probeSelector: aSel
               graphSelector: M(getCount)
               width: 0];
}

- (void)step
{
  [sequenceList forEach: M(step)];
}

- (void)update
{
  [sequenceList forEach: M(update)];
}

- (void)outputGraph
{
  [sequenceList forEach: M(outputGraph)];
}

- (void)outputToFile
{
  [sequenceList forEach: M(outputToFile)];
}

- (void)drop
{
  id index, aSequence;
  
  if (fileName)
    FREEBLOCK (fileName);

  if (graphics)
    [graph drop];

  index = [sequenceList begin: getZone (self)];
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

PHASE(Creating)
PHASE(Setting)
PHASE(Using)

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

- setUnsignedArg: (unsigned)val
{
  [activeGrapher setArg: 0 ToUnsigned: val];
  return self;
}

- (void)step
{
  if (activeGrapher)
    [activeGrapher step];
  
  if (activeOutFile)
    [activeOutFile step];
}

- (void)update
{
  // No update to be done 
  // when there's only a single object connected
}

- (void)outputGraph
{
  if (activeGrapher)
    [activeGrapher step];
}

- (void)outputToFile
{
  if (activeOutFile)
    [activeOutFile step];
}

- (void)drop
{
  if (activeGrapher)
    [activeGrapher drop];
  if (activeOutFile)
    [activeOutFile drop];
  [super drop];
}

@end 


@implementation EZAverageSequence

PHASE(Creating)
PHASE(Setting)
PHASE(Using)

- setAverager: anAverager
{
  averager = anAverager;

  return self;
}

- setUnsignedArg: (unsigned)val
{
  [averager setArg: 0 ToUnsigned: val];
  return self;
}

- (void)step
{
  [averager update];
  [super step];
}

- (void)update
{
  [averager update];
  [super update];
}

- (void)drop
{
  [averager drop];
  [super drop];
}

@end 
