// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <analysis/EZGraph.h>
#import <analysis.h> // ActiveGraph, ActiveOutFile
#import <gui.h> // Graph
#import <simtools.h> // OutFile
#import <defobj/defalloc.h> // getZone
#include <misc.h> // strlen, stpcpy

#import <analysis/ActiveOutFile.h>

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

  obj->graphics = YES;
  obj->fileOutput = NO;
  obj->title = aTitle;
  obj->baseName = NULL;
  obj->xLabel = xl;
  obj->yLabel = yl;
  obj->graphColors = defaultGraphColors;
  obj->colorCount = NUMCOLORS;
  obj->colorIdx = 0;
  [obj setWindowGeometryRecordName: windowGeometryRecordName];

  return [obj createEnd];
}

+ create: aZone setTitle: (const char *)aTitle setAxisLabelsX: (const char *)xl Y: (const char *)yl setWindowGeometryRecordName: (const char *)windowGeometryRecordName setSaveSizeFlag: (BOOL)theSaveSizeFlag
{
  EZGraph *obj = [super createBegin: aZone];

  obj->graphics = YES;
  obj->fileOutput = NO;
  obj->title = aTitle;
  obj->baseName = NULL;
  obj->xLabel = xl;
  obj->yLabel = yl;
  obj->graphColors = defaultGraphColors;
  obj->colorCount = NUMCOLORS;
  obj->colorIdx = 0;
  [obj setWindowGeometryRecordName: windowGeometryRecordName];
  obj->saveSizeFlag = theSaveSizeFlag;

  return [obj createEnd];
}

+ create: aZone setFileOutput: (BOOL)fileOutputFlag
{
    EZGraph *obj = [super createBegin: aZone];    
    obj->graphics = NO;
    obj->fileOutput = YES;
    obj->title = NULL;
    obj->baseName = NULL;
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
    obj->graphics = NO;
    obj->fileOutput = YES;
    obj->title = NULL;
    obj->baseName = ZSTRDUP (aZone, aFileName);
    obj->xLabel = NULL;
    obj->yLabel = NULL;
    obj->graphColors = defaultGraphColors;
    obj->colorCount = NUMCOLORS;
    obj->colorIdx = 0;

    return [obj createEnd];
}

+ create: aZone setHDF5Container: (id <HDF5>)hdf5Obj
                       setPrefix: (const char *)keyPrefix
{
    EZGraph *obj = [super createBegin: aZone];    
    obj->graphics = NO;
    obj->fileOutput = NO;
    obj->hdf5Container = hdf5Obj;
    obj->title = NULL;
    obj->baseName = ZSTRDUP (aZone, keyPrefix);
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

  obj->graphics = YES;
  obj->fileOutput = NO;
  obj->title = NULL;
  obj->baseName = NULL;
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

- setHDF5Container: (id <HDF5>)hdf5Obj
{
  hdf5Container = hdf5Obj;
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
  baseName = STRDUP (aFileName);

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

- (void)setScaleModeX: (BOOL)xs Y: (BOOL)ys
{
  [graph setScaleModeX: xs Y: ys];
}

- (void)setRangesXMin: (double)xmin Max: (double)xmax
{
  [graph setRangesXMin: xmin Max: xmax];
}

- (void)setRangesYMin: (double)ymin Max: (double)ymax
{
  [graph setRangesYMin: ymin Max: ymax];
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
  return baseName;
}

static const char *
sequence_graph_filename (id aZone, const char *baseName, const char *aName)
{ 
  if (baseName == NULL)
    {
      char *buf = [aZone alloc: strlen (aName) + 1], *p;
      p = stpcpy (buf, aName);            
      return (buf);
    }
  else
    {      
      const char *delim = ".";
      char *buf =
        [aZone alloc: strlen (baseName) + strlen (delim) + strlen (aName) + 1];
      char *p = buf;
      
      p = stpcpy (buf, baseName);
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

  if (hdf5Container)
    {
      id hdf5Dataset;

      hdf5Group = [[[[[HDF5 createBegin: aZone]
                       setParent: hdf5Container]
                      setName: baseName]
                     setWriteFlag: YES]
                    createEnd];
      hdf5Dataset = [[[[[[[HDF5 createBegin: aZone]
                           setParent: hdf5Group]
                          setName: aName]
                         setWriteFlag: YES]
                        setDatasetFlag: YES]
                       setExtensibleDoubleVector]
                      createEnd];

      aGrapher = [ActiveOutFile createBegin: aZone];
      [aGrapher setHDF5Dataset: hdf5Dataset];
      [aGrapher setDataFeed: anObj];
      [aGrapher setProbedSelector: aSel];
      aGrapher = [aGrapher createEnd];

      [aSeq setActiveOutFile: aGrapher];
    }
  else if (fileOutput)
    {
      id aFileObj;
      const char *fName;

      fName = sequence_graph_filename (aZone, baseName, aName);
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
  
  if (baseName)
    FREEBLOCK (baseName);

  if (graphics)
    [graph drop];

  index = [sequenceList begin: getZone (self)];
  while ((aSequence = [index next]))
    {
      [index remove];
      [aSequence drop];
    }
  [index drop];

  if (hdf5Group)
    [hdf5Group drop];

  [super drop];
}

@end


@implementation EZSequence

PHASE(Creating)
PHASE(Setting)
PHASE(Using)

- (void)setActiveOutFile: (id <ActiveOutFile>)anActiveOutFile
{
  activeOutFile = (ActiveOutFile *)anActiveOutFile;
}

- (void)setActiveGrapher: (id <ActiveGraph>)aGrapher
{
  activeGrapher = aGrapher;
}

- (void)setUnsignedArg: (unsigned)val
{
  if (activeGrapher)
    [activeGrapher setArg: 0 ToUnsigned: val];
  if (activeOutFile)
    [activeOutFile setArg: 0 ToUnsigned: val];
}

- (void)step
{
  if (activeGrapher)
    [activeGrapher step];
  
  if (activeOutFile)
    [activeOutFile step];
}

- (double)getCurrentValue
{
  if (activeGrapher)
    return [activeGrapher getCurrentValue];
  if (activeOutFile)
    return [activeOutFile getCurrentValue];
  abort ();
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
    {
      if (activeOutFile->hdf5Dataset)
        [activeOutFile->hdf5Dataset drop];
      [activeOutFile drop];
    }
  [super drop];
}

@end 


@implementation EZAverageSequence

PHASE(Creating)
PHASE(Setting)
PHASE(Using)

- (void)setAverager: (id <Averager>)anAverager
{
  averager = anAverager;
}

- (id <Averager>)getAverager
{
  return averager;
}

- (void)setUnsignedArg: (unsigned)val
{
  [averager setArg: 0 ToUnsigned: val];
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
