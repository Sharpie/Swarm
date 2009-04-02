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

// Simple Graph Object -> encapsulates many of the low-level objects 
//                        required to get graphs/file traces to work.

#import <Swarm/analysis.h> // EZGraph
#import <Swarm/gui.h>
#import <Swarm/GUIComposite.h>

@class EZGraph;
@class EZSequence;
@class EZAverageSequence;

@interface EZGraph: GUIComposite <EZGraph>
{
  int graphics;
  id <Graph> graph;

  BOOL fileOutput;
  id <HDF5> hdf5Container;
  id <HDF5> hdf5Group;

  id sequenceList;

  const char *xLabel;
  const char *yLabel;
  const char *title;
  const char *baseName;

  const char * const *graphColors;
  unsigned colorCount;

  unsigned colorIdx;
}
+ create: aZone setTitle: (const char *)aTitle setAxisLabelsX: (const char *)xl Y: (const char *)yl setWindowGeometryRecordName: (const char *)windowGeometryRecordName;
+ create: aZone setTitle: (const char *)aTitle setAxisLabelsX: (const char *)xl Y: (const char *)yl setWindowGeometryRecordName: (const char *)windowGeometryRecordName setSaveSizeFlag: (BOOL)saveSizeFlag;
+ create: aZone setFileOutput: (BOOL)fileOutputFlag;
+ create: aZone setHDF5Container: (id <HDF5>)hdf5Container setPrefix: (const char *)prefix;
+ create: aZone setFileName: (const char *)aFileName;

- setGraphics: (BOOL)state;
- setFileOutput: (BOOL)state;

- setTitle: (const char *)aTitle;
- setFileName: (const char *)aFileName;
- setAxisLabelsX: (const char *)xl Y: (const char *)yl;

- setColors: (const char * const *) colors count: (unsigned)nc;

- createEnd;

- (void)setRangesXMin: (double)xmin Max: (double)xmax;
- (void)setRangesYMin: (double)ymin Max: (double)ymax;
- (void)setScaleModeX: (BOOL)xs Y: (BOOL)ys;

- (id <Graph>) getGraph;

- createGraphSequence: (const char *)aName 
          forSequence: aSeq
         withFeedFrom: anObj 
          andSelector: (SEL)aSel;

- (id <EZSequence>)createSequence: (const char *)aName
    withFeedFrom: anObj 
     andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createAverageSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingAverageSequence: (const char *)aName 
            withFeedFrom: aCollection
            andSelector: (SEL)aSel
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createVarianceSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingVarianceSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel 
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createStdDevSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMovingStdDevSequence: (const char *)aName 
           withFeedFrom: aCollection 
            andSelector: (SEL) aSel 
               andWidth: (unsigned)width;

- (id <EZAverageSequence>)createTotalSequence: (const char *)aName 
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMinSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createMaxSequence: (const char *)aName
       withFeedFrom: aCollection 
        andSelector: (SEL) aSel;

- (id <EZAverageSequence>)createCountSequence: (const char *)aName
         withFeedFrom: aCollection 
          andSelector: (SEL) aSel;

- dropSequence: aSeq;

- (const char *)getTitle;
- (const char *)getFileName;

- (void)step;

- (void)update;
- (void)outputGraph;
- (void)outputToFile;

@end

@interface EZSequence : SwarmObject <EZSequence>
{
@public
 
  ActiveOutFile *activeOutFile;
  id <ActiveGraph> activeGrapher;
}

- (void)setActiveOutFile: (id <ActiveOutFile>)anActiveOutFile;
- (void)setActiveGrapher: (id <ActiveGraph>)aGrapher;
- (void)setUnsignedArg: (unsigned)val;

- (void)step;

- (void)update;
- (void)outputGraph;
- (void)outputToFile;
- (double)getCurrentValue;
@end

@interface EZAverageSequence : EZSequence <EZAverageSequence>
{
  id <Averager> averager;
}

- (void)setAverager: (id <Averager>)anAverager;
- (id <Averager>)getAverager;
- (void)setUnsignedArg: (unsigned)val;

@end

