// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h>
#import <tkobjc.h>
#import <simtools.h>
#import <analysis.h>

@implementation EZGraph

+createBegin: aZone {
  EZGraph * obj ;

  obj = [super createBegin: aZone] ;
  obj->graphics = 1 ;
  obj->fileOutput = 0 ;
  obj->title = NULL ;
  obj->xLabel = NULL ;
  obj->yLabel = NULL ;
  return obj ;
}

-setGraphics: (int) state {
  graphics = state ;
  return self ;
}

-setFileOutput: (int) state {
  fileOutput = state ;
  return self ;
}

-setAxisLabelsX: (char *) xl Y: (char *) yl { 
  xLabel = xl ;
  yLabel = yl ;
  return self ;
}

-setTitle: (char *) aTitle {
  title = aTitle ;
  return self ;
}

-createEnd {

  if(graphics){
    theGraph = [BLTGraph create: [self getZone]];
    [theGraph title: title] ;
    [theGraph axisLabelsX: xLabel Y: yLabel];
    [theGraph pack];
  }

  sequenceList = [List create: [self getZone]] ;

  return self ;
}

-getGraph {
  return theGraph ;
}

-createSequence: (char *) aName 
   withFeedFrom: anObj 
    andSelector: (SEL) aSel {

  id aSeq ;
  id aGrapher ;

  aSeq = [EZSequence create: [self getZone]] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anObj]; 
    [aGrapher setProbedSelector: aSel];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anObj]; 
    [aGrapher setProbedSelector: aSel];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;
}

-createAverageSequence: (char *) aName 
          withFeedFrom: aCollection 
           andSelector: (SEL) aSel {
  id aSeq ;
  id anAverager ;
  id aGrapher ;

  aSeq = [EZAverageSequence create: [self getZone]] ;

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getAverage)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getAverage)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;
}

-createTotalSequence: (char *) aName 
        withFeedFrom: aCollection 
         andSelector: (SEL) aSel {

  id aSeq ;
  id anAverager ;
  id aGrapher ;

  aSeq = [EZAverageSequence create: [self getZone]] ;

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getTotal)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getTotal)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;

}

-createMinSequence: (char *) aName 
      withFeedFrom: aCollection 
       andSelector: (SEL) aSel {

  id aSeq ;
  id anAverager ;
  id aGrapher ;

  aSeq = [EZAverageSequence create: [self getZone]] ;

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getMin)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getMin)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;

}

-createMaxSequence: (char *) aName 
      withFeedFrom: aCollection 
       andSelector: (SEL) aSel {

  id aSeq ;
  id anAverager ;
  id aGrapher ;

  aSeq = [EZAverageSequence create: [self getZone]] ;

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getMax)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getMax)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;

}

-createCountSequence: (char *) aName 
        withFeedFrom: aCollection 
         andSelector: (SEL) aSel {

  id aSeq ;
  id anAverager ;
  id aGrapher ;

  aSeq = [EZAverageSequence create: [self getZone]] ;

  anAverager = [Averager createBegin: [self getZone]];
  [anAverager setCollection: aCollection];
  [anAverager setProbedSelector: aSel];
  anAverager = [anAverager createEnd];

  [aSeq setAverager: anAverager] ;

  if(graphics){
    id anElement ;

    anElement = [theGraph createElement] ;
    [anElement setLabel: aName] ;

    aGrapher = [ActiveGraph createBegin: [self getZone]];
    [aGrapher setElement: anElement];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getCount)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveGrapher: aGrapher] ;    
  }
 
  if(fileOutput){
    id aFileObj ;

    aFileObj = [OutFile create: [self getZone] withName: aName] ;

    aGrapher = [ActiveOutFile createBegin: [self getZone]];
    [aGrapher setFileObject: aFileObj];
    [aGrapher setDataFeed: anAverager]; 
    [aGrapher setProbedSelector: M(getCount)];
    aGrapher = [aGrapher createEnd];

    [aSeq setActiveOutFile: aGrapher] ;    
  }

  [sequenceList addLast: aSeq] ;

  return self ;

}

-step {
  [sequenceList forEach: M(step)] ;
  return self ;
}

-(void) drop {
  id index, aSequence ;

  index = [sequenceList begin: [self getZone]] ;
  while( (aSequence = [index next]) ){
    [index remove] ;
    [aSequence drop] ;
  }
  [index drop] ;
  [super drop] ;
}

@end


@implementation EZSequence

-setActiveOutFile: anActiveOutFile {
  theActiveOutFile = anActiveOutFile ;
  return self ;
}

-setActiveGrapher: aGrapher{
  theActiveGrapher = aGrapher ;
  return self ;
}

-step {

  if(theActiveGrapher)
    [theActiveGrapher step] ;

  if(theActiveOutFile)
    [theActiveOutFile step] ;

  return self ;
}

-(void) drop {
  [theActiveGrapher drop] ;
  [theActiveOutFile drop] ;
  [super drop] ;
}

@end 


@implementation EZAverageSequence

-setAverager: anAverager {
  theAverager = anAverager ;
  return self ;
}

-step {

  [theAverager update] ;
  [super step] ;

  return self ;
}

-(void) drop {
  [theAverager drop] ;
  [super drop] ;
}

@end 
