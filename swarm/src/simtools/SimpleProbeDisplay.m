// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <string.h>
#import <tkobjc/control.h>
#import <simtools/SimpleProbeDisplay.h>
#import <simtools/global.h>
#import <simtools/Archiver.h>

@implementation SimpleProbeDisplay

- setWindowGeometryRecordName: (const char *)theName
{
  windowGeometryRecordName = theName;
  return self;
}

- setProbedObject: (id)anObject
{

  probedObject = anObject;
  return self;
}

- getProbedObject
{
  return probedObject;
}

- setProbeMap: (ProbeMap *)p
{
  probeMap = p;
  return self;
}

- getProbeMap
{  
  return probeMap;
}

// finalize creation: create widgets, set them up.
- createEnd
{
  int i ;
  id index ;
  id probe ;
  id hideB ;
  id top_top_Frame, raisedFrame, c_Frame, the_canvas ;

  numberOfProbes = [probeMap getNumEntries] ;
	
  topLevel = [Frame createBegin: [self getZone]];
  [topLevel setWindowGeometryRecordName : windowGeometryRecordName];
  topLevel= [topLevel createEnd]; 
  [topLevel setWindowTitle: (char *) [probedObject name]] ;
  withdrawWindow (topLevel);

  c_Frame =  [Frame createParent: topLevel]; 

  the_canvas = [Canvas createParent: c_Frame];
  configureProbeCanvas (the_canvas);

  [c_Frame pack];

  topFrame =  [Frame createParent: the_canvas];
  setBorderWidth (topFrame, 0);

  createWindow (topFrame);
  
  top_top_Frame =  [Frame createParent: topFrame];  

  raisedFrame =  [Frame createParent: top_top_Frame];  
  setRelief (raisedFrame);

  myTitle = [Label createParent: raisedFrame];
  [myTitle setText: getId (probedObject)];
  
  setAnchorWest (myTitle);
  setColorBlue (myTitle);

  dragAndDrop (myTitle, self);

  configureButton3ForCompleteProbeDisplay (myTitle, probedObject);
  configureWindowEntry (myTitle);
  configureWindowExit (myTitle);
  
  [myTitle pack];
  
  hideB = [Button createParent: top_top_Frame];

  configureHideButton (self, hideB, raisedFrame);

  middleFrame =  [Frame  createParent: topFrame] ;  
  leftFrame =  [Frame createParent: middleFrame];
  rightFrame = [Frame createParent: middleFrame];
  bottomFrame = [Frame createParent: topFrame];

  if (numberOfProbes > 0)
    widgets = (id *)
      [[self getZone] alloc: sizeof(id) * numberOfProbes];
  else
    widgets = 0;

  index = [probeMap begin: globalZone] ;

  i = 0 ;
  while ((probe = [index next]) != nil)
    {      
      if([probe isKindOf: [VarProbe class]])
        {
          widgets[i] =	
            [[VarProbeWidget createBegin: [self getZone]]
              setParent: topFrame];
          [widgets[i]  setProbe: probe] ;
          [widgets[i] setObject: probedObject] ;
          [widgets[i]  setMyLeft:  leftFrame] ;
          [widgets[i]  setMyRight: rightFrame] ;
          widgets[i] = [widgets[i] createEnd] ;
          [widgets[i] pack];
          i++ ;
      }
    }
  
  [index drop];

  index = [probeMap begin: globalZone] ;

  //When I figure out how to 'rewind' I'll do just that...
  while ((probe = [index next]) != nil)
    {
      if([probe isKindOf: [MessageProbe class]])
        {
          widgets[i] =	
            [[MessageProbeWidget createBegin: [self getZone]] 
              setParent: bottomFrame] ;
          [widgets[i]  setProbe: probe] ;
          [widgets[i] setObject: probedObject] ;
          widgets[i] = [widgets[i] createEnd] ;
          [widgets[i] pack] ;
          i++ ;
        }
    }
  
  [index drop];

  // This label is not being garbage collected!!!!!
  if (!i)
    {
      index = [Label createParent: topFrame] ;
      [index setText: "No Instance Variables or Messages."] ;
      [index pack] ;
    }

  packFill (top_top_Frame);
  packFillLeft (leftFrame, 0);
  packFillLeft (rightFrame, 1);

  [middleFrame pack];
  [bottomFrame pack];

  deiconify (topLevel);
  assertGeometry (topFrame);

  markedForDropFlag = NO;

  [probeDisplayManager addProbeDisplay: self];
  
  return self;
}

- update
{
  int i ;

  for (i = 0; i < numberOfProbes; i++)
    [widgets[i] update] ;
  
  return self;
}

- (void)setRemoveRef: (BOOL) theRemoveRef
{
  removeRef = theRemoveRef;
}

- (void)setObjectRef: (ref_t)theObjectRef
{
  objectRef = theObjectRef;
}

- (void)markForDrop
{
  if ([probeDisplayManager getDropImmediatelyFlag])
    [self drop];
  else
    markedForDropFlag = YES;
}

- (BOOL)getMarkedForDropFlag
{
  return markedForDropFlag;
}

- (void)drop
{
  int i ;

  [leftFrame drop] ;
  [rightFrame drop] ;
  [middleFrame drop] ;
  [bottomFrame drop] ;

  for(i = 0 ; i < numberOfProbes ; i++)
    [widgets[i] drop] ;

  if(numberOfProbes)
    [[self getZone] free: widgets] ;

  [topLevel drop] ;

  [probeDisplayManager removeProbeDisplay: self];

  if (removeRef)
    [probedObject removeRef: objectRef];
  
  [super drop] ;
}

- (const char *)package
{
  return packageName (probedObject);
}

- (const char *)getId
{
  return getId (probedObject);
}

@end
