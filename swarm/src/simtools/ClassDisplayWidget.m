// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <stdlib.h>
#import <objc/objc.h>
#import <string.h>
#import <tkobjc.h>
#import <simtools/ClassDisplayWidget.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

@implementation ClassDisplayWidget

+createBegin: aZone {
  id obj ;

  obj = [super createBegin: aZone] ;
  [obj setMaxLabelWidth: 0] ;
  return obj ;
}

-setOwner: anOwner{
  owner = anOwner ;
  return self ;
}

-setMySuperClass: aWidget {
  mySuperClass = aWidget ;
  return self ;
}

-setMySubClass: aWidget {
  mySubClass = aWidget ;
  return self ;
}

-setProbedObject: (id) anObject {
  if (SAFEPROBES) {
    if (probedObject != 0) {
      [InvalidCombination raiseEvent:
 "It is an error to reset the object when building a ClassDisplayWidget\n"];
      return nil;
    }
  }
  probedObject = anObject;
  return self;
}

-setMaxLabelWidth: (int) width {
  maxLabelWidth = width ;
  return self ;
}

-getProbedObject {
  return probedObject;
}

-setClassToDisplay: (Class) aClass {
  theClass = aClass ;
  return self ;
}

-getProbeMap {
  return probeMap;
}

// finalize creation: create widgets, set them up.
-createEnd {
  int i ;
  id index ;
  id probe ;
  char *ownerName ; //uses because tclObjc_objectToName is non-reentrant

  [super createEnd];

  if (SAFEPROBES) {
    if (probedObject == 0 || theClass == 0) {
      [InvalidCombination raiseEvent: 
        "ClassDisplayWidget object was not properly initialized\n"];
      return nil;
    }
  }

  ownerName = strdup(tclObjc_objectToName(owner)) ;

  [globalTkInterp eval:
    "%s configure -borderwidth 3 -relief ridge",
    [self getWidgetName]] ;

  probeMap = [[[ProbeMap createBegin: [self getZone]] 
                       setProbedClass: theClass]
                       createEnd] ;

  numberOfProbes = [probeMap getNumEntries] ;
	
  topRow = [Frame createParent: self] ;

  myTitle  = [Label createParent: topRow] ;
  [myTitle setText: (char *)theClass->name];
  [globalTkInterp
    eval: "%s configure -anchor w -foreground blue", [myTitle getWidgetName]] ;

  dragAndDrop (myTitle, self);

  hideB = [Button createParent: topRow] ;
  if(mySubClass != nil){
    [globalTkInterp 
      eval: "%s configure -command {pack forget %s ; \
             %s armSuperButton ; %s do_resize}",
      [hideB getWidgetName],[self getWidgetName],
      tclObjc_objectToName(mySubClass),ownerName] ;
    [globalTkInterp
      eval: "%s configure -bitmap hide -activeforeground red -foreground red", 
      [hideB getWidgetName]] ;
  } else {
    [globalTkInterp 
      eval: "%s configure -command {%s drop}",
      [hideB getWidgetName],ownerName] ;
    [globalTkInterp
  eval: "%s configure -bitmap special -activeforeground red -foreground red", 
      [hideB getWidgetName]] ;
  }

  superB = [Button createParent: topRow] ;
  [globalTkInterp
  eval: "%s configure -bitmap super -activeforeground forestgreen -foreground forestgreen", 
    [superB getWidgetName]] ;

  if (mySuperClass != nil) {
    [globalTkInterp eval: "%s configure -command { pack %s -before %s -fill both -expand 1 ; %s configure -state disabled ; %s do_resize}",
		    [superB getWidgetName],[mySuperClass getWidgetName],
		    [self getWidgetName],[superB getWidgetName],
                    ownerName];
  } else {
    [globalTkInterp eval: "%s configure -command { bell }; %s configure -state disabled",
		    [superB getWidgetName], [superB getWidgetName]];
  }
   
  middleFrame =  [Frame  createParent: self] ;  
  leftFrame =  [Frame  createParent: middleFrame] ;  
  rightFrame = [Frame  createParent: middleFrame] ;
  bottomFrame = [Frame  createParent: self] ;

  if (numberOfProbes > 0)
    widgets = (id *)
      [[self getZone] alloc: sizeof(id) * numberOfProbes];
  else
    widgets = 0;
	
  index = [probeMap begin: globalZone] ;

  i = 0 ;
  while( (probe = [index next]) ){

    if([probe isKindOf: [VarProbe class]]){
      widgets[i] =	
        [[VarProbeWidget createBegin: [self getZone]] setParent: self] ;

      [widgets[i]  setProbe: probe] ;
      [widgets[i] setObject: probedObject] ;
      [widgets[i]  setMyLeft:  leftFrame] ;
      [widgets[i]  setMyRight: rightFrame] ;
      [widgets[i]  setMaxLabelWidth: maxLabelWidth ] ;

      widgets[i] = [widgets[i] createEnd] ;

      [widgets[i] pack] ;

      i++ ;
    }
  }
  [index drop];

  //Will 'rewind' index when I find out how...
  index = [probeMap begin: globalZone] ;

  while( (probe = [index next]) ){

    if([probe isKindOf: [MessageProbe class]]){
      widgets[i] =	
        [[MessageProbeWidget createBegin: [self getZone]] 
                            setParent: bottomFrame] ;

      [widgets[i]  setProbe: probe] ;
      [widgets[i] setObject: probedObject] ;
      [widgets[i]  setMaxReturnWidth: maxLabelWidth ] ;
      widgets[i] = [widgets[i] createEnd] ;
      [widgets[i] pack] ;
      i++ ;
    }
  }

  [index drop];

  [globalTkInterp eval: "pack %s %s -side right",
	  	    [superB getWidgetName],[hideB getWidgetName]] ;

  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand 0",
		  [myTitle getWidgetName],
		  [hideB getWidgetName]] ;

  [topRow pack] ;

  [globalTkInterp eval: "pack %s -side left -fill both -expand 0",
		  [leftFrame getWidgetName]] ;

  [globalTkInterp eval: "pack %s -side left -fill both -expand 1",
		  [rightFrame getWidgetName]] ;

  [middleFrame pack] ;
  [bottomFrame pack] ;

  return self;
}

-armSuperButton {
  [globalTkInterp eval: "%s configure -state normal",
     [superB getWidgetName]] ;

  return self ;
}

-update {
  int i ;

  for (i = 0; i < numberOfProbes; i++)
    [widgets[i] update] ;
  
  return self;
}

-(void)drop {
  
  int i ;

  [topRow drop] ;
  [myTitle drop] ;
  [hideB drop] ;
  [superB drop] ;
  [leftFrame drop] ;
  [rightFrame drop] ;
  [middleFrame drop] ;
  [bottomFrame drop] ;
  if(numberOfProbes > 0){
    for(i = 0 ; i < numberOfProbes ; i++)
      [widgets[i] drop] ;
    [[self getZone] free: widgets] ;
  }

  [super drop] ;
}

-(char *) package{
  return tclObjc_objectToName(probedObject) ;  ;
}

-(const char *) getId{
  if([probedObject respondsTo: @selector(getInstanceName)])
    return [probedObject getInstanceName] ;
  else
    return [probedObject name];
}

@end
