// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <string.h>
#import <tkobjc.h>
#import <simtools/SimpleProbeDisplay.h>
#import <simtools/global.h>

@implementation SimpleProbeDisplay

-setProbedObject: (id) anObject {

  probedObject = anObject;
  return self;
}

-getProbedObject {
  return probedObject;
}

-setProbeMap: (ProbeMap *) p {

  probeMap = p;
  return self;
}

-getProbeMap {  
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
	
  topLevel = [Frame create: [self getZone]];
  
  [topLevel setWindowTitle: (char *) [probedObject name]] ;
  [globalTkInterp eval: "wm withdraw %s",
     [topLevel getWidgetName]] ;

  c_Frame =  [Frame  createParent: topLevel] ;  
  the_canvas = [Canvas createParent: c_Frame] ;

  [globalTkInterp eval: 
   "%s configure -width 10 -height 10 -yscrollcommand {%s.yscroll set} ; \
    scrollbar %s.yscroll -orient vertical -command {%s yview} ; \
    pack %s.yscroll -side right -fill y ; \
    pack %s -side left -fill both  -expand true",
    [the_canvas getWidgetName],
    [c_Frame getWidgetName],
    [c_Frame getWidgetName],
    [the_canvas getWidgetName],
    [c_Frame getWidgetName],
    [the_canvas getWidgetName]] ;

  [c_Frame pack] ;

  topFrame =  [Frame createParent: the_canvas] ;
  
  [globalTkInterp eval: "%s configure -bd 0", [topFrame getWidgetName]] ;

  [globalTkInterp eval: "%s create window 0 0 -anchor nw -window %s",
     [the_canvas getWidgetName],
     [topFrame getWidgetName]] ;
  
  top_top_Frame =  [Frame  createParent: topFrame] ;  

  raisedFrame =  [Frame  createParent: top_top_Frame] ;  

  [globalTkInterp eval: "%s configure -relief ridge -borderwidth 3",
    [raisedFrame getWidgetName]] ;

  myTitle  = [Label createParent: raisedFrame] ;

  if([probedObject respondsTo: @selector(getInstanceName)])
    [myTitle setText: (char *) [probedObject getInstanceName]];
  else
    [myTitle setText: (char *) [probedObject name]];

  [globalTkInterp
    eval: "%s configure -anchor w -foreground blue", [myTitle getWidgetName]] ;

  dragAndDrop (myTitle, self);
  {
    // have to make a private copy of the return for objectToName.
    char pdmName[512];
    strcpy(pdmName, tclObjc_objectToName(probeDisplayManager));
        
  [globalTkInterp eval: 
     "bind %s <ButtonPress-3> {%s createCompleteProbeDisplayFor: %s}",
		  [myTitle getWidgetName],
		  pdmName, tclObjc_objectToName(probedObject)];
  }
  
  [globalTkInterp eval: "bind %s <Enter> {%s configure -fg CornFlowerBlue}",
                        [myTitle getWidgetName],
                        [myTitle getWidgetName]] ;


  [globalTkInterp eval: "bind %s <Leave> {%s configure -fg blue}",
                        [myTitle getWidgetName],
                        [myTitle getWidgetName]] ;

  [myTitle pack] ;

  hideB = [Button createParent: top_top_Frame] ;

  [globalTkInterp 
    eval: "%s configure -command {%s drop}",
    [hideB getWidgetName],tclObjc_objectToName(self)] ;
  [globalTkInterp
    eval: "%s configure -bitmap special -activeforeground red -foreground red", 
    [hideB getWidgetName]] ;

  [globalTkInterp eval: "pack %s -side right -fill both -expand 0",
		  [hideB getWidgetName]] ;

  [globalTkInterp eval: "pack %s -before %s -side left -fill both -expand 0",
		  [raisedFrame getWidgetName],
		  [hideB getWidgetName]] ;

  middleFrame =  [Frame  createParent: topFrame] ;  

  leftFrame =  [Frame  createParent: middleFrame] ;  
  rightFrame = [Frame  createParent: middleFrame] ;

  bottomFrame = [Frame  createParent: topFrame] ;

  if (numberOfProbes > 0)
    widgets = (id *)
      [[self getZone] alloc: sizeof(id) * numberOfProbes];
  else
    widgets = 0;

  index = [probeMap begin: globalZone] ;

  i = 0 ;
  while( (probe = [index next]) != nil ){

    if([probe isKindOf: [VarProbe class]]){
      widgets[i] =	
        [[VarProbeWidget createBegin: [self getZone]] setParent: topFrame] ;
      [widgets[i]  setProbe: probe] ;
      [widgets[i] setObject: probedObject] ;
      [widgets[i]  setMyLeft:  leftFrame] ;
      [widgets[i]  setMyRight: rightFrame] ;
      widgets[i] = [widgets[i] createEnd] ;
      [widgets[i] pack] ;
      i++ ;
    }
  }

  [index drop];

  index = [probeMap begin: globalZone] ;

  //When I figure out how to 'rewind' I'll do just that...
  while( (probe = [index next]) != nil ){

    if([probe isKindOf: [MessageProbe class]]){
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
  if(!i){
    index = [Label createParent: topFrame] ;
    [index setText: "No Instance Variables or Messages."] ;
    [index pack] ;
  }

  [globalTkInterp eval: "pack %s -fill both -expand 0",
		  [top_top_Frame getWidgetName]] ;
  
  [globalTkInterp eval: "pack %s -side left -fill both -expand 0",
		  [leftFrame getWidgetName]] ;
    
  [globalTkInterp eval: "pack %s -side left -fill both -expand 1",
		  [rightFrame getWidgetName]] ;

  [middleFrame pack] ;
  [bottomFrame pack] ;

//  [globalTkInterp eval: "update idletasks"] ;
  [globalTkInterp eval: "wm deiconify %s",[topLevel getWidgetName]] ;

  [globalTkInterp eval:
     "tkwait visibility %s ; \
      set width [winfo width %s] ; \
      set height [winfo height %s] ; \
      %s configure -scrollregion [list 0 0 $width $height] ; \
      if {$height > 500} {set height 500} ; \
      %s configure -width $width -height $height",
      [topFrame getWidgetName],
//      [topFrame getWidgetName],
      [topFrame getWidgetName],
      [topFrame getWidgetName],
      [the_canvas getWidgetName],
      [the_canvas getWidgetName]] ;

  [probeDisplayManager addProbeDisplay: self];

  return self;
}

-update {
  int i ;

  for (i = 0; i < numberOfProbes; i++)
    [widgets[i] update] ;
  
  return self;
}

-(void) setRemoveRef: (BOOL) torf {
  removeRef = torf;
}

-(void) setObjectRef: (ref_t) or {
  objectRef = or;
}

-(void)drop{

  int i ;

  [leftFrame drop] ;
  [rightFrame drop] ;
  [middleFrame drop] ;
  [bottomFrame drop] ;

  for(i = 0 ; i < numberOfProbes ; i++)
    [widgets[i] drop] ;

  if(numberOfProbes)
    [[self getZone] free: widgets] ;


  [globalTkInterp eval: "destroy %s",[topLevel getWidgetName]] ;
  [topLevel drop] ;


  [probeDisplayManager removeProbeDisplay: self];
  
  if (removeRef) [probedObject removeRef: objectRef];

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





