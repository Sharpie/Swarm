// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/VarProbeWidget.h>
#import <simtools/ProbeDisplay.h>
#import <simtools/global.h>
#import <objc/objc-api.h>

@implementation VarProbeWidget

+createBegin: aZone {
  id obj ;

  obj = [super createBegin: aZone] ;
  [obj setMaxLabelWidth: 0] ;

  return obj ;
}

-setObject: obj {
	myObject = obj ;
	return self ;
}

-setProbe: (Probe *) the_probe {
  myProbe = (VarProbe *) the_probe ;
  return self;
}

-setMyLeft: obj {
  myLeft = obj ;
  return self ;
}

-setMyRight: obj {
  myRight = obj ;
  return self ;
}

-setMaxLabelWidth: (int) width {
  maxLabelWidth = width ;
  return self ;
}

-createEnd {
  char theType ;

  [super createEnd];

  myLabel  = [Label  createParent: myLeft] ;
  [myLabel setText: [myProbe getProbedVariable]];
  [globalTkInterp
    eval: "%s configure -anchor e", [myLabel getWidgetName]] ;
  if(maxLabelWidth)
    [globalTkInterp
      eval: "%s configure -width %d", [myLabel getWidgetName],maxLabelWidth] ;

  
  myEntry  = [Entry  createParent: myRight] ;
  theType = ([myProbe getProbedType])[0] ;
  
  if([myProbe isInteractive]){
    [globalTkInterp eval:
     "bind %s <Return> {%s configure -highlightcolor red ;
                        update ;
                        %s setValue} ;
      bind %s <KeyRelease-Return> {%s configure -highlightcolor black ;
                                   update} ;
      bind %s <FocusIn> {%s selection range 0 end} ;
      bind %s <FocusOut> {%s selection clear}",
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      tclObjc_objectToName(self),
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName]
    ] ;

    interactive = 1 ;
  } else {

    [globalTkInterp eval: "%s configure -state disabled", 
                          [myEntry getWidgetName]] ;
  }

  if(theType == _C_ID){

    [globalTkInterp eval:
      "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;
                                 update ;
                                 %s Spawn ; 
                                 %s configure -highlightcolor black ;
                                 update} ;",
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName], 
      tclObjc_objectToName(self),
      [myEntry getWidgetName]] ;


    [globalTkInterp eval: 
  "drag&drop target %s handler id {%s idReceive}", 
      [myEntry getWidgetName],
      tclObjc_objectToName(self)] ;

    [globalTkInterp eval: "drag&drop source %s config -packagecmd {do_package %s} -sitecmd sitecmd -button 1", 
        [myEntry getWidgetName],
        tclObjc_objectToName(self)] ;

    [globalTkInterp eval: "drag&drop source %s handler id send_id", 
      [myEntry getWidgetName],
      tclObjc_objectToName(self)] ;

  } else

    [globalTkInterp eval:
      "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;
                           update ;
                           bell ; update ; 
                           %s configure -highlightcolor black ;
                           update} ;",
      [myEntry getWidgetName],
      [myEntry getWidgetName],
      [myEntry getWidgetName], 
      [myEntry getWidgetName]] ;

  [self update] ;

  return self;
}

-Spawn {
  id target ;

  target = ( *(id *)[myProbe probeRaw: myObject] ) ;

  if(target)
    [probeDisplayManager createProbeDisplayFor: target] ;   
  else
    [globalTkInterp eval: "bell ; update ;"] ;

  return self ;
}

-pack {

  [myLabel pack] ;  
  [myEntry pack] ;

  return self;
}

-setValue {

  [myProbe setData: myObject ToString: [myEntry getValue]] ;

  return self ;
}

-update {
  char buffer[512];

  if(!interactive){

    [globalTkInterp eval: "%s configure -state normal", 
                          [myEntry getWidgetName]] ;
    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]] ;
    [globalTkInterp eval: "%s configure -state disabled", 
                          [myEntry getWidgetName]] ;

  } else {

    [myEntry setValue: [myProbe probeAsString: myObject Buffer: buffer]] ;

  }

  [globalTkInterp eval: "update"] ;

  return self ;
}

-(void)drop {

  [myLabel drop] ;
  [myEntry drop] ;
  
  [super drop] ;
}

-idReceive {

  id resObj ;

  resObj = tclObjc_nameToObject([[globalTkInterp eval: 
     "gimme $DDOBJ"] result]) ;

  [myProbe setData: myObject To: &resObj ] ; 

  [globalTkInterp eval:
    "focus %s", [myEntry getWidgetName]] ;

  [self update] ;

  return self;
}

-(char *)package {

  id *content ;

  content = [myProbe probeRaw: myObject] ;
  if(*content == nil){
    [globalTkInterp eval: "bell ; update"] ;
    return "" ;
  }

  return tclObjc_objectToName(*content) ;  ;
}

-(const char *)getId {
  return [myEntry getValue] ;
}

@end


