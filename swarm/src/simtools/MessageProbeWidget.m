// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import <string.h>
#import <ctype.h>
#import <stdio.h>
#import <malloc.h>
#import <stdlib.h> // for alpha
#import <simtools/MessageProbeWidget.h>
#import <simtools/ProbeDisplay.h>
#import <simtools/global.h>

@implementation MessageProbeWidget

+createBegin: aZone {
  id obj ;

  obj = [super createBegin: aZone] ;
  [obj setMaxReturnWidth: 0] ;

  return obj ;
}

-setObject: obj {
  myObject = obj ;
  return self ;
}

-setProbe: (Probe *) the_probe {
  myProbe = (MessageProbe *) the_probe ;
  return self;
}

-setMaxReturnWidth: (int) width {
  maxReturnWidth = width ;
  return self ;
}

-createEnd {

  int i, which_arg ;
  char bcmd[1024] ;
  
  [super createEnd] ;
  
  if(![myProbe getHideResult]){
    result = [Entry createParent: self] ;
    [globalTkInterp eval: 
      "%s configure -state disabled",
      [result getWidgetName]];
    if(maxReturnWidth)
      [globalTkInterp
        eval: "%s configure -width %d", [result getWidgetName],maxReturnWidth] ;
    if([myProbe isResultId]){
      [globalTkInterp eval:
        "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;
                                   update ;
                                   %s Spawn ; 
                                   %s configure -highlightcolor black ;
                                   update ; focus %s ; update } ;",
        [result getWidgetName],
        [result getWidgetName],
        [result getWidgetName], 
        tclObjc_objectToName(self),
        [result getWidgetName],
        [self getWidgetName]] ;
      dragAndDrop (result, self);
    } else {
      [globalTkInterp eval:
        "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;
                             update ;
                             bell ; update ; 
                             %s configure -highlightcolor black ;
                             update ; focus %s ; update} ;",
        [result getWidgetName],
        [result getWidgetName],
        [result getWidgetName], 
        [result getWidgetName],
        [self getWidgetName]] ;
    }

    [globalTkInterp eval: 
      "pack %s -side left -expand 0 -fill both",
      [result getWidgetName]];
  }

  argNum = [myProbe getArgNum] ;

  if(argNum){
    objWindows = (int *) malloc(sizeof(int)*argNum) ;
    argNum *= 2 ; 
    myWidgets = (Widget **) malloc(sizeof(Widget *)*argNum) ;
  } else {
    myWidgets = (Widget **) malloc(sizeof(Widget *)) ;
  }

  myWidgets[0] = [Button createParent: self] ;

  bcmd[0] = '\0' ;
  strcat(bcmd,tclObjc_objectToName(self)) ;
  strcat(bcmd," dynamic") ;
  [(Button *)myWidgets[0] setCommand: bcmd] ;

  [globalTkInterp eval: "%s configure -text %s",
    [myWidgets[0] getWidgetName], [myProbe getArgName: 0]];

  if(argNum)
    [globalTkInterp eval: 
      "pack %s -side left -expand 0 -fill both",
      [myWidgets[0] getWidgetName]];
  else   
    [globalTkInterp eval: 
      "pack %s -side left -expand 1 -fill both",
      [myWidgets[0] getWidgetName]];

  for(i = 1 ; i < argNum ; i++ ){

    which_arg = i / 2 ;

    if(i%2){

      myWidgets[i] = [Entry createParent: self] ;
      if([myProbe isArgumentId: which_arg]){

        objWindows[which_arg] = 1 ;
        [globalTkInterp eval: "%s configure -state disabled", 
           [myWidgets[i] getWidgetName]] ;

        [globalTkInterp eval:
          "bind %s <Button-3> {focus %s ; %s configure -highlightcolor red ;
                                 update ;
                                 %s argSpawn: %d ; 
                                 %s configure -highlightcolor black ;
                                 update ; focus %s ; update } ;",
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName], 
          tclObjc_objectToName(self),
          which_arg,
          [myWidgets[i] getWidgetName],
          [self getWidgetName]] ;

        dragAndDropArg (myWidgets[i], self, which_arg);

      } else {
        objWindows[which_arg] = 0 ;
        [globalTkInterp eval:
          "bind %s <Button-3> {focus %s; %s configure -highlightcolor red ;
                               update ;
                               bell ; update ; 
                               %s configure -highlightcolor black ;
                               update} ;",
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName], 
          [myWidgets[i] getWidgetName]] ;

        [globalTkInterp eval: 
          "bind %s <FocusIn> {%s selection range 0 end} ;
           bind %s <FocusOut> {%s selection clear}",
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName],
          [myWidgets[i] getWidgetName]];
      }

      [globalTkInterp eval: 
        "pack %s -side left -expand 1 -fill both",
        [myWidgets[i] getWidgetName]];

    } else {

      myWidgets[i] = [Label createParent: self] ;
      [globalTkInterp eval: 
        "%s configure -text %s",
        [myWidgets[i] getWidgetName],[myProbe getArgName: which_arg]];

      [globalTkInterp eval: 
        "pack %s -side left -expand 0 -fill both",
        [myWidgets[i] getWidgetName]];
    }
  }

  return self ;
}

int empty(const char *str){
  int i, length  ;

  if(str == NULL)
    return 1 ;

  length = strlen(str) ;
  for(i = 0 ; i < length ; i++){
    if(!isspace(str[i]))
      break ;
  }

  if(i < length)
    return 0 ;
  else 
    return 1 ;
}

-dynamic {
  int i ;
  char *test ;
  char *result_string ;

  for(i = 0 ; i < (argNum / 2) ; i++){

    test = strdup([ ((Entry *) myWidgets[2*i + 1]) getValue]) ;

    if(empty(test)){
      [globalTkInterp eval: "bell"] ;
      return self ;
    }
    
    if(!objWindows[i])
      [myProbe setArg: i To: test] ;
  }

  // Here I must insist on a TCLOBJC mediated call since there will often
  // be situations where the probe might attempt a direct call thus casting
  // the result to an int (when the probedMessage does not take arguments).
  [myProbe _trueDynamicCallOn_: myObject resultStorage: &result_string] ;
 
  if(![myProbe getHideResult]){
    [globalTkInterp eval: 
      "%s configure -state normal",
      [result getWidgetName]];

    if([myProbe isResultId])
      if((resultObject = tclObjc_nameToObject(result_string)) != nil)
        if([resultObject respondsTo: @selector(getInstanceName)])
          [result setValue: (char *)[resultObject getInstanceName]] ;
        else
          [result setValue: (char *)[resultObject name]] ;
      else    
        [result setValue: result_string] ;
    else
      [result setValue: result_string] ;

    [globalTkInterp eval: 
      "%s configure -state disabled",
      [result getWidgetName]];
  }

  free(result_string) ;


  [probeDisplayManager update] ;

  return self ;
}

-Spawn {
  if(resultObject != nil)
    [probeDisplayManager createProbeDisplayFor: resultObject];
  else
    [globalTkInterp eval: "bell ; update"] ;

  return self ;
}

-argSpawn: (int) which {
  id arg_obj ;
  char *id_name ;

  id_name = [myProbe getArg: which] ;
  
  if(id_name != NULL){
    arg_obj = tclObjc_nameToObject(id_name) ;
    [probeDisplayManager createProbeDisplayFor: arg_obj];
  }
  else
    [globalTkInterp eval: "bell ; update"] ;
    
  return self ;
}

-update {
  return self ;
}

-(void)drop {

  int i ;

  for(i = 0 ; i < argNum ; i++)
    [myWidgets[i] drop] ;
  
  [super drop] ;
}

-(char *)package {

  if(resultObject == nil){
    [globalTkInterp eval: "bell ; update"] ;
    return "" ;
  }
  
  return tclObjc_objectToName(resultObject) ;  ;
}

-(char *)package: (int) which {
  char *id_name ;

  id_name = [myProbe getArg: which] ;
  
  if(id_name == NULL){
    [globalTkInterp eval: "bell ; update"] ;
    return "" ;
  }
    
  return id_name ;  ;
}

-(const char *)getId {
  if(![myProbe getHideResult])
    return [result getValue] ;
  else
    return NULL ;
}

-(const char *)getId: (int) which {
  return [((Entry *) myWidgets[which*2 + 1]) getValue] ;
}

-idReceive: (int) which {

  id resObj ;
  char *objName ;

  objName = strdup([[globalTkInterp eval: "gimme $DDOBJ"] result]) ;
  resObj = tclObjc_nameToObject(objName) ;

  [myProbe setArg: which To: objName] ;

  which *= 2 ;
  which += 1 ;

  [globalTkInterp eval: "%s configure -state normal", 
                        [myWidgets[which] getWidgetName]] ;
  if([resObj respondsTo: @selector(getInstanceName)])
    [((Entry *)myWidgets[which]) 
       setValue: (char *)[resObj getInstanceName]] ;
  else
    [((Entry *)myWidgets[which]) 
       setValue: (char *)[resObj name]] ;

  [globalTkInterp eval: "%s configure -state disabled ; update", 
                        [myWidgets[which] getWidgetName]] ;

  return self;
}

@end
