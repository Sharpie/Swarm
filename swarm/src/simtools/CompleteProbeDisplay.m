// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <string.h>
#import <tkobjc.h>
#import <objc/objc.h>
#import <objc/objc-api.h>
#import <simtools/CompleteProbeDisplay.h>
#import <simtools/global.h>

@implementation CompleteProbeDisplay

-setProbedObject: (id) anObject {
  probedObject = anObject;
  return self;
}

-getProbedObject {
  return probedObject;
}

int max(int a, int b){
  if(a > b)
    return a ;
  else
    return b ;
}

int max_class_var_length(Class class){
  IvarList_t ivarList;	
  int i, local_max, num;

  local_max = 0 ;

  if(!(ivarList = class->ivars))
    return 0 ;

  num = ivarList->ivar_count;

  for (i = 0; i < num ; i++)
    local_max = max(strlen(ivarList->ivar_list[i].ivar_name),local_max) ;

  return local_max ;
}

// finalize creation: create widgets, set them up.
-createEnd {
  Class class ;
  id classWidget ;
  int maxwidth ;
  id classList ;
  id index ;
  id previous ;
  id c_Frame ;

  topLevel = [Frame create: [self getZone]] ;
  [globalTkInterp eval: "wm withdraw %s",
     [topLevel getWidgetName]] ;

  if([probedObject respondsTo: @selector(getInstanceName)])
    [topLevel setWindowTitle: (char *) [probedObject getInstanceName]];
  else
    [topLevel setWindowTitle: (char *) [probedObject name]];

  c_Frame = [Frame  createParent: topLevel] ;  
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

  topFrame = [Frame createParent: the_canvas] ;

  [globalTkInterp eval: "%s configure -bd 0", [topFrame getWidgetName]] ;

  [globalTkInterp eval: "%s create window 0 0 -anchor nw -window %s",
     [the_canvas getWidgetName],
     [topFrame getWidgetName]] ;
  

  maxwidth = 0 ;

  classList = [List create: [self getZone]] ;
  for (class = [probedObject class]; 
       class != nil; 
       class = class_get_super_class(class)){
    
    [classList addFirst: (id) class] ;	
    maxwidth = max(max_class_var_length(class),maxwidth) ;
  }

  widgets = [List create: [self getZone]] ;
  previous = nil ;
  index = [classList begin: [self getZone]] ;
  while( (class = (Class) [index next]) ){
    classWidget = 
     [
      [
      [
       [
        [
         [
          [ClassDisplayWidget createBegin: [self getZone]] 
         setParent: topFrame]
        setMaxLabelWidth: maxwidth]
       setProbedObject: probedObject]
      setClassToDisplay: class]
     setOwner: self]
    setMySuperClass: previous] ;

    if(previous != nil){
      [previous setMySubClass: classWidget] ;
      previous = [previous createEnd] ;
      [widgets addLast: previous] ;
    }

    previous = classWidget ;
  }

  [previous setMySubClass: nil] ;
  [previous setOwner: self] ;
  previous =[previous createEnd] ;
  [widgets addLast: previous] ;

  [previous pack] ;

  [index drop];

  [classList drop] ;

  [globalTkInterp eval: "wm deiconify %s",[topLevel getWidgetName]] ;

  [globalTkInterp eval:
     "tkwait visibility %s ; \
      set width [winfo width %s] ; \
      set height [winfo height %s] ; \
      %s configure -scrollregion [list 0 0 $width $height] ; \
      if {$height > 500} {set height 500} ; \
      %s configure -width $width -height $height",
      [topFrame getWidgetName],
      [topFrame getWidgetName],
      [topFrame getWidgetName],
      [the_canvas getWidgetName],
      [the_canvas getWidgetName]] ;

  [probeDisplayManager addProbeDisplay: self];

  return self;
}

-do_resize {
  [globalTkInterp eval:
     "pack forget %s ; \
      pack %s -expand true -fill both ; \
      tkwait visibility %s ;
      set width [winfo width %s] ; \
      set height [winfo height %s] ; \
      %s configure -scrollregion [list 0 0 $width $height] ; \
      if {$height > 500} {set height 500} ; \
      %s configure -width $width -height $height",
      [the_canvas getWidgetName],      
      [the_canvas getWidgetName],      
      [the_canvas getWidgetName],      
      [topFrame getWidgetName],
      [topFrame getWidgetName],
      [the_canvas getWidgetName],
      [the_canvas getWidgetName]] ;
  return self ;
}

-(int) getStepHeight{
  id index, aWidget ;
  int s_height ;

  index = [widgets begin: [self getZone]] ;
  while( (aWidget = [index next]) )
    if( (s_height = [aWidget getStepHeight] ) )
      return s_height ;

  return 20 ; // If all else fails, here is a default value...    
}

-update {
  id index ;
  id a_widget ;

  index = [widgets begin: [self getZone]] ;
  while( (a_widget = [index next]) != nil )
    [a_widget update] ;
  [index drop];

  return self;
}

-(void) setRemoveRef: (BOOL) torf {
  removeRef = torf;
}

-(void) setObjectRef: (ref_t) or {
  objectRef = or;
}

-(void)drop {
 
  id index ;
  id a_widget ;

  index = [widgets begin: [self getZone]] ;
  while( (a_widget = [index next]) != nil )
    [a_widget drop] ;
  [index drop];
  [widgets drop] ;

  [globalTkInterp eval: "destroy %s",[topFrame getWidgetName]] ;
  [topFrame drop] ;

 
  //drop for toplevels should automatically do a self-destroy!!!
  [globalTkInterp eval: "destroy %s",[topLevel getWidgetName]] ;
  [topLevel drop] ;

  [probeDisplayManager removeProbeDisplay: self];
  
  if (removeRef) [probedObject removeRef: objectRef];

  [super drop] ; 
}
@end
