// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <string.h>
#import <tkobjc/control.h>
#import <simtools/CompleteProbeDisplay.h>
#import <simtools/ClassDisplayWidget.h>
#import <simtools/global.h> // probeDisplayManager
#import <objc/objc-api.h> // IvarList_t
#import <collections.h> // List

@implementation CompleteProbeDisplay

static int
max (int a, int b)
{
  if(a > b)
    return a;
  else
    return b;
}

static int
max_class_var_length (Class class)
{
  IvarList_t ivarList;	
  int i, local_max, num;
  
  local_max = 0 ;

  if (!(ivarList = class->ivars))
    return 0 ;
  
  num = ivarList->ivar_count;

  for (i = 0; i < num ; i++)
    local_max = max (strlen (ivarList->ivar_list[i].ivar_name), local_max);
  
  return local_max ;
}

// finalize creation: create widgets, set them up.
- createEnd
{
  Class class;
  id classWidget;
  int maxwidth;
  id classList;
  id index;
  id previous;

  [super createEnd];

  maxwidth = 0;

  classList = [List create: [self getZone]];
  for (class = [probedObject class]; 
       class != nil; 
       class = class_get_super_class(class))
    {
      [classList addFirst: (id) class];	
      maxwidth = max(max_class_var_length(class),maxwidth);
    }
  
  widgets = [List create: [self getZone]];
  previous = nil;
  index = [classList begin: [self getZone]];
  while ((class = (Class) [index next]))
    {
      classWidget = 
        [[[[[[[ClassDisplayWidget createBegin: [self getZone]] 
               setParent: topFrame]
              setMaxLabelWidth: maxwidth]
             setProbedObject: probedObject]
            setClassToDisplay: class]
           setOwner: self]
          setMySuperClass: previous];
      
      if (previous != nil)
        {
          [previous setMySubClass: classWidget];
          previous = [previous createEnd];
          [widgets addLast: previous];
        }
      previous = classWidget;
    }
  [previous setMySubClass: nil];
  [previous setOwner: self];
  previous = [previous createEnd];
  [widgets addLast: previous];

  [previous pack];
  [index drop];

  [classList drop];

  [self install];
  return self;
}

- do_resize
{
  tkobjc_packForgetAndExpand (canvas);
  tkobjc_assertGeometry (topFrame);
  return self;
}

- (int)getStepHeight
{
  id index, aWidget;
  int s_height;
  
  index = [widgets begin: [self getZone]];
  while ((aWidget = [index next]))
    if ((s_height = [aWidget getStepHeight]))
      return s_height;
  
  return 20; // If all else fails, here is a default value...    
}

- update 
{
  id index;
  id a_widget;

  index = [widgets begin: [self getZone]];
  while ((a_widget = [index next]) != nil)
    [a_widget update];
  [index drop];
  
  return self;
}

- (void)drop
{
 
  id index;
  id a_widget;

  index = [widgets begin: [self getZone]];
  while ((a_widget = [index next]) != nil)
    [a_widget drop];
  [index drop];
  [widgets drop];

  [topFrame drop];
  [topLevel drop];

  [probeDisplayManager removeProbeDisplay: self];
  
  if (removeRef)
    [probedObject removeRef: objectRef];
  
  [super drop]; 
}
@end

