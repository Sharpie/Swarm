// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtoolsgui/CompleteProbeDisplay.h>
#import <simtoolsgui/ClassDisplayWidget.h>
#import <simtoolsgui.h> // probeDisplayManager
#import <collections.h> // List
#import "../defobj/internal.h" // map_objc_class_ivars

#include <swarmconfig.h>
#ifdef HAVE_JDK
#import "../defobj/java.h" // map_java_class_ivars
#endif

#include <misc.h> // strlen

static size_t
max_class_varname_length (Class class)
{
  size_t maxlen = 0;
  void notelen (const char *name)
    {
      size_t len = strlen (name);
      
      if (len > maxlen)
        maxlen = len;
    }
#ifdef HAVE_JDK
  if ([class respondsTo: M(isJavaProxy)])
    {
      void process_java_ivar (const char *name, fcall_type_t type)
        {
          notelen (name);
        }
      map_java_class_ivars (SD_JAVA_FIND_OBJECT_JAVA (class),
                            process_java_ivar);
    }
  else
#endif
    {
      void process_objc_ivar (const char *name, fcall_type_t type,
                              unsigned offset, unsigned rank, unsigned *dims)
        {
          notelen (name);
        }
      map_objc_class_ivars (class, process_objc_ivar);
    }
  return maxlen;
}

@implementation CompleteProbeDisplay

PHASE(Creating)

// finalize creation: create widgets, set them up.
- createEnd
{
  Class class;
  id classWidget;
  id classList;
  id index;
  id previous;
  size_t maxWidth;

  horizontalScrollbarFlag = YES;
  [super createEnd];

  maxWidth = 0;

  classList = [List create: [self getZone]];
  for (class = SD_GETCLASS (probedObject);
       class != nil; 
       class = SD_SUPERCLASS (class))
    {
      size_t classWidth = max_class_varname_length (class);

      if (classWidth > maxWidth)
        maxWidth = classWidth;

      [classList addFirst: (id) class];
    }
  
  widgets = [List create: [self getZone]];
  previous = nil;
  index = [classList begin: [self getZone]];
  while ((class = (Class) [index next]))
    {
      classWidget = 
        [[[[[[[ClassDisplayWidget createBegin: [self getZone]] 
               setParent: topFrame]
              setMaxLabelWidth: maxWidth]
             setProbedObject: probedObject]
            setClassToDisplay: class]
           setOwner: self]
          setMySuperclass: previous];
      
      if (previous != nil)
        {
          [previous setMySubclass: classWidget];
          previous = [previous createEnd];
          [widgets addLast: previous];
        }
      previous = classWidget;
    }
  [previous setMySubclass: nil];
  [previous setOwner: self];
  previous = [previous createEnd];
  [widgets addLast: previous];

  [previous pack];
  [index drop];

  [classList drop];

  [self install];
  return self;
}

PHASE(Using)

- do_resize
{
  [canvas packForgetAndExpand];
  [canvas checkGeometry: topFrame];
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
   if (!markedForDropFlag)
      [a_widget update];
    else
      break;
  [index drop];
  return self;
}

- (void)drop
{
  id index, a_widget;

  // If drops all go through markForDrop, then the disable there
  // should take care of it.
  // [topLevel disableDestroyNotification];

  index = [widgets begin: [self getZone]];
  while ((a_widget = [index next]) != nil)
    [a_widget drop];
  [index drop];
  [widgets drop];

  [probeDisplayManager removeProbeDisplay: self];
  
  if (removeRef)
    [probedObject removeRef: objectRef];
  
  [super drop]; 
}
@end

