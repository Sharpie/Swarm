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

#import <simtoolsgui/CompleteProbeDisplay.h>
#import <simtoolsgui/ClassDisplayWidget.h>
#import <simtoolsgui.h> // probeDisplayManager
#import <collections.h> // List
#import <defobj/defalloc.h> // getZone
#import "../defobj/internal.h" // map_objc_class_ivars

#import <defobj/directory.h>

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
      map_java_class_ivars (SD_JAVA_FIND_CLASS_JAVA (class),
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

  classList = [List create: getZone (self)];
  for (class = SD_GETCLASS (probedObject);
       class != Nil; 
       class = SD_SUPERCLASS (class))
    {
      size_t classWidth = max_class_varname_length (class);

      if (classWidth > maxWidth)
        maxWidth = classWidth;

      [classList addFirst: (id) class];
    }
  
  widgets = [List create: getZone (self)];
  previous = nil;
  index = [classList begin: getZone (self)];
  while ((class = (Class) [index next]))
    {
      classWidget = 
        [[[[[[[ClassDisplayWidget createBegin: getZone (self)]
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
  if (previous)
    {
      [previous setMySubclass: nil];
      [previous setOwner: self];
      previous = [previous createEnd];
      [widgets addLast: previous];
      [previous pack];
    }
  else
    abort ();
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
  
  index = [widgets begin: getZone (self)];
  while ((aWidget = [index next]))
    if ((s_height = [aWidget getStepHeight]))
      return s_height;
  
  return 20; // If all else fails, here is a default value...    
}

- (void)update 
{
  id index;
  id a_widget;
  
  index = [widgets begin: getZone (self)];
  while ((a_widget = [index next]) != nil)
   if (!markedForDropFlag)
     [a_widget update];
   else
     break;
  [index drop];
}

- (void)drop
{
  id index, a_widget;

  // If drops all go through markForDrop, then the disable there
  // should take care of it.
  // [topLevel disableDestroyNotification];

  index = [widgets begin: getZone (self)];
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

