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

#import <objectbase/Probe.h>
#import <collections.h>
#import <defobj.h> // Warning
#import "local.h"

#include <defobj/directory.h>
#include "../defobj/COM.h"

#include <misc.h> // isDigit, atoi, atof

@implementation Probe
PHASE(Creating)

+  createBegin: aZone 
{
  Probe *obj = [super createBegin: aZone];

  obj->objectToNotify = nil;
  obj->language = LanguageObjc;
  return obj;
}

- createEnd
{
  [super createEnd];
  //return full description by default...
  stringReturnType = DefaultString;
  
  //by default we do not want to bother checking the class is valid
  //each time the probe is used...
  safety = 0;
  
  return self;
}

PHASE(Setting)
- setSafety
{
  safety = YES;
  return self;
}

PHASE(Using)

- setObjectToNotify: anObject
{
#if 0
  if (anObject != nil
      && (![anObject
             respondsTo:
               M(eventOccurredOn:via:withProbeType:on:ofType:withData:)])
      && (![anObject respondsTo: M(forEach:)]))
    raiseEvent (NotImplemented,
                "Object %0#p of class %s does not implement\n"
                "standard probe hook message.\n", 
                anObject,
                [[anObject class] name]);
#endif
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          if ([anObject respondsTo: M(forEach:)])
            {
              // put all the objects on the ProbeMap's list 
              // at the time when we're created onto our list
              id index, tempObj;
              index = [anObject begin: scratchZone];
              while ( (tempObj = [index next]) != nil ) {
                if (([objectToNotify contains: tempObj]) == NO)
                  [objectToNotify addLast: tempObj];
              }
              [index drop];
            }
          else
            if (([objectToNotify contains: anObject]) == NO)
              [objectToNotify addLast: anObject];
        }
      else
        {  // objectToNotify is not a list
          id temp;
          temp = objectToNotify;
          objectToNotify = [List create: [self getZone]];
          [objectToNotify addLast: temp];
          if ([objectToNotify contains: anObject] == NO)
            [objectToNotify addLast: anObject];
        }
    }
  else
    objectToNotify = anObject;
  
  return self;
}

- getObjectToNotify
{
  return objectToNotify;
}

- setProbedClass: (Class)aClass
{
#ifdef HAVE_JDK
  // if class passed to setProbedClass is 
  if ([aClass respondsTo: M(isJavaProxy)])
    language = LanguageJava;
#endif

  probedClass = aClass;
  return self;
}

- setProbedObject: anObject
{
  COMobject cObj = SD_COM_FIND_OBJECT_COM (anObject);

  probedObject = anObject;
  
  if (cObj && COM_is_javascript (cObj))
    {
      probedClass = Nil;
      language = LanguageJS;
    }
  else
    [self setProbedClass: SD_GETCLASS (anObject)];
  return self;
}


- (Class)getProbedClass
{
  return probedClass;
}

- (const char *)getProbedType
{
  return probedType;
}

- unsetSafety
{
  safety = NO;
  return self;
}

- clone: aZone
{
  [self subclassResponsibility: @selector(clone)];
  return self;
}

- (val_t)guessValue: (const char *)str
{
  val_t val;

  if (strcmp (str, "false") == 0)
    {
      val.type = fcall_type_boolean;
      val.val.boolean = NO;
    }
  else if (strcmp (str, "true") == 0)
    {
      val.type = fcall_type_boolean;
      val.val.boolean = YES;
    }
  else
    {
      BOOL nonValue = NO, hasDecimal = NO;
      size_t i;
      char ch;
      
      for (i = 0; (ch = str[i]); i++)
        {
          if (ch == '.')
            if (hasDecimal)
              {
                nonValue = YES;
                break;
              }
            else
              hasDecimal = YES;
          else if (!isDigit (ch) && !(ch == '-' && i == 0))
            {
              nonValue = YES;
              break;
            }
        }
      if (nonValue)
        {
          val.type = fcall_type_string;
          val.val.string = str;
        }
      else if (hasDecimal)
        {
          val.type = fcall_type_double; 
          val.val._double = atof (str);
        }
      else
        {
          val.type = fcall_type_sint;
          val.val.sint = atoi (str);
        }
    }
  return val;
}

- (void)drop
{
  if (probedType)
    ZFREEBLOCK (globalZone, probedType);
  [super drop];
}

@end
