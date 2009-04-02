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

#import <objectbase/CompleteVarMap.h>
#import <collections.h>
#import <defobj/swarm-objc-api.h>
#import <defobj.h> // Warning
#import <defobj/defalloc.h> // getZone

#include <swarmconfig.h>
#ifdef HAVE_JDK
#import <defobj/JavaProxy.h>
#import "../defobj/java.h" // SD_JAVA_FIND_CLASS_JAVA
#endif

#import "local.h"

@implementation CompleteVarMap

PHASE(Creating)
- createEnd
{
  Class aClass;
  id classList;  // added to ensure the vars are added from Object downwards
  id anIndex;    // as required by the ObjectSaver (for example)
	
  if (SAFEPROBES)
    if (!probedClass)
      {
        raiseEvent (WarningMessage,
                    "CompleteVarMap object was not properly initialized\n");
        return nil;
      }

  probes = [Map createBegin: getZone (self)];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
	
#ifdef HAVE_JDK
  if ([probedClass respondsTo: M(isJavaProxy)])
    { 
      jclass currentClass, nextClass;
      jclass classObject;

      classObject = SD_JAVA_FIND_CLASS_JAVA (probedClass);

      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      [self addJavaFields: classObject];

      count = 0;
      for (currentClass = (*jniEnv)->GetSuperclass (jniEnv, classObject);
           currentClass;
           nextClass = (*jniEnv)->GetSuperclass (jniEnv, currentClass),
             (*jniEnv)->DeleteLocalRef (jniEnv, currentClass),
             currentClass = nextClass)
        [self addJavaFields: currentClass];
      return self;
    }
#endif

  classList = [List create: getZone (self)];

  count = 0;

  aClass = probedClass;
  do {
    [classList addFirst: (id) aClass];
    aClass = swarm_class_getSuperclass(aClass);
  } while (aClass);
  
  anIndex = [classList begin: getZone (self)];
  while ((aClass = (id) [anIndex next]))
    [self addObjcFields: aClass];
  [anIndex drop];
  [classList drop];
  return self;
}

PHASE(Using)
@end

