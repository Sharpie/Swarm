// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <stdio.h>
#include <string.h>

#import <objectbase/ProbeMap.h>
#import <objc/objc-api.h>

#import "local.h"

//S: A container class for Probes used to specify the contents of a 
//S: ProbeDisplay.
//D: A ProbeMap is a Map-type collection of Probes. They are used to gather 
//D: several Probes, who usually have a common referent, into a single
//D: bundle. For example, all the instance variables of a ModelSwarm might be 
//D: gathered into a single ProbeMap. Each ProbeMap is then installed
//D: into the global ProbeLibrary. 
@implementation ProbeMap

+ createBegin: aZone
{
  ProbeMap *tempObj;

  tempObj = [super createBegin: aZone];
  tempObj->objectToNotify = nil;
  return tempObj;
}

- setObjectToNotify: anObject
{
  id temp_otn;

  if (anObject != nil
      && ([anObject 
            respondsTo:
              M(eventOccurredOn:via:withProbeType:on:ofType:withData:)] == NO)
      && ![anObject respondsTo: M(forEach:)])
    raiseEvent (NotImplemented, "Object %0#p of class %s does not implement "
                "standard probe hook message.\n", 
                anObject, [[anObject class] name]);
  
  
  // this is pretty ugly, if you set more than one thing to 
  // notify, you'll be invoking this code more than once with
  // possibly no effect.
  
  // inherit the probeLibrary's otn, which should NOT be a list
  if (objectToNotify != nil) {
    if ((temp_otn = [probeLibrary getObjectToNotify]) != nil)
      {
        if ([objectToNotify respondsTo: M(forEach:)])
          {
            if ([temp_otn respondsTo: M(forEach:)])
              {
                // both exist and both are lists
                // add contents of temp_otn to otn
                id index, tempObj;
                index = [temp_otn begin: scratchZone];
                while ( (tempObj = [index next]) != nil)
                  {
                    if (([objectToNotify contains: tempObj]) == NO)
                      [objectToNotify addLast: tempObj];
                  }
                [index drop];
              }
            else
              {
                // both exist, otn is list temp_otn not list
                // add temp_otn to otn
                if (([objectToNotify contains: temp_otn]) == NO)
                  [objectToNotify addLast: temp_otn];
              }
          }
        else if ([temp_otn respondsTo: M(forEach:)])
          {
            // both exist, otn is not list temp_otn is list
            // add otn to front of temp_otn and swap
            id tempObj;
            tempObj = objectToNotify;
            objectToNotify = temp_otn;
            if ([objectToNotify contains: tempObj] == NO)
              [objectToNotify addFirst: tempObj];
          }
      }
    //else clause => otn exists, temp_otn does not, so do nothing
  }
  else if ((temp_otn = [probeLibrary getObjectToNotify]) != nil)
    objectToNotify = temp_otn;
  //else clause => neither exist, so do nothing
  
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
              while ( (tempObj = [index next]) != nil )
                {
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
          if (([objectToNotify contains: anObject]) == NO) 
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

//M: The setProbedClass: method sets the class of the object that the set of 
//M: probes that constitute the probe map points at. This message must be sent 
//M: before createEnd. 
- setProbedClass: (Class)aClass
{
  if (SAFEPROBES)
    {
      if (probedClass != 0)
        {
          fprintf (stderr, "It is an error to reset the class\n");
          return nil;
        }
    }
  probedClass = aClass;
  return self;
}

//M: The getProbedClass method returns the class of the object that the set of 
//M: probes that constitute the probe map points at.
- (Class)getProbedClass
{
  return probedClass;
}

- _copyCreateEnd_
{
  if (SAFEPROBES)
    {
      if (probedClass == 0)
        {
          fprintf(stderr, "ProbeMap object was not properly initialized\n");
          return nil;
        }
    }
  
  numEntries = 0;
  
  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;
  
  return self;
}

- createEnd
{
  IvarList_t ivarList;
  MethodList_t methodList;
  //The compiler seems to put the methods in the 
  //opposite order than the one in which they were
  //declared, so we need to manually invert them.
  id inversionList; 
  id index;
  
  int i;
  id a_probe;
  
  if (SAFEPROBES)
    {
      if (probedClass == 0)
        {
          fprintf(stderr, "ProbeMap object was not properly initialized\n");
          return nil;
        }
    }
  
  if (objectToNotify == nil)
    [self setObjectToNotify: 
            [probeLibrary getObjectToNotify]];
  
  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;
  
  if (!(ivarList = probedClass->ivars))
    numEntries = 0;
  else
    {
      numEntries = ivarList->ivar_count;
      
      for (i = 0; i < numEntries; i++)
        {
          const char *name;
          
          name = ivarList->ivar_list[i].ivar_name;
          
          a_probe = [VarProbe createBegin: [self getZone]];
          [a_probe setProbedClass: probedClass];
          [a_probe setProbedVariable: name];
          if (objectToNotify != nil) 
            [a_probe setObjectToNotify: objectToNotify];
          a_probe = [a_probe createEnd];
          
          [probes at: [String create: [self getZone] setC: name]
                  insert: a_probe];
        }
    }
  
  if ((methodList = probedClass->methods))
    {
      numEntries += methodList->method_count;
      
      inversionList = [List create: [self getZone]];
      
      for (i = 0; i < methodList->method_count; i++)
        {
          a_probe = [MessageProbe createBegin: [self getZone]];
          [a_probe setProbedClass: probedClass];
          [a_probe setProbedSelector: methodList->method_list[i].method_name];
          if (objectToNotify != nil) 
            [a_probe setObjectToNotify: objectToNotify];
          a_probe = [a_probe createEnd];
          
          if(a_probe)
            [inversionList addFirst: a_probe];
          else
            numEntries--;
        }
      
      index = [inversionList begin: [self getZone]];
      while ((a_probe = [index next]))
        {
          [probes at: 
                    [String 
                      create: [self getZone] 
                      setC: [a_probe getProbedMessage]] 
                  insert: 
                    a_probe];
          [index remove];
        }	
      [index drop];
      [inversionList drop];
  }
  return self;
}

//M: The clone: method returns a clone of the probe map. If the initial probe
//M: map created by Library Generation or by the default version of Object 
//M: generation, the probe map should be cloned prior to making changes to it 
//M: to avoid having the changes affect the other potential users of the 
//M: probe map.
- clone: aZone
{
  ProbeMap *npm;
  id index;
  id a_probe;
  
  npm = [ProbeMap createBegin: aZone];
  [npm setProbedClass: probedClass];
  npm =	[npm _copyCreateEnd_];
  
  index = [self begin: aZone];
  
  while ((a_probe = [index next]) != nil)
    [npm _fastAddProbe_: [a_probe clone: aZone]];
  
  [index drop];
  
  return npm;
}

//M: The getNumEntries method returns the number of probes in the ProbeMap.
- (int)getNumEntries
{
  return numEntries;
}

//M: The addProbeMap: method is used to tailor the contents of a ProbeMap by
//M: performing "set inclusion" with another ProbeMap. The typing is verified 
//M: prior to inclusion.
- addProbeMap: (ProbeMap *)aProbeMap
{
  Class aClass;
  Class class;
  id index;
  id a_probe;
	
  aClass = [aProbeMap getProbedClass];

  for (class = probedClass; class!=Nil; class = class_get_super_class (class))
    if (class==aClass)
      {
        index = [aProbeMap begin: globalZone];
        while ((a_probe = [index next]) != nil)
	  [self _fastAddProbe_: a_probe];
        [index drop];
        return self;
      }
  
  fprintf(stderr,"ProbeMap not added because %s is not a superclass of %s\n",
          aClass->name, probedClass->name);
  return self;
}

//M: The addProbe: method adds a probe to the contents of the ProbeMap.
//M: The ProbeMap will always make sure that the probedClass of the Probe being
//M: added corresponds to its own probedClass.
- addProbe: aProbe
{
  id string;
  Class aClass;
  Class class;
	
  if([aProbe isKindOf: [VarProbe class]])
    string = [String create: [self getZone]
                           setC: [aProbe getProbedVariable]];
  else	
    string = [String create: [self getZone]
                           setC: strdup ([aProbe getProbedMessage])];
  
  if ([probes at: string] != nil)
    fprintf (stderr,"addProbe: There was already a probe for %s!!!\n",
             [string getC]);
  
  aClass = [aProbe getProbedClass];
  
  for (class = probedClass;
       class != Nil;
       class = class_get_super_class (class))
    if (class == aClass)
      {
        [probes at: string insert: aProbe];
        numEntries++;
        if (objectToNotify != nil) 
          [aProbe setObjectToNotify: objectToNotify];
        return self;
      }
  
  fprintf (stderr, "Probe not added to ProbeMap because %s is not a superclass of %s\n", aClass->name, probedClass->name);
  
  return self;
}


// Here, we don't check that the probe is of an appropriate class...
// This method is used by addProbeMap and clone where the check is done
// for all the probes within the candidate ProbeMap.
//
// Note: In practice, it is probably unnecessary to check for duplicate
//       inclusion...

- _fastAddProbe_: aProbe
{

  id string;

  if([aProbe isKindOf: [VarProbe class]])
    string = [String create: [self getZone]
                     setC: [aProbe getProbedVariable]];
  else
    string = [String create: [self getZone]
                     setC: strdup([aProbe getProbedMessage])];

  if ([probes at: string] != nil)
    fprintf (stderr,"addProbe: There was already a probe for %s!!!\n",
            [string getC]);

  [probes at: string insert: aProbe];
  numEntries++;

  if (objectToNotify != nil) 
    [aProbe setObjectToNotify: objectToNotify];

  return self;
}


// Note: the candidate ProbeMap for removal does not have to contain the
// *same* class in order to cause removal... Only probes with the same
// name!
//
// [We do not check that the classes are appropriate because the
// user may want to subtract commonly named methods from unrelated
// classes!!!]
//M: The dropProbeMap: method is used to drop a probe from a probe map. It is
//M: equivalent to callling dropProbeForVariable for each variable name present
//M: in the ProbeMap being dropped, followed by a call to dropProbeForMessage
//M: for each message name present in the ProbeMap being dropped.
- dropProbeMap: (ProbeMap *) aProbeMap
{
  id index;
  id a_probe;
			
  index = [aProbeMap begin: globalZone];

  while ((a_probe = [index next]) != nil)
    if ([a_probe isKindOf: [VarProbe class]])
      [self dropProbeForVariable: [a_probe getProbedVariable]];
    else
      [self dropProbeForMessage: strdup([a_probe getProbedMessage])];
  
  [index drop];
	
  return self;
}

//M: The dropProbeForVariable: method is used to drop a Probe from the 
//M: ProbeMap. No class verification takes place since the probe is dropped
//M: based on its variableName, not its actual id value.
- dropProbeForVariable: (const char *)aVariable
{
  id string;
  
  string = [String create: [self getZone] setC: aVariable];
  if([probes removeKey: string] != nil)
    numEntries--;
  [string drop];
  
  return self;
}

//M: The getProbeForVariable: method returns the Probe corresponding to the 
//M: given variable name.
- (Probe *)getProbeForVariable: (const char *)aVariable
{
  id string;
  id res;
  
  string = [String create: [self getZone] setC: aVariable];
  
  res = [probes at: string];
  [string drop];
  
  if (res == nil)
    { 
      // if not found
      if (SAFEPROBES)
        fprintf (stderr, "Warning: the variable %s was not found\n",aVariable);
      return nil;
    }
  else
    return res;
}

//M: The dropProbeForMessage: method is used to drop a Probe from the ProbeMap.
//M: No class verification takes place since the probe is dropped based on its
//M: messageName, not its actual id value.
- dropProbeForMessage: (const char *)aMessage
{
  id string;
  
  string = [String create: [self getZone] setC: aMessage];
  if([probes removeKey: string] != nil)
    numEntries--;
  [string drop];
  
  return self;
}

//M: The getProbeForMessage: method returns the Probe corresponding to the
//M: specified message name.
- (Probe *)getProbeForMessage: (const char *)aMessage
{
  id string;
  id res;
  
  string = [String create: [self getZone] setC: aMessage];

  res = [probes at: string];
  [string drop];

  if (res == nil)
    {
      if (SAFEPROBES)
        fprintf(stderr, "Warning: the message %s was not found\n",aMessage);
      return nil;
    }
  else
    return res;
}

//M: The begin: method returns an iterator (index) over the ProbeMap. This 
//M: index is used in the exact same way any Map index is used. 
-begin: aZone
{
  return [probes begin: aZone];
}

@end
