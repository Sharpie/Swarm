// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <stdio.h>

#import <objectbase/CompleteProbeMap.h>
#import <collections.h>
#import <objc/objc-api.h>

#import "local.h"

@implementation CompleteProbeMap

- createEnd
{
  IvarList_t ivarList;
  MethodList_t methodList;
  int i;
  id a_probe;
  Class a_class;

  id classList;  //added to ensure the vars are added from Object downwards
  id anIndex;    //as required by the ObjectSaver (for example).
	

  if (SAFEPROBES)
    if (probedClass == 0)
      {
        fprintf (stderr,
                 "CompleteProbeMap object was not properly initialized\n");
        return nil;
      }
  
  probes = [Map createBegin: [self getZone]];
  [probes setCompareFunction: &p_compare];
  probes = [probes createEnd];
  
  if (probes == nil)
    return nil;

  classList = [List create: [self getZone]];
  if (!classList) 
    return nil;

  numEntries = 0;

  a_class = probedClass;
  do
    {
      [classList addFirst: (id) a_class];
      a_class = a_class->super_class;
    } 
  while(a_class);
  
  anIndex = [classList begin: [self getZone]];
  while ((a_class = (id) [anIndex next]))
    {
      if ((ivarList = a_class->ivars))
        {
          numEntries += ivarList->ivar_count;
          
          for (i = 0; i < ivarList->ivar_count; i++)
            {
              const char *name = ivarList->ivar_list[i].ivar_name;
              
              a_probe = [VarProbe createBegin: [self getZone]];
              [a_probe setProbedClass: a_class];
              [a_probe setProbedVariable: name];
              if (objectToNotify != nil) 
                [a_probe setObjectToNotify: objectToNotify];
              a_probe = [a_probe createEnd];
              
              [probes at: [String create: [self getZone] setC: name]
                      insert: a_probe];
            }
        }
      
      if ((methodList = a_class->methods))
        {
          numEntries += methodList->method_count;
          
          for (i = 0; i < methodList->method_count; i++)
            {
              a_probe = [MessageProbe createBegin: [self getZone]];
              [a_probe setProbedClass: probedClass];
              [a_probe setProbedSelector: methodList->method_list[i].method_name];
              if (objectToNotify != nil) 
                [a_probe setObjectToNotify: objectToNotify];
              a_probe = [a_probe createEnd];
              
              if (a_probe)
                [probes 
                  at: 
                    [String 
                      create: [self getZone] 
                      setC: 
                        sel_get_name (methodList->method_list[i].method_name)]
                  insert: a_probe];
            }
        }
    }
  [anIndex drop];
  [classList drop];

  return self;
}

@end

