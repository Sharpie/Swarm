// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <stdio.h>
#include <string.h>

#import <swarmobject/ProbeMap.h>
#import <objc/objc-api.h>

#import "local.h"

@implementation ProbeMap

-setProbedClass: (Class) aClass {
  if (SAFEPROBES) {
    if (probedClass != 0) {
      fprintf(stderr, "It is an error to reset the class\n");
      return nil;
    }
  }
  probedClass = aClass;
  return self;
}

-(Class) getProbedClass {
  return probedClass;
}

-_copyCreateEnd_ {

  if (SAFEPROBES) {
    if (probedClass == 0) {
      fprintf(stderr, "ProbeMap object was not properly initialized\n");
      return nil;
    }
  }
  
  numEntries = 0 ;

  probes = [Map createBegin: [self getZone]] ;
  [probes setCompareFunction: &p_compare] ;
  probes = [probes createEnd] ;
	
  if (probes == nil)
    return nil;

  return self;
}

-createEnd {
  IvarList_t ivarList;
  MethodList_t methodList;
                      //The compiler seems to put the methods in the 
  id inversionList ;  //opposite order than the one in which they were
                      //declared, so we need to manually invert them.
  id index ;

  int i;
  id a_probe ;


  if (SAFEPROBES) {
    if (probedClass == 0) {
      fprintf(stderr, "ProbeMap object was not properly initialized\n");
      return nil;
    }
  }

  probes = [Map createBegin: [self getZone]] ;
  [probes setCompareFunction: &p_compare] ;
  probes = [probes createEnd] ;

  if (probes == nil)
    return nil;

  if(!(ivarList = probedClass->ivars))
    numEntries = 0 ;
  else{
    numEntries = ivarList->ivar_count;

    for (i = 0; i < numEntries; i++) {
      char *name;

      name = (char *) ivarList->ivar_list[i].ivar_name;
      
      a_probe = [VarProbe createBegin: [self getZone]];
      [a_probe setProbedClass: probedClass];
      [a_probe setProbedVariable: name];
      a_probe = [a_probe createEnd];

      [probes at: [String create: [self getZone] setC: name] insert: a_probe] ;
    }
  }

  if((methodList = probedClass->methods)){
    numEntries += methodList->method_count;

    inversionList = [List create: [self getZone]] ;

    for (i = 0; i < methodList->method_count; i++) {
      a_probe = [MessageProbe createBegin: [self getZone]];
      [a_probe setProbedClass: probedClass];
      [a_probe setProbedSelector: methodList->method_list[i].method_name];
      a_probe = [a_probe createEnd];

      if(a_probe)
        [inversionList addFirst: a_probe] ;
      else
        numEntries-- ;
    }
    
    index = [inversionList begin: [self getZone]] ;
    while( (a_probe = [index next]) ){
      [probes 
         at: 
             [String 
                create: [self getZone] 
                setC: (char *) [a_probe getProbedMessage]] 
         insert: 
             a_probe] ;
      [index remove] ;
    }	
    [index drop] ;
    [inversionList drop] ;
  }

  return self;
}

-clone: aZone {
  ProbeMap *npm ;
  id index ;
  id a_probe ;
	
  npm = [ProbeMap createBegin: aZone] ;
  [npm setProbedClass: probedClass] ;
  npm =	[npm _copyCreateEnd_] ;

  index = [self begin: aZone] ;

  while( (a_probe = [index next]) != nil )
    [npm _fastAddProbe_: [a_probe clone: aZone]] ;

    [index drop];

  return npm ;
}

-(int) getNumEntries {
  return numEntries;
}

-addProbeMap: (ProbeMap *) aProbeMap {

  Class aClass ;
  Class class ;
  id index ;
  id a_probe ;
	
  aClass = [aProbeMap getProbedClass] ;

  for (class = probedClass; class!=Nil; class = class_get_super_class(class))
    if (class==aClass){
      index = [aProbeMap begin: globalZone] ;
        while( (a_probe = [index next]) != nil )
	  [self _fastAddProbe_: a_probe] ;
      [index drop];
      return self ;
    }

  fprintf(stderr,"ProbeMap not added because %s is not a superclass of %s\n",
    aClass->name, probedClass->name) ;
  return self ;
}

-addProbe: aProbe {

  id roger_string ;
  Class aClass ;
  Class class ;
	
  if([aProbe isKindOf: [VarProbe class]])
    roger_string = [String create: [self getZone]
                    setC: [aProbe getProbedVariable]] ;
  else	
    roger_string = [String create: [self getZone]
                    setC: strdup([aProbe getProbedMessage])] ;

  if([probes at: roger_string] != nil)
    fprintf(stderr,"addProbe: There was already a probe for %s!!!\n",
            [roger_string getC]) ;

  aClass = [aProbe getProbedClass] ;

  for (class = probedClass; class!=Nil; class = class_get_super_class(class))
    if (class==aClass){
      [probes at: roger_string insert: aProbe] ;
      numEntries++ ;
      return self ;
    }

  fprintf(stderr,"Probe not added to ProbeMap because %s is not a superclass of %s\n",aClass->name, probedClass->name) ;

  return self ;
}


// Here, we don't check that the probe is of an appropriate class...
// This method is used by addProbeMap and clone where the check is done
// for all the probes within the candidate ProbeMap.
//
// Note: In practice, it is probably unnecessary to check for duplicate
//       inclusion...

-_fastAddProbe_: aProbe {

  id roger_string ;

  if([aProbe isKindOf: [VarProbe class]])
    roger_string = [String create: [self getZone]
                     setC: [aProbe getProbedVariable]] ;
  else
    roger_string = [String create: [self getZone]
                     setC: strdup([aProbe getProbedMessage])] ;

  if([probes at: roger_string] != nil)
    fprintf(stderr,"addProbe: There was already a probe for %s!!!\n",
            [roger_string getC]) ;

  [probes at: roger_string insert: aProbe] ;
  numEntries++ ;

  return self ;
}


// Note: the candidate ProbeMap for removal does not have to contain the
// *same* class in order to cause removal... Only probes with the same
// name!
//
// [We do not check that the classes are appropriate because the
// user may want to subtract commonly named methods from unrelated
// classes!!!]

-dropProbeMap: (ProbeMap *) aProbeMap {

  id index ;
  id a_probe ;
			
  index = [aProbeMap begin: globalZone] ;

  while( (a_probe = [index next]) != nil )
    if([a_probe isKindOf: [VarProbe class]])
      [self dropProbeForVariable: [a_probe getProbedVariable]] ;
    else
      [self dropProbeForMessage: strdup([a_probe getProbedMessage])] ;

  [index drop];
	
  return self ;
}

-dropProbeForVariable: (char *) aVariable {
  id roger_string ;
  
  roger_string = [String create: [self getZone] setC: aVariable] ;
  if([probes removeKey: roger_string] != nil)
    numEntries-- ;
  [roger_string drop] ;
  
  return self ;
}

-(Probe *) getProbeForVariable: (char *) aVariable {
  id roger_string ;
  id res ;
	
  roger_string = [String create: [self getZone] setC: aVariable] ;

  res = [probes at: roger_string] ;
  [roger_string drop] ;

  if (res == nil) {			  // if not found
    if (SAFEPROBES)
      fprintf(stderr, "Warning: the variable %s was not found\n",aVariable);
    return nil;					  // return nil
  } else
    return res ;
}

-dropProbeForMessage: (char *) aMessage {
  id roger_string ;
  
  roger_string = [String create: [self getZone] setC: aMessage] ;
  if([probes removeKey: roger_string] != nil)
    numEntries-- ;
  [roger_string drop] ;
  
  return self ;
}

-(Probe *) getProbeForMessage: (char *) aMessage {
  id roger_string ;
  id res ;
	
  roger_string = [String create: [self getZone] setC: aMessage] ;

  res = [probes at: roger_string] ;
  [roger_string drop] ;

  if (res == nil) {			  // if not found
    if (SAFEPROBES)
      fprintf(stderr, "Warning: the message %s was not found\n",aMessage);
    return nil;					  // return nil
  } else
    return res ;
}

-begin: aZone {
	return [probes begin: aZone] ;
}

@end
