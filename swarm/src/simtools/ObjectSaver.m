// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <simtools/ObjectSaver.h>
#import <simtools/OutFile.h>

//S: A class to save an object's instance variables to a file.
//D: This class is used to write an object's variables to a specified file. 
//D: If only a subset of the variables should be written out, the set is 
//D: specified by a template ProbeMap (where the ProbeMap will contain Probes 
//D: for those variables which should be saved). 
@implementation ObjectSaver

+createBegin: aZone {
  ObjectSaver * anObj ;

  anObj = [super createBegin: aZone] ;

                                     //For some reason I still don't trust 
  anObj->templateProbeMap = nil ;    //that the var will be automatically 
                                     //initialised to the right value, so 
                                     //I do it myself!

  return anObj ;
}

//M: The save:to: method saves the entire target object without actually 
//M: returning an instance of ObjectSaver to the user.
+save: anObject to: aFileObject {
  id anObj ;

  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj saveObject: anObject] ;
  [anObj drop] ;
  return self ;
}

//M: The save:to:withTemplate: method saves the subset of target object 
//M: variables specified in a template from anObject without actually 
//M: returning an instance of ObjectSaver to the user.
+save: anObject to: aFileObject withTemplate: aProbeMap{
  id anObj ;

  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj setTemplateProbeMap: aProbeMap] ;
  [anObj saveObject: anObject] ;
  [anObj drop] ;
  return self ;
}

//M: The save:toFileNamed: method saves the entire target object to the file
//M: aFileName.
+save: anObject toFileNamed: (const char *)aFileName
 {
  id anObj ;
  id aFileObject ;

  aFileObject = [OutFile create: [anObject getZone] withName: aFileName] ;

  if(!aFileObject)
    [self _crash_: aFileObject] ;
      
  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj saveObject: anObject] ;
  [anObj drop] ;
  [aFileObject drop] ;

  return self ;
}

//M: The save:toFileNamed:withTemplate: method saves the subset of variables
//M: specified in a template from the target object to the file
//M: aFileName.
+save: anObject toFileNamed: (const char *)aFileName withTemplate: aProbeMap
{
  id anObj ;
  id aFileObject ;

  aFileObject = [OutFile create: [anObject getZone] withName: aFileName] ;

  if(!aFileObject)
    [self _crash_: aFileObject] ;
      
  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj setTemplateProbeMap: aProbeMap] ;
  [anObj saveObject: anObject] ;
  [anObj drop] ;
  [aFileObject drop] ;

  return self ;
}

+(void) _crash_: anObject {
  if([anObject respondsTo: M(getInstanceName)])
    fprintf(stderr,"Could not save %s properly...\n",
               [anObject getInstanceName]) ;
  else
    fprintf(stderr,"Could not save %s properly...\n",
               [anObject name]) ;
  exit(-1) ;
}

-(void) _crash_: anObject {
  if([anObject respondsTo: M(getInstanceName)])
    fprintf(stderr,"Could not save %s properly...\n",
               [anObject getInstanceName]) ;
  else
    fprintf(stderr,"Could not save %s properly...\n",
               [anObject name]) ;
  exit(-1) ;
}

//M: The setFileObject: method sets the target fileObject which the instance 
//M: of the ObjectSaver class should use.
-setFileObject: aFileObject {
  theFileObject = aFileObject ;
  return self ;
}

//M: The saveObject: message tells an instance of the ObjectSaver class to 
//M: save the state of the target object into the requested file.
- saveObject: anObject
{
  id aProbeMap, aProbe, anIndex ;
  char aBuffer[2000] ;

  if(templateProbeMap)
    aProbeMap = templateProbeMap ;
  else
    aProbeMap = [probeLibrary getCompleteProbeMapFor: [anObject class]] ;

  //put the @begin...

  [theFileObject putString: "#Machine Generated Object-IVAR-Dump\n"] ;
  [theFileObject putString: "@begin\n"] ;

  anIndex = [aProbeMap begin: [self getZone]] ;
  while( (aProbe = [anIndex next]) )
    if([aProbe isKindOf: [VarProbe class]])
      if([aProbe isInteractive]){
        [theFileObject putString: [aProbe getProbedVariable]] ;
        [theFileObject putTab] ;
        [theFileObject putString: 
          [aProbe probeAsString: anObject Buffer: aBuffer withFullPrecision: 1]] ;
        [theFileObject putNewLine] ;
      } ;
  [anIndex drop] ;

  //put the @end...
  [theFileObject putString: "@end\n"] ;

  return self ;  
}

//M: The setTemplateProbeMap: method is used to specify which variables of the
//M: source object(s) should be saved by the ObjectSaver instance to which this
//M: message was sent.
-setTemplateProbeMap: aProbeMap {

  templateProbeMap = aProbeMap ;
  return self ;
}

@end
