// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <stdio.h>
#import <objectbase.h>
#import <simtools/ObjectLoader.h>
#import <simtools/InFile.h>

//S: A class to load an object's instance variables from a file.
//D: This class is used to initialize the variables of a target object from
//D: a data file. The data file is required to have a very simple format.
@implementation ObjectLoader

+createBegin: aZone {
  ObjectLoader * anObj ;

  anObj = [super createBegin: aZone] ;

                                  //For some reason I still don't trust 
  anObj->probeMapCache = nil ;    //that the var will be automatically 
                                  //initialised to the right value, so 
                                  //I do it myself!

  return anObj ;
}

//M: The load:from: method loads anObject from the previously opened 
//M: aFileObject without returning an actual instance of the ObjectLoader 
//M: class.  The FileObject remains open after the method has been called. 
+load: anObject from: aFileObject {
  id anObj ;

  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj loadObject: anObject] ;
  [anObj drop] ;
  return self ;
}

//M: The load:fromFileNamed: method loads anObject from the file named 
//M: aFileName.  The ObjectLoader class will open the file, initialize the 
//M: object with its contents and then close the file.
+load: anObject fromFileNamed: (const char *)aFileName
{
  id anObj ;
  id aFileObject ;

  aFileObject = [InFile create: [anObject getZone] withName: aFileName] ;

  if(!aFileObject)
    [self _crash_: anObject] ;
      
  anObj = [self create: [aFileObject getZone]] ;
  [anObj setFileObject: aFileObject] ;
  [anObj loadObject: anObject] ;
  [anObj drop] ;

  [aFileObject drop] ;

  return self ;
}

+(void) _crash_: anObject {
  if([anObject respondsTo: M(getInstanceName)])
    fprintf(stderr,"Could not initialize %s properly...\n",
               [anObject getInstanceName]) ;
  else
    fprintf(stderr,"Could not initialize %s properly...\n",
               [anObject name]) ;
  exit(-1) ;
}

-(void) _crash_: anObject {
  if([anObject respondsTo: M(getInstanceName)])
    fprintf(stderr,"Could not initialize %s properly...\n",
               [anObject getInstanceName]) ;
  else
    fprintf(stderr,"Could not initialize %s properly...\n",
               [anObject name]) ;
  exit(-1) ;
}

//M: The setFileObject: method sets the source fileObject which the instance 
//M: of the ObjectLoader class should use by sending it this message.
-setFileObject: aFileObject {
  theFileObject = aFileObject ;
  return self ;
}

//M: The loadObject: message must be sent to an instance of the ObjectLoader 
//M: class in order to initialize the target object from the requested file.
- loadObject: anObject
{
  id aProbeMap, aProbe ;
  char aString[200], aChar ;

  if(probeMapCache)
    aProbeMap = probeMapCache ;
  else
    aProbeMap = [probeLibrary getCompleteVarMapFor: [anObject class]] ;

  //find the @begin...

  while(1){

    while(1){
      if(!([theFileObject getChar: &aChar]))
        [self _crash_: anObject] ;

      if(aChar == '#'){
        if(![theFileObject skipLine])
          [self _crash_: anObject] ;
      } else {
        if(![theFileObject unGetChar: aChar])
          [self _crash_: anObject] ;
        break ;
      }
    }

    if(![theFileObject getWord: aString])
      [self _crash_: anObject] ;
   
    if( (aString[0] == '@') && 
        (aString[1] == 'b') &&
        (aString[2] == 'e') &&
        (aString[3] == 'g') &&
        (aString[4] == 'i') &&
        (aString[5] == 'n') ) {
      if(![theFileObject skipLine])
        [self _crash_: anObject] ;   
      break ;
    }
  }

  while(1){
   
    while(1){ 

      if(!([theFileObject getChar: &aChar]))
        [self _crash_: anObject] ;

      if(aChar == '#'){
        if(![theFileObject skipLine])
          [self _crash_: anObject] ;
      } else {
        if(![theFileObject unGetChar: aChar])
          [self _crash_: anObject] ;
        break ;
      }
    }

    if(![theFileObject getWord: aString])
      [self _crash_: anObject] ;
       
    // Check for single quotes that surround the alphanumeric representation
    // of unsigned char variables.
    if (aString[0] == '\'') {
      [theFileObject skipLine];
      if(![theFileObject getWord: aString])
	[self _crash_: anObject];
    }


    if( (aString[0] == '@') && 
        (aString[1] == 'e') &&
        (aString[2] == 'n') &&
        (aString[3] == 'd') ) {
      [theFileObject skipLine] ; //Note: if this fails we don't care...
                                 //(users may or may not have a newline
                                 //character at the end of their last 
                                 //object specification (last line in file).   
      break ;
    }

    aProbe = [aProbeMap getProbeForVariable: aString] ; 
    if( !aProbe )
      [self _crash_: anObject] ;

    if(![theFileObject getLine: aString])
      [self _crash_: anObject] ;
       
    if(![aProbe setData: anObject ToString: aString])
      [self _crash_: anObject] ;

    if(![theFileObject skipLine])
      [self _crash_: anObject] ;   
  }

  return self ;  
}

//M: The setTemplateProbeMap: method is used to specify which variables of the
//M: target object(s) should be loaded by the ObjectLoader instance to which
//M: this message was sent.
- setTemplateProbeMap: probeMap
{
  probeMapCache = probeMap;
  return self;
}

//M: The updateCache: method should be called if an ObjectLoader instance is 
//M: going to initialize a large number of objects from the same class.
-updateCache: exampleTarget {

  if(!exampleTarget)
    probeMapCache = nil ;
  else 
    probeMapCache = [probeLibrary getCompleteVarMapFor: 
                       [exampleTarget class]] ;

  return self ;
}

@end
