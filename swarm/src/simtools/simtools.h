// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.    

/*
Name:            simtools.h
Description:     miscellaneous widgetry
Library:         simtools
*/

#import <objectbase.h>
//
// UName --
//   a class used to generate unique names (e.g. "critter1", "critter2" etc.)
//
@protocol UName <SwarmObject>
CREATING
+ create: aZone setBaseName: (const char *)aString;
+ create: aZone setBaseNameObject: aStringObject;

- setBaseName: (const char *)aString;
- setBaseNameObject: aStringObject;
USING
- (const char *)getNewName;
- getNewNameObject;
@end

//
// InFile --
//   a class which was intended to support file input. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol InFile <SwarmObject>
CREATING
+ create: aZone withName: (const char *)theName;
USING
- (int)getWord: (char *)aWord;
- (int)getLine: (char *)aLine;
- (int)getInt: (int *)anInt;
- (int)getUnsigned: (unsigned *)anUnsigned;
- (int)getLong: (long *)aLong;
- (int)getUnsignedLong: (unsigned long *)anUnsLong;
- (int)getDouble: (double *)aDouble;
- (int)getFloat: (float *)aFloat;
- (int)getChar: (char *)aChar;
- (int)unGetChar: (char)aChar;
- (int)skipLine;
@end

//
// OutFile --
//   a class which was intended to support file output. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol OutFile <SwarmObject>
CREATING
+ create: aZone withName: (const char *)theName;
USING
- putString: (const char *)aString;
- putInt: (int) anInt;
- putUnsigned: (unsigned)anUnsigned;
- putLong: (long)aLong;
- putUnsignedLong: (unsigned long)anUnsLong;
- putDouble: (double)aDouble;
- putFloat: (float)aFloat;
- putChar: (char)aChar;
- putTab;
- putNewLine;
@end

//
// AppendFile --
//   a class which was intended to support (appended) file output. There have been 
//   justified requests from our userbase to re-design this interface.
//
@protocol AppendFile <SwarmObject>
CREATING
+ create: aZone withName: (const char *)theName;
USING
- putString: (const char *)aString;
- putInt: (int)anInt;
- putUnsigned: (unsigned)anUnsigned;
- putLong: (long)aLong;
- putUnsignedLong: (unsigned long)anUnsLong;
- putDouble: (double)aDouble;
- putFloat: (float)aFloat;
- putChar: (char)aChar;
- putTab;
- putNewLine;
@end

//
// ObjectLoader --
//   a particularly bad attempt to design some form of standard for object
//   loading 
//
@protocol ObjectLoader <SwarmObject>
USING
+ load: anObject from: aFileObject;
+ load: anObject fromFileNamed: (const char *) aFileName;

- setFileObject: aFileObject;
- loadObject: anObject;
// In case the same class is being loaded multiple times...
- updateCache: exampleTarget; 
@end

//
// ObjectSaver --
//   a particularly bad attempt to design some form of standard for object
//   saving 
//   The ProbeMap argument is used to specify what variables get saved.
//
@protocol ObjectSaver <SwarmObject>
USING
+ save: anObject to: aFileObject;
+ save: anObject to: aFileObject withTemplate: aProbeMap;
+ save: anObject toFileNamed: (const char *)aFileName;
+ save: anObject toFileNamed: (const char *)aFileName 
                withTemplate: (ProbeMap *)aProbeMap;

- setFileObject: aFileObject;
- setTemplateProbeMap: aProbeMap;
- saveObject: anObject;
@end

//
// QSort --
//   a class (not to be instantiated) wrapper around the C sort routine.
//
@protocol QSort <SwarmObject>
USING
+ (void)sortObjectsIn: aCollection;
+ (void)sortObjectsIn: aCollection using: (SEL) aSelector;
+ (void)sortNumbersIn: aCollection;
+ (void)sortNumbersIn: aCollection
                using: (int(*)(const void*,const void*)) comp_fun;
+ (void)reverseOrderOf: aCollection;
@end

//
// NSelect --
//   a class (not to be instantiated) wrapper around a Knuth algorithm
//   for the selection of exactly N elements form a collection without
//   repetition.
//
@protocol NSelect <SwarmObject>
USING
+ (void)select: (int)n from: aCollection into: bCollection;
@end

//
// ActiveOutFile --
//   provides the continuous data feed between Swarm and a File
//
@protocol ActiveOutFile <MessageProbe>
USING
- setFileObject: aFileObj;
- setDataFeed: d;
- step;
@end

void initSwarm (int argc, const char **argv);
void initSwarmArguments (int argc, const char **argv, Class argumentsClass);

// Flag for whether we're in graphics mode or not. Default is 1.
extern int swarmGUIMode;

@class ControlPanel;
@class UName;
@class InFile;
@class OutFile;
@class AppendFile;
@class ObjectLoader;
@class ObjectSaver;
@class QSort;
@class NSelect;
@class ActiveOutFile;
