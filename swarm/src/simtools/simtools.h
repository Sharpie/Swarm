// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.    

//S: General simulation tools

//D: A collection of tools that are only loosely related to each other.
//D: the class hierarchy is virtually flat.

#import <objectbase.h>

@protocol UName <SwarmObject>
//S: A class used to generate unique names (e.g. "critter1", "critter2" etc.)

//D: This class is used to generate unique names (agent0, agent1,
//D: agent2...) for objects in a simulation. The user will typically create
//D: an instance of the UName class initialized with a baseName presented
//D: either as a (const char *) or an object of class String. The user can
//D: then request new names, again either as (const char *)'s or as
//D: instances of the String class. The user can also reset the counter
//D: used to generate the names in case s/he wants to restart naming
//D: objects with the same baseName.

//D: Note: Both in the case of initialization by (const char *) and
//D: initialization by an instance of the String class, the original is
//D: copied not stored internally so it is up to the user to free the
//D: original (const char *) or String instance if/when necessary!

CREATING
//M: The create:setBaseNameObject: method is used to create an instance of the
//M: UName class and set the base name given a const char *.  This method will 
//M: automatically reset the counter.
+ create: aZone setBaseName: (const char *)aString;

//M: The create:setBaseNameObject: method is used to create an instance of the
//M: UName class and set the base name given an object of class String. This 
//M: method will automatically reset the counter.
+ create: aZone setBaseNameObject: aStringObject;

//M: The setBaseName: method is used to set the base name given a const char *.
- setBaseName: (const char *)aString;

//M: The setBaseNameObject: method is used to set the base name given an object
//M: of class String.
- setBaseNameObject: aStringObject;

USING
//M: The getNewName method generates a new name as a character string.
- (const char *)getNewName;

//M: The getNewNameObject generates a new name as a String Object.
- getNewNameObject;

//M: Resets the counter used as a suffix in the unique names generated.
- resetCounter;
@end

@protocol InFile <SwarmObject>
//S: Class to perform file input.

//D: This class is intended to simplify the input file-I/O in Swarm. It
//D: essentially deals with the detailed file opening and closing routines
//D: thus alleviating the need for C file I/O procedure calls.

CREATING
//M: This is the create method for InFiles, where theName is, of course the
//M: name of the file to open.
+ create: aZone withName: (const char *)theName;

USING
//M: The getWord: method returns a string that does not contain spaces, tabs,
//M: and newlines.
- (int)getWord: (char *)aWord;

//M: The getLine: method loads the argument string with the characters up to,
//M: but not including a newline character.
- (int)getLine: (char *)aLine;

//M: The getInt: method takes a pointer of type Int and loads it with an 
//M: instance of that type from the open file.  In case of failure, the method
//M: returns 0.
- (int)getInt: (int *)anInt;

//M: The getUnsigned: method takes a pointer of type unsigned and loads it  
//M: with an instance of that type from the open file. In case of failure, the 
//M: method returns 0.
- (int)getUnsigned: (unsigned *)anUnsigned;

//M: The getLong: method takes a pointer of type long and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getLong: (long *)aLong;

//M: The getUnsignedLong: method takes a pointer of type unsigned long and 
//M: loads it with an instance of that type from the open file. In case of
//M: failure, the method returns 0.
- (int)getUnsignedLong: (unsigned long *)anUnsLong;

//M: The getDouble: method takes a pointer of type double and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getDouble: (double *)aDouble;

//M: The getFloat: method takes a pointer of type float and loads it with an 
//M: instance of that type from the open file. In case of failure, the method
//M: returns 0.
- (int)getFloat: (float *)aFloat;

//M: The getChar: method takes a pointer of type char and loads it with an 
//M: instance of that type from the open file.  In case of failure, the method
//M: returns 0.
- (int)getChar: (char *)aChar;

//M: Returns a character for re-reading.
- (int)unGetChar: (char)aChar;

//M: Skips a line.
- (int)skipLine;

//M: The drop method must be called when the user wants to close the file.
//M: Only by dropping the InFile object will the file truly be closed.
- (void)drop;
@end

@protocol OutFile <SwarmObject>
//S: A class to perform file output.

//D: This class is intended to simplify output file-I/O in Swarm. It 
//D: essentially deals with the detailed file opening and closing routines 
//D: thus alleviating the need for C file I/O procedure calls. 

CREATING
//M: The create:withName: method opens a file named theName and creates an 
//M: Outfile object.
+ create: aZone withName: (const char *)theName;

USING
//M: The putString: method takes an instance of type string and writes it
//M: into the open file.
- putString: (const char *)aString;

//M: The putInt: method takes an instance of type int and writes it into
//M: the open file.
- putInt: (int) anInt;

//M: The putUnsigned: method takes an instance of type unsigned and writes it 
//M: into the open file.
- putUnsigned: (unsigned)anUnsigned;

//M: The putLong: method takes an instance of type long and writes it 
//M: into the open file.
- putLong: (long)aLong;

//M: The putUnsignedLong: method takes an instance of type unsigned long and 
//M: writes it into the open file.
- putUnsignedLong: (unsigned long)anUnsLong;

//M: The putDouble: method takes an instance of type double and writes it 
//M: into the open file.
- putDouble: (double)aDouble;

//M: The putFloat: method takes an instance of type float and writes it 
//M: into the open file.
- putFloat: (float)aFloat;

//M: The putChar: method takes an instance of type char and writes it 
//M: into the open file.
- putChar: (char)aChar;

//M: The putNewline method writes a newline into the open file.
- putNewLine;

//M: The putTab method writes a tab into the open file.
- putTab;

//M: The drop method closes the open file.  This method must be called to
//M: close the file.
- (void)drop;
@end


@protocol AppendFile <OutFile>
//S: A class for appended file output.

//D: This class subclasses from OutFile, the only functional difference being
//D: that it opens a given file in Append Mode rather than in Overwrite mode.

CREATING
//M: The create:withName: method is the create method for AppendFiles, where 
//M: theName is the name of the file to open.
+ create: aZone withName: (const char *)theName;
@end

@protocol ObjectLoader <SwarmObject>
//S: A class to load an object's instance variables from a file.

//D: This class is used to initialize the variables of a target object from
//D: a data file. The data file is required to have a very simple format.

USING
//M: The load:from: method loads anObject from the previously opened 
//M: aFileObject without returning an actual instance of the ObjectLoader 
//M: class.  The FileObject remains open after the method has been called. 
+ load: anObject from: aFileObject;

//M: The load:fromFileNamed: method loads anObject from the file named 
//M: aFileName.  The ObjectLoader class will open the file, initialize the 
//M: object with its contents and then close the file.
+ load: anObject fromFileNamed: (const char *)aFileName;

//M: The load:fromAppConfigFileNamed: method loads anObject from th
//M: application-specific configuration file named aFileName. 
//M: The ObjectLoader class will open the file, initialize the 
//M: object with its contents and then close the file.
+ load: anObject fromAppConfigFileNamed: (const char *)aFileName;

//M: The load:fromAppConfigFileNamed: method loads anObject from th
//M: application-specific data file named aFileName. 
//M: The ObjectLoader class will open the file, initialize the 
//M: object with its contents and then close the file.
+ load: anObject fromAppDataFileNamed: (const char *)aFileName;

//M: The setFileObject: method sets the source fileObject which the instance 
//M: of the ObjectLoader class should use by sending it this message.
- setFileObject: aFileObject;

//M: The loadObject: message must be sent to an instance of the ObjectLoader 
//M: class in order to initialize the target object from the requested file.
- loadObject: anObject;

//M: The updateCache: method should be called if an ObjectLoader instance is 
//M: going to initialize a large number of objects from the same class.
- updateCache: exampleTarget; 

//M: The setTemplateProbeMap: method is used to specify which variables of the
//M: target object(s) should be loaded by the ObjectLoader instance to which
//M: this message was sent.
- setTemplateProbeMap: probeMap;
@end

@protocol ObjectSaver <SwarmObject>
//S: A class to save an object's instance variables to a file.

//D: This class is used to write an object's variables to a specified file. 
//D: If only a subset of the variables should be written out, the set is 
//D: specified by a template ProbeMap (where the ProbeMap will contain Probes 
//D: for those variables which should be saved). 

USING
//M: The save:to: method saves the entire target object without actually 
//M: returning an instance of ObjectSaver to the user.
+ save: anObject to: aFileObject;

//M: The save:to:withTemplate: method saves the subset of target object 
//M: variables specified in a template from anObject without actually 
//M: returning an instance of ObjectSaver to the user.
+ save: anObject to: aFileObject withTemplate: aProbeMap;

//M: The save:toFileNamed: method saves the entire target object to the file
//M: aFileName.
+ save: anObject toFileNamed: (const char *)aFileName;

//M: The save:toFileNamed:withTemplate: method saves the subset of variables
//M: specified in a template from the target object to the file
//M: aFileName.
+ save: anObject toFileNamed: (const char *)aFileName 
                withTemplate: (ProbeMap *)aProbeMap;

//M: The setFileObject: method sets the target fileObject which the instance 
//M: of the ObjectSaver class should use.
- setFileObject: aFileObject;

//M: The setTemplateProbeMap: method is used to specify which variables of the
//M: source object(s) should be saved by the ObjectSaver instance to which this
//M: message was sent.
- setTemplateProbeMap: aProbeMap;

//M: The saveObject: message tells an instance of the ObjectSaver class to 
//M: save the state of the target object into the requested file.
- saveObject: anObject;
@end

@protocol QSort <SwarmObject>
//S: A class to sort a collection.

//D: QSort is simply a "wrapper" for the C native "qsort" function, as applied
//D: to a Swarm collection. The values will appear in ascending order by
//D: default. Reversing the order of a collection can be made by calling 
//D: reverseOrderOf. All these methods modify the underlying collection, so
//D: any indexes should always be regenerated. 

USING
//M: The sortObjectsIn: method will sort the objects in the collection with the
//M: "compare' function for the object.  If the objects don't provide a
//M: compare function, sortObjectsIn uses the default from the defobj library. 
+ (void)sortObjectsIn: aCollection;

//M: The sortObjectsIn:using: method will sort the objects in the collection 
//M: with the specified comparison function for the object.
+ (void)sortObjectsIn: aCollection using: (SEL) aSelector;

//M: The sortNumbersIn: method performs integer comparisons on the objects in
//M: the collection using the default "compare" function.  The default assumes
//M: that the numbers should be monotonically increasing.
+ (void)sortNumbersIn: aCollection;

//M: The sortNumbersIn:using: method performs integer comparisons on the 
//M: objects in the collection with the specified comparison function for the
//M: object.
+ (void)sortNumbersIn: aCollection
                using: (int(*)(const void*,const void*)) comp_fun;

//M: The reverseOrderOf: method reverses the current order of a collection.
//M: To make a "reversed" sort, simply call one of the appropriate "sort"
//M: methods on a collection then call this function on the same collection.
+ (void)reverseOrderOf: aCollection;
@end

@protocol NSelect <SwarmObject>
//S: A class to select exactly N elements at random from a collection.

//D: NSelect selects exactly N elements from a collection without repetition.
//D: A target collection must be provided.

USING
//M: The select:from:into: method selects exactly N elements from a collection
//M: without repetition into another collection.  The selection algorithm
//M: is from Knuth.
+ (void)select: (int)n from: aCollection into: bCollection;
@end

@protocol ListShuffler <SwarmObject>
//S: A class to randomize the order of a given Swarm List

//D: ListShuffler randomizes the order of the elements in a List; 
//D: either the whole list or the num lowest elements. The list must be
//D: supplied. An uniform distribution can be supplied, or the system-
//D: supplied uniformUnsRand is used. The algorith is from Knuth.
//D: All these methods modify the underlying collection, so
//D: any indexes should always be regenerated. 
CREATING

+ createBegin: aZone;

//M: the setUniformRandom: method connects the supplied uniform distribution 
//M: to the Shuffler (run after createBegin:).
- setUniformRandom: dist;

- createEnd;

//M: the create:setUniformRandom method creates the Shuffler
//M: and connects the supplied distribution object.
+ create: aZone setUniformRandom: dist;

USING

//M: the shuffleWholeList method randomizes the whole list.
- shuffleWholeList: list;

//M: the shufflePartialList:Num method randomizes the order of the 'num'
//M: lowest elements of the list, or the whole list if (num > size of list).
- shufflePartialList: list Num: (int)num;
@end


//F: Initializes the Swarm libraries. This call should be in any Swarm code
//F: you write.
extern void initSwarm (int argc, const char **argv);

//F: Like initSwarm, but specifies what class to use for argument
//F: parsing, typically this will be a subclass of Arguments.
extern void initSwarmArguments (int argc, const char **argv, Class argumentsClass);

//G: Flag for whether we're in graphics mode or not.  Default is 1.
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
@class ListShuffler;

