// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         StringObject.m
Description:  character string object   
Library:      collections
*/

#import <collections/StringObject.h>
#import <defobj/defalloc.h>

#include <misc.h> // memcpy, size_t
#import <defobj.h> // HDF5 things

@implementation String_c

PHASE(Creating)
     
+ createBegin: aZone
{
  String_c *newString;

  newString = [aZone allocIVars: self];

  // create initial string of length zero

  newString->count  = 0;
  newString->literalFlag = NO;
  newString->string = "";
  return newString;
}

- createEnd
{
  createByMessageTo (self, copy:);
  setMappedAlloc (self);
  setNextPhase (self);
  return self;
}

+ create: aZone  // same as createBegin: -- optimization only
{
  String_c *newString;

  newString = [aZone allocIVars: getNextPhase (self)];
  setMappedAlloc (newString);
  newString->count  = 0;
  newString->string = "";
  return newString;
}

+ create: aZone setC: (const char *)cstring
{
  String_c *newString;

  newString = [aZone allocIVars: getNextPhase (self)];
  setMappedAlloc (newString);
  newString->count = strlen (cstring);

  if (newString->count > 0)
    {
      newString->string = [getZone (newString)
                                   allocBlock: newString->count + 1];
      memcpy (newString->string, cstring, newString->count + 1);
    }
  else
    newString->string = "";

  return newString;
}


PHASE(Setting)
- setLiteralFlag: (BOOL)theLiteralFlag
{
  literalFlag = theLiteralFlag;
  return self;
}

- (void)setC: (const char *)cstring
{
  size_t countNew;
  char *stringNew;

  if (!cstring)
    raiseEvent (InvalidArgument, "> argument is nil\n");
  countNew = strlen (cstring);
  if (countNew > 0)
    {
      stringNew = [getZone (self) allocBlock: countNew + 1];
      memcpy (stringNew, cstring, countNew + 1);
    }
  else
    stringNew = "";
  if (count > 0)
    [getZone (self) freeBlock: string blockSize: count + 1];
  string = stringNew;
  count = countNew;
}

PHASE(Using)

//
// copy: -- standard method to copy internal state of object
//
- copy: aZone
{
  String_c  *newString;

  newString = [aZone copyIVars: self];
  setMappedAlloc (newString);
  if (count > 0)
    {
      newString->string = [aZone allocBlock: count + 1];
      memcpy (newString->string, string, count + 1);
    }
  return newString;
}

- (const char *)getC
{
  return string;
}

- (void)catC: (const char *)cstring
{
  id zone;
  size_t appendCount;
  char *stringNew;

  zone = getZone (self);
  if (!cstring)
    raiseEvent (InvalidArgument, "> argument is nil\n");
  appendCount = strlen (cstring);
  if ((count + appendCount) > 0)
    {
      stringNew = [zone allocBlock: count + appendCount + 1];
      memcpy (stringNew, string, count);
      memcpy (stringNew + count, cstring, appendCount + 1);
      if (count > 0)
        [zone freeBlock: string blockSize: count + 1];
      string = stringNew;
      count  = count + appendCount;
    }
}

- (unsigned)getCount
{
  return count;
}

- (unsigned)count
{
  return count;
}

- (unsigned)length
{
  return count;
}

- (int)compare: aString
{
  return strcmp (string, ((String_c *) aString)->string);
}

- (BOOL)getLiteralFlag
{
  return literalFlag;
}

- lispIn: expr
{
  id first = [expr getFirst];
  const char *newStr = [first getC];

  count = strlen (newStr);
  string = [getZone (self) allocBlock: count + 1];
  strcpy (string, newStr);

  return self;
}

- (void)lispOutShallow: stream
{
  if (literalFlag)
    [stream catString: string];
  else
    {
      [stream catStartMakeInstance: [self getTypeName]];
      [stream catSeparator];
      [stream catC: " \""];
      [stream catC: string];
      [stream catC: "\")"];
    }
}

- (void)lispOutDeep: stream
{
  [self lispOutShallow: stream];
}


- hdf5In: hdf5Obj
{
  [hdf5Obj loadDataset: &string];
  return self;
}

- (void)hdf5OutShallow: hdf5Obj
{
  [hdf5Obj storeAsDataset: [hdf5Obj getHDF5Name]
           typeName: [self getTypeName]
           type: fcall_type_string
           rank: 0
           dims: NULL
           ptr: &string];
}

//
// describe: -- standard method to generate debug description
//
- (void)describe: outputCharStream
{
  char buffer[100];

  [super describe: outputCharStream];
  sprintf (buffer, "> number of characters: %d\n", count);
  [outputCharStream catC: buffer];
  if (count <= 64)
    {
      sprintf (buffer, "> string value: %s\n", string);
      [outputCharStream catC: buffer];
    } 
  else
    {
      sprintf (buffer, "> string value (first 50 characters): \"%.50s\"\n",
               string);
      [outputCharStream catC: buffer];
    }
}

//
// mapAllocations: -- standard method to map internal allocations
//
- (void)mapAllocations: (mapalloc_t)mapalloc
{
  if (!includeBlocks (mapalloc) || *string == '\0')
    return;
  mapalloc->size = count + 1;
  mapAlloc (mapalloc, string);
}

@end
