// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/HDF5Archiver.h>
#import <defobj/HDF5Object.h>
#import <defobj.h> // OSTRDUP
#import <misc.h> // access
#import <defobj/defalloc.h> // getZone

#include <collections/predicates.h>

@implementation HDF5Archiver_c

static id
hdf5_create_app_group (const char *appKey, id hdf5Obj)
{
  id hdf5AppObj = hdf5Obj;
  char *newAppKey, *modeKey;

  newAppKey = OSTRDUP (hdf5Obj, appKey);
  modeKey = newAppKey;
  
  while (*modeKey && *modeKey != '/')
    modeKey++;
  if (*modeKey == '/')
    {
      *modeKey = '\0';
      modeKey++;
      hdf5AppObj = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                        setWriteFlag: YES]
                       setParent: hdf5Obj]
                      setName: newAppKey]
                     createEnd];
    }
  else
    raiseEvent (InvalidArgument, "expecting composite app/mode key");
  return [[[[[HDF5 createBegin: [hdf5AppObj getZone]]
              setParent: hdf5AppObj]
             setName: modeKey]
            setWriteFlag: YES]
           createEnd];
}

PHASE(Creating)

+ createBegin: aZone
{
  HDF5Archiver_c *newArchiver = [super createBegin: aZone];
  return newArchiver;
}

+ create: aZone setPath: (const char *)thePath
{
  return [super create: aZone setPath: thePath];
}

- setDefaultPath
{
  path = defaultPath (SWARMARCHIVER_HDF5);
  return self;
}

- setDefaultAppPath
{
  path = defaultAppPath ([arguments getAppDataPath],
                         [arguments getAppName],
                         SWARMARCHIVER_HDF5_SUFFIX);
  return self;
}

- createEnd
{
  [super createEnd];
  [self ensureApp: 
          [[[[[HDF5 createBegin: getZone (self)]
               setWriteFlag: NO]
              setParent: nil]
             setName: path]
            createEnd]];
  return self;
}

PHASE(Setting)

- (void)ensureApp: hdf5File
{
  if (systemArchiverFlag)
    {
      int appIterateFunc (id appHDF5Obj)
        {
          int modeIterateFunc (id modeHDF5Obj)
            {
              [applicationMap at: [self createAppKey: [appHDF5Obj getName]
                                         mode: [modeHDF5Obj getName]]
                              insert: modeHDF5Obj];
              return 0;
            }
          [appHDF5Obj iterate: modeIterateFunc drop: NO];
          return 0;
        }
      if (hdf5File)
        [hdf5File iterate: appIterateFunc drop: NO];
      if (![self getApplication])
        hdf5File = nil;
    }
  [applicationMap at: currentApplicationKey insert: hdf5File];
}

PHASE(Using)

- getWritableController
{
  id hdf5Obj = [self getApplication];
  
  if (hdf5Obj)
    {
      if ([hdf5Obj getWriteFlag])
        return hdf5Obj;
      else
        [hdf5Obj drop];
    }
  hdf5Obj = [[[[[HDF5 createBegin: getZone (self)]
                 setWriteFlag: YES]
                setParent: nil]
               setName: path]
              createEnd];
  
  if (systemArchiverFlag)
    hdf5Obj = hdf5_create_app_group ([currentApplicationKey getC], hdf5Obj);
  
  [applicationMap at: currentApplicationKey replace: hdf5Obj];
  return hdf5Obj;
}

- (void)putDeep: (const char *)key object: object
{
  id group = [[[[[HDF5 createBegin: getZone (self)]
                  setWriteFlag: YES]
                 setParent: [self getWritableController]]
                setName: key]
               createEnd];

  if (!group)
    abort ();
  [object hdf5OutDeep: group];
  [group drop];
}

- (void)putShallow: (const char *)key object: object
{
  id dataset = [[[[[[HDF5 createBegin: getZone (self)]
                     setWriteFlag: YES]
                    setParent: [self getWritableController]]
                   setDatasetFlag: YES]
                  setName: key]
                 createEnd];
  if (!dataset)
    abort ();
  [object hdf5OutShallow: dataset];
  [dataset drop];
}

- getWithZone: aZone key: (const char *)key 
{
  id result; 
  id parent = [self getApplication];
  
  if (parent)
    {
      id hdf5Obj = [[[[[HDF5 createBegin: getZone (self)]
                        setParent: parent]
                       setDatasetFlag: [parent checkDatasetName: key]]
                      setName: key]
                     createEnd];
  
      if (hdf5Obj)
        {
          result = hdf5In (aZone, hdf5Obj);
          [hdf5Obj drop];
        }
      else
        result = nil;
    }
  else
    result = nil;
  return result;
}

- getObject: (const char *)key
{
  return [self getWithZone: getZone (self) key: key];
}

- (void)sync
{
  [self updateArchiver];
  [[self getApplication] flush];
}

@end

