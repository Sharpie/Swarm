// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/HDF5Archiver.h>
#import <defobj/HDF5Object.h>
#import <misc.h>

#include <collections/predicates.h>

externvardef HDF5Archiver_c *hdf5AppArchiver;
externvardef HDF5Archiver_c *hdf5Archiver;

@implementation HDF5Archiver_c

PHASE(Creating)

+ createBegin: aZone
{
  HDF5Archiver_c *newArchiver = [super createBegin: aZone];

  newArchiver->applicationMap = [Map create: aZone];
  newArchiver->classes = [List create: aZone];
  newArchiver->instances = [List create: aZone];
  newArchiver->path = NULL;
  newArchiver->inhibitLoadFlag = NO;
  newArchiver->systemArchiverFlag = NO;
  
  return newArchiver;
}

+ create: aZone setPath: (const char *)thePath
{
  HDF5Archiver_c *obj = [self createBegin: aZone];
  obj->path = thePath;
  return [obj createEnd];
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
  id aZone = [self getZone];

  [super createEnd];
  
  currentApplicationKey = [self createAppKey: [arguments getAppName]
                                mode: [arguments getAppModeString]];

  if (!inhibitLoadFlag)
    {
      if (access (path, R_OK) != -1)
        {
          id file = [[[[[HDF5 createBegin: aZone]
                         setCreateFlag: NO]
                        setParent: nil]
                       setName: path]
                      createEnd];
          
          [self hdf5LoadArchiver: file];
          [file drop];
        }
    }
  
  return self;
}

PHASE(Setting)

- hdf5LoadObjectMap: topHDF5Obj key: appKey
{
  id app;
  id aZone = [self getZone];

  int objIterateFunc (id hdf5Obj)
    {
      id key = [String create: aZone setC: [hdf5Obj getName]];
      id value = hdf5In (aZone, hdf5Obj);
      id objectMap = ([hdf5Obj getDatasetFlag] 
                      ? [app getShallowMap]
                      : [app getDeepMap]);

      if ([objectMap at: key] == nil)
        [objectMap at: key insert: value];
      else
        {
          raiseEvent (WarningMessage,
                      "Duplicate HDF5 object key `%s'",
                      [key getC]);
          [key drop];
          [value drop];
        }
      return 0;
    }
  app = [self ensureApp: appKey];
  [topHDF5Obj iterate: objIterateFunc];
  return self; 
}

- hdf5LoadArchiver: hdf5File
{
  if (systemArchiverFlag)
    {
      int appIterateFunc (id appHDF5Obj)
        {
          int modeIterateFunc (id modeHDF5Obj)
            {
              [self hdf5LoadObjectMap: modeHDF5Obj key: 
                      [self createAppKey: [appHDF5Obj getName]
                            mode: [modeHDF5Obj getName]]];
              return 0;
            }
          [appHDF5Obj iterate: modeIterateFunc];
          return 0;
        }
      [hdf5File iterate: appIterateFunc];
    }
  else
    [self hdf5LoadObjectMap: hdf5File key: currentApplicationKey];
  return self;
}

PHASE(Using)

static id
hdf5_create_app_group (const char *appKey, id hdf5Obj, id *hdf5AppObjPtr)
{
  id hdf5AppObj = hdf5Obj;
  char *newAppKey, *modeKey;

  newAppKey = strdup (appKey);
  modeKey = newAppKey;
  
  while (*modeKey && *modeKey != '/')
    modeKey++;
  if (*modeKey == '/')
    {
      *modeKey = '\0';
      modeKey++;
      hdf5AppObj = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                        setCreateFlag: YES]
                       setParent: hdf5Obj]
                      setName: newAppKey]
                     createEnd];
      *hdf5AppObjPtr = hdf5AppObj;
    }
  else
    raiseEvent (InvalidArgument, "expecting composite app/mode key");
  return [[[[[HDF5 createBegin: [hdf5AppObj getZone]]
              setParent: hdf5AppObj]
             setName: modeKey]
            setCreateFlag: YES]
           createEnd];
}

static void
hdf5_output_objects (id <Map> objectMap, id hdf5Obj, BOOL deepFlag)
{
  id index = [objectMap begin: scratchZone];
  id key, member;
  
  for (member = [index next: &key];
       [index getLoc] == (id) Member;
       member = [index next: &key])
    {
      if (member)
        {
          if (deepFlag && !stringp (member))
            {
              id memberGroup = [[[[[HDF5 createBegin: [hdf5Obj getZone]]
                                    setCreateFlag: YES]
                                   setParent: hdf5Obj]
                                  setName: [key getC]]
                                 createEnd];
              
              [member hdf5OutDeep: memberGroup];
              [memberGroup drop];
            }
          else
            {
              id dataset = [[[[[[HDF5 createBegin: [hdf5Obj getZone]]
                             setCreateFlag: YES]
                                setParent: hdf5Obj]
                               setDatasetFlag: YES]
                              setName: [key getC]]
                             createEnd];
              
              [member hdf5OutShallow: dataset];
              [dataset drop];
            }
        }
    }
}

static void
archiverHDF5Put (id aZone, const char *keyStr, id value, id addMap, id removeMap)
{
  id key = [String create: [addMap getZone] setC: keyStr];
  
  if ([addMap at: key])
    [addMap at: key replace: value];
  else
    [addMap at: key insert: value];

  if ([removeMap at: key])
    [removeMap removeKey: key];
}

- putDeep: (const char *)key object: object
{
  id app = [self getApplication];

  archiverHDF5Put ([self getZone], key, object, [app getDeepMap], 
               [app getShallowMap]);
  return self;
}

- putShallow: (const char *)key object: object
{
  id app = [self getApplication];

  archiverHDF5Put ([self getZone], key, object, [app getShallowMap], 
               [app getDeepMap]);
  return self;
}

static id
archiverHDF5Get (id aZone, id string, id app)
{
  id result;
  
  result = [[app getDeepMap] at: string];
  if (result == nil)
    result = [[app getShallowMap] at: string];
  return result;
}

- _getWithZone_: aZone _object_: (const char *)key 
{
  id string = [String create: [self getZone] setC: key];
  id app = [self getApplication];
  id result; 
  
  result = archiverHDF5Get (aZone, string, app);

  [string drop];
  return result;
}

- getObject: (const char *)key
{
  return [self _getWithZone_: [self getZone] _object_: key];
}

- getWithZone: aZone object: (const char *)key
{
  return [self _getWithZone_: aZone _object_: key];
}

- _hdf5Out_: hdf5Obj
{
  id <MapIndex> index = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  while ((app = [index next: &appKey]))
    {
      if (systemArchiverFlag)
        {
          id appGroup;
          id modeGroup = hdf5_create_app_group ([appKey getC],
                                                hdf5Obj, &appGroup);
          
          hdf5_output_objects ([app getShallowMap], modeGroup, NO);
          hdf5_output_objects ([app getDeepMap], modeGroup, YES);
          
          [modeGroup drop];
          [appGroup drop];
        }
      else
        {
          hdf5_output_objects ([app getShallowMap], hdf5Obj, NO);
          hdf5_output_objects ([app getDeepMap], hdf5Obj, YES);
        }
    }
  [index drop];
  return self;
}

- save
{
  [super updateArchiver];
  if ([self countObjects: YES] + [self countObjects: NO] > 0)
    {
      id hdf5Obj = [[[[[HDF5 createBegin: [self getZone]]
                        setCreateFlag: YES]
                       setParent: nil]
                      setName: path]
                     createEnd];
      
      [self _hdf5Out_: hdf5Obj];
      [hdf5Obj drop];
    }
  
  return self;
}

@end
