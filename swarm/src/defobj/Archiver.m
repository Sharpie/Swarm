// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Archiver.h>

#import <collections.h>
#import <collections/predicates.h> // list_literal_p, listp, pairp, stringp
#import <defobj.h> // arguments

#include <misc.h> // access, getenv, xmalloc, stpcpy, strdup

Archiver_c *archiver;

static int
compareStrings (id val1, id val2)
{
  return [val1 compare: val2];
}

@interface Application: CreateDrop
{
  const char *name;
  id <Map> lispDeepMap;
  id <Map> lispShallowMap;
#ifdef HAVE_HDF5
  id <Map> hdf5DeepMap;
  id <Map> hdf5ShallowMap;
#endif
}
+ createBegin: aZone;
- setName: (const char *)name;
- getLispDeepMap;
- getLispShallowMap;
#ifdef HAVE_HDF5
- getHDF5DeepMap;
- getHDF5ShallowMap;
#endif
@end

@implementation Application
+ createBegin: aZone
{
  Application *obj = [super createBegin: aZone];

  obj->lispDeepMap =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
  obj->lispShallowMap =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
#ifdef HAVE_HDF5
  obj->hdf5DeepMap =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
  obj->hdf5ShallowMap =
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
#endif
  obj->name = "EMPTY";

  return obj;
}

- setName: (const char *)theName
{
  name = strdup (theName);
  return self;
}

- getLispDeepMap
{
  return lispDeepMap;
}

- getLispShallowMap
{
  return lispShallowMap;
}

#ifdef HAVE_HDF5
- getHDF5DeepMap
{
  return hdf5DeepMap;
}

- getHDF5ShallowMap
{
  return hdf5ShallowMap;
}
#endif

- (void)drop
{
  [lispShallowMap deleteAll];
  [lispDeepMap deleteAll];
#ifdef HAVE_HDF5
  [hdf5ShallowMap deleteAll];
  [hdf5DeepMap deleteAll];
#endif
  [super drop];
}

@end

#ifdef HAVE_HDF5

@implementation HDF5
PHASE(Creating)
- setParent: theParent
{
  parent = theParent;
  return self;
}

- setName: (const char *)theName
{
  name = theName;
  return self;
}

- createEnd
{
  [super createEnd];
  if (parent == nil)
    {
      if ((loc_id = H5Fcreate (name, H5F_ACC_TRUNC,
                               H5P_DEFAULT, H5P_DEFAULT))  < 0)
        raiseEvent (SaveError, "Failed to create HDF5 file `%s'", name);
    }
  else
    {
      if ((loc_id = H5Gcreate (((HDF5 *) parent)->loc_id, name, 0)) < 0)
        raiseEvent (SaveError, "Failed to create HDF5 group `%s'", name);
    }
  return self;
}

PHASE(Using)

- store: (const char *)datasetName type: (const char *)type ptr: (void *)ptr
{
  void store (hid_t type)
    {
      hid_t did;
      hid_t space;
      hsize_t dims[1];

      if ((space = H5Screate_simple (1, dims, NULL)) == -1)
        raiseEvent (SaveError, "unable to create dataspace");
      if ((did = H5Dcreate (loc_id, datasetName,
                            type, space, H5P_DEFAULT)) < 0)
        raiseEvent (SaveError, "unable to store %s as char", datasetName);
      
      if (H5Dwrite (did, type, space, space, H5P_DEFAULT, ptr) < 0)
        raiseEvent (SaveError, "unable to write %s as char", datasetName);
      if (H5Dclose (did) < 0)
        raiseEvent (SaveError, "unable to close dataset %s", datasetName);
      if (H5Sclose (space) < 0)
        raiseEvent (SaveError, "unable to close dataspace");
    }

  switch (*type)
    {
    case _C_CHR:
      store (H5T_NATIVE_CHAR);
      break;
    case _C_UCHR:
      store (H5T_NATIVE_UCHAR);
      break;
    case _C_SHT:
      store (H5T_NATIVE_SHORT);
      break;
    case _C_USHT:
      store (H5T_NATIVE_USHORT);
      break;
    case _C_INT:
      store (H5T_NATIVE_INT);
      break;
    case _C_UINT:
      store (H5T_NATIVE_UINT);
      break;
    case _C_LNG:
      store (H5T_NATIVE_LONG);
      break;
    case _C_ULNG:
      store (H5T_NATIVE_ULONG);
      break;
    case _C_FLT:
      store (H5T_NATIVE_FLOAT);
      break;
    case _C_DBL:
      store (H5T_NATIVE_DOUBLE);
      break;
    case _C_CHARPTR:
      {
        const char *str = *(const char **)ptr;
        hid_t stringtype = H5Tcopy (H5T_C_S1);
        H5Tset_size (stringtype, strlen (str) + 1);

        store (stringtype);
        if (H5Tclose (stringtype) < 0)
          raiseEvent (SaveError, "cannot close stringtype");
      }
      break;
    case _C_ARY_B:
      printf ("ignoring array [%s]", name);
      break;
    default:
      abort ();
    }
  return self;
}

- (void)drop
{
  if (parent == nil)
    {
      if (H5Fclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 file");
    }
  else
    {
      if (H5Gclose (loc_id) < 0)
        raiseEvent (SaveError, "Failed to close HDF5 group");
    }
}  

@end
#endif

static const char *
defaultPath (const char *swarmArchiver)
{
  const char *home = getenv ("HOME");

  if (home)
    {
      char *buf = xmalloc (strlen (home) + 1 + strlen (swarmArchiver) + 1), *p;

      p = stpcpy (buf, home);
      p = stpcpy (p, "/");
      p = stpcpy (p, swarmArchiver);
      
      return buf;
    }
  return NULL;
}

static const char *
lispDefaultPath (void)
{
  return defaultPath (SWARMARCHIVER_LISP);
}

#ifdef HAVE_HDF5
static const char *
hdf5DefaultPath (void)
{
  return defaultPath (SWARMARCHIVER_HDF5);
}
#endif

static void
lispProcessPairs (id aZone, 
                  id obj,
                  void (*mapUpdateFunc) (id, id))
{
  if (!listp (obj))
    raiseEvent (InvalidArgument, "argument to processPairs not a list");
  {
    id listExprIndex = [obj begin: scratchZone];
    id listExpr = [listExprIndex next];
    
    if (!list_literal_p (listExpr))
      raiseEvent (InvalidArgument,
                  "first string in processPairs not \"list\"");
    {
      id consObject;

      while ((consObject = [listExprIndex next]) != nil)
        {
          if (!pairp (consObject))
            raiseEvent (InvalidArgument, "Expecting a pair object");

          {
            id key = [[consObject getCar] copy: aZone];
            
            if (listp (key))
              {
                id first = [key getFirst];
                id last = [key getLast];
                
                if (!stringp (first))
                  raiseEvent (InvalidArgument,
                              "first pair item not a string (%s)",
                              [first name]);
                if (!stringp (last))
                  raiseEvent (InvalidArgument,
                              "second pair item not a string (%s)",
                              [last name]);
                [first catC: "/"];
                [first catC: [last getC]];
                key = [first copy: aZone];
              }
            
            if (!stringp (key))
              raiseEvent (InvalidArgument, "key not a string");
            mapUpdateFunc (key, [consObject getCdr]);
          }
        }
    }
    [listExprIndex drop];
  }
}

static void
lispProcessMakeObjcPairs (id aZone, id expr, id objectMap)
{
  {
    void mapUpdate (id key, id valexpr)
      {
        id value = lispIn (aZone, valexpr);

        if ([objectMap at: key])
          [objectMap at: key replace: value];
        else
          [objectMap at: key insert: value];
      }
    lispProcessPairs (aZone, expr, mapUpdate);
  }
}

static void
lispProcessApplicationPairs (id aZone, id expr, id applicationMap)
{
  void mapUpdate (id key, id value)
    {
      Application *app = [applicationMap at: key];
      
      if (app == nil)
        {
          app = [[[Application createBegin: aZone]
                   setName: [key getC]]
                  createEnd];
          [applicationMap at: key insert: app];
        }
      lispProcessMakeObjcPairs (aZone, value, [app getLispDeepMap]);
    }
  lispProcessPairs (aZone, expr, mapUpdate);
}

static void
lispLoadArchiverExpr (id applicationMap, id expr)
{
  id archiverCallExprIndex, archiverCallName;
  
  if (!listp (expr))
    raiseEvent (InvalidArgument, "argument to Archiver lispIn not a list");
  
  archiverCallExprIndex = [expr begin: scratchZone];
  archiverCallName = [archiverCallExprIndex next];

  if (!stringp (archiverCallName))
    raiseEvent (InvalidArgument, "Archiver function not a string");
  
  if (strcmp ([archiverCallName getC], ARCHIVER_FUNCTION_NAME) != 0)
    raiseEvent (InvalidArgument,
                "Archiver function name incorrect: [%s]",
                [archiverCallName getC]);

  lispProcessApplicationPairs ([applicationMap getZone],
                               [archiverCallExprIndex next],
                               applicationMap);
  [archiverCallExprIndex drop];
}

void
archiverRegister (id client)
{

  if ([client isClass])
    {
      if (![archiver->classes contains: client])
        [archiver->classes addLast: client];
    }
  else if (![archiver->instances contains: client])
    [archiver->instances addLast: client];
}

void
archiverUnregister (id client)
{
  if ([client isClass])
    [archiver->classes remove: client];
  else
    [archiver->instances remove: client];
}

void
lispArchiverPut (const char *key, id object, BOOL deepFlag)
{
  id app = [archiver getApplication];
  id map = deepFlag ? [app getLispDeepMap] : [app getLispShallowMap];
  id keyObj = [String create: [archiver getZone] setC: key];
  
  if ([map at: keyObj])
    [map at: keyObj replace: object];
  else
    [map at: keyObj insert: object];
}

void
hdf5ArchiverPut (const char *key, id object, BOOL deepFlag)
{
  id app = [archiver getApplication];
  id map = deepFlag ? [app getHDF5DeepMap] : [app getHDF5ShallowMap];
  id keyObj = [String create: [archiver getZone] setC: key];
  
  if ([map at: keyObj])
    [map at: keyObj replace: object];
  else
    [map at: keyObj insert: object];
}

id
lispArchiverGet (const char *key)
{
  id string = [String create: [archiver getZone] setC: key];
  id app = [archiver getApplication];
  id result;
  
  result = [[app getLispDeepMap] at: string];
  if (result == nil)
    result = [[app getLispShallowMap] at: string];
  
  [string drop];
  return result;
}

id
hdf5ArchiverGet (const char *key)
{
  id string = [String create: [archiver getZone] setC: key];
  id app = [archiver getApplication];
  id result;
  
  result = [[app getHDF5DeepMap] at: string];
  if (result == nil)
    result = [[app getHDF5ShallowMap] at: string];
  
  [string drop];
  return result;
}

void
archiverSave (void)
{
  [archiver save];
}

@implementation Archiver_c
PHASE(Creating)

+ createBegin: aZone
{
  Archiver_c *newArchiver = [super createBegin: aZone];

  newArchiver->applicationMap = 
    [[[Map createBegin: aZone] setCompareFunction: &compareStrings] createEnd];
  newArchiver->classes = [List create: aZone];
  newArchiver->instances = [List create: aZone];
  newArchiver->lispPath = lispDefaultPath ();
#ifdef HAVE_HDF5
  newArchiver->hdf5Path = hdf5DefaultPath ();
#endif
  return newArchiver;
}

- createEnd
{
  const char *appName = [arguments getAppName];
  const char *appModeString = [arguments getAppModeString];

  [super createEnd];
  currentApplicationKey = 
    [String create: [self getZone] setC: appName];
  
  [currentApplicationKey catC: "/"];
  [currentApplicationKey catC: appModeString];
  
  if (lispPath)
    {
      FILE *fp = fopen (lispPath, "r");
      
      if (fp != NULL)
        {
          // Create a temporary zone to simplify destruction of expression
          id inStreamZone = [Zone create: scratchZone];
          id inStream = [InputStream create: inStreamZone setFileStream: fp];
          
	  lispLoadArchiverExpr (applicationMap,
				[inStream getExpr]);
          [inStreamZone drop]; 
          fclose (fp);
        }
    }
#ifdef HAVE_HDF5
  if (hdf5Path)
    {
      // printf ("post:[%s]\n", [self name]);
    }
#endif
  return self;
}

PHASE(Setting)

- setLispPath: (const char *)thePath
{
  lispPath = strdup (thePath);

  return self;
}

#ifdef HAVE_HDF5
- setHDF5Path: (const char *)thePath
{
  hdf5Path = strdup (thePath);
  
  return self;
}
#endif

PHASE(Using)
     
- getApplication
{
  id app = [applicationMap at: currentApplicationKey];
  
  if (app == nil)
    {
      app = [Application create: [self getZone]];
      [applicationMap at: currentApplicationKey insert: app];
    }
  return app;
}

static void
lisp_print_appkey (const char *appKey, id <OutputStream> outputCharStream)
{
  FILE *fp = [outputCharStream getFileStream];
  
  [outputCharStream catC: "'("];
  while (*appKey && *appKey != '/')
    {
      fputc (*appKey, fp);
      appKey++;
    }
  if (*appKey == '/')
    {
      fputc (' ', fp);
      appKey++;
      while (*appKey)
        {
          fputc (*appKey, fp);
          appKey++;
        }
    }
  [outputCharStream catC: ")"];
}

static void
lisp_output_objects (id <Map> objectMap, id outputCharStream, BOOL deepFlag)
{
  id objectMapIndex = [objectMap begin: scratchZone];
  id key, member;
  
  while ((member = [objectMapIndex next: &key]))
    {
      [outputCharStream catC: "\n        (cons '"];
      [outputCharStream catC: [key getC]];
      [outputCharStream catC: "\n          "];
      if (![member isClass])
        [member lispOut: outputCharStream deep: deepFlag];
      else
        {
          SEL sel = M(lispOut:deep:);
          IMP func = get_imp (id_CreatedClass_s, sel);
          
          func (member, sel, outputCharStream, NO);
        }
      [outputCharStream catC: ")"];
    }
}

- lispOut: outputCharStream
{
  id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  [outputCharStream catC: "(" ARCHIVER_FUNCTION_NAME "\n  (list"];
  
  [outputCharStream catC: "\n    (cons "];
  while ((app = [appMapIndex next: &appKey]))
    {
      lisp_print_appkey ([appKey getC], outputCharStream);
      [outputCharStream catC: "\n      (list"];
      lisp_output_objects ([app getLispShallowMap], outputCharStream, NO);
      lisp_output_objects ([app getLispDeepMap], outputCharStream, YES);
      [outputCharStream catC: ")"];
    }
  [outputCharStream catC: ")))\n"];
  [appMapIndex drop];
  return self;
}

#ifdef HAVE_HDF5

static id
hdf5_create_app_group (const char *appKey, id hdf5Obj)
{
  id hdf5AppObj = hdf5Obj;
  char *modeKey;

  appKey = strdup (appKey);
  modeKey = (char *) appKey;
  
  while (*modeKey && *modeKey != '/')
    modeKey++;
  if (*modeKey == '/')
    {
      *modeKey = '\0';
      modeKey++;
      hdf5AppObj = [[[[HDF5 createBegin: [hdf5Obj getZone]]
                       setParent: hdf5Obj]
                      setName: appKey]
                     createEnd];
    }
  return [[[[HDF5 createBegin: [hdf5AppObj getZone]]
             setParent: hdf5AppObj]
            setName: modeKey]
           createEnd];
}

static void
hdf5_output_objects (id <Map> objectMap, id hdf5Obj, BOOL deepFlag)
{
  id objectMapIndex = [objectMap begin: scratchZone];
  id key, member;
  
  while ((member = [objectMapIndex next: &key]))
    {
      id hdf5Group = [[[[HDF5 createBegin: [hdf5Obj getZone]]
                         setParent: hdf5Obj]
                        setName: [key getC]]
                       createEnd];
      
      if (![member isClass])
        [member hdf5Out: hdf5Group deep: deepFlag];
      else
        {
          SEL sel = M(hdf5Out:deep:);
          IMP func = get_imp (id_CreatedClass_s, sel);
          
          func (member, sel, hdf5Group, NO);
        }
    }
}

- (unsigned)countHDF5Objects: (BOOL)deepFlag
{
  id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  unsigned count = 0;
  
  while ((app = [appMapIndex next: &appKey]))
    count += [(deepFlag ? [app getHDF5DeepMap] : [app getHDF5ShallowMap])
               getCount];
  [appMapIndex drop];
  return count;
}

- hdf5Out: hdf5Obj
{
  id <MapIndex> appMapIndex = [applicationMap begin: scratchZone];
  id app;
  id <String> appKey;
  
  while ((app = [appMapIndex next: &appKey]))
    {
      id hdf5Group = hdf5_create_app_group ([appKey getC], hdf5Obj);
        
      hdf5_output_objects ([app getHDF5ShallowMap], hdf5Group, NO);
      hdf5_output_objects ([app getHDF5DeepMap], hdf5Group, YES);
    }
  [appMapIndex drop];
  return self;
}

#endif

- updateArchiver
{
  id <Index> index;
  id item;
  IMP func = get_imp (id_CreatedClass_s, M(updateArchiver));
  
  index = [classes begin: [self getZone]];
  while ((item = [index next]))
    func (item, M(updateArchiver));
  [index drop];
  [instances forEach: @selector (updateArchiver)];
  return self;
}

- save
{
  [self updateArchiver];
  if (lispPath)
    {
      FILE *fp = fopen (lispPath, "w");
      id outStream;
      
      if (fp == NULL)
        raiseEvent (SaveError, "Cannot open lisp archive %s", lispPath);
      outStream = [OutputStream create: scratchZone setFileStream: fp];
      [self lispOut: outStream];
      fclose (fp);
      [outStream drop];
    }
#ifdef HAVE_HDF5
  if (hdf5Path)
    {
      if ([self countHDF5Objects: YES] +
          [self countHDF5Objects: NO] > 0)
        {
          id hdf5Obj = [[[[HDF5 createBegin: [self getZone]]
                           setParent: nil]
                          setName: hdf5Path]
                         createEnd];
          
          [self hdf5Out: hdf5Obj];
          [hdf5Obj drop];
        }
    }
#endif
  return self;
}

- (void)drop
{
  [applicationMap deleteAll];
  [applicationMap drop];
  if (lispPath)
    XFREE (lispPath);
  [classes drop];
  [instances drop];
  [super drop];
}

@end
