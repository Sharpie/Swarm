// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/VarProbe.h>
#import <defobj.h> // raiseEvent, WarningMessage, STRDUP, FREEBLOCK
#import "local.h"

#include <objc/objc-api.h>
#include <misc.h> // strcmp, strcpy, sprintf, sscanf

#include "../defobj/internal.h" // process_array

#ifdef HAVE_JDK
#include <defobj/directory.h> // directory services for Java Proxy lookup
#include <defobj/javavars.h>
#endif

#include <swarmconfig.h> // PTRUINT

@implementation VarProbe

PHASE(Creating)

- setProbedVariable: (const char *)aVariable
{ 
  if (probedVariable)
    {
      if (SAFEPROBES)
        {
          raiseEvent (WarningMessage,
                      "It is an error to reset the variable\n");
          return nil;
        }
      else 
        FREEBLOCK (probedVariable);     // memory allocation?
    }
  probedVariable = STRDUP (aVariable);	   // make a local copy
  return self;
}

- createEnd
{
  IvarList_t ivarList;
  int i;
  id aZone = [self getZone];
  
  [super createEnd];

  if (SAFEPROBES)
    if (probedVariable == 0 || probedClass == 0)
      raiseEvent (WarningMessage,
                  "VarProbe object was not properly initialized\n");
#ifdef HAVE_JDK
  if (isJavaProxy)
    {
      classObject = SD_FINDJAVA (jniEnv, probedClass);
      if (!classObject)
	raiseEvent (SourceMessage,
		    "Java class to be probed can not be found!\n");      
      fieldObject = 
	(*jniEnv)->CallObjectMethod(jniEnv, classObject, 
				    m_ClassGetDeclaredField, 
				    (*jniEnv)->NewStringUTF(jniEnv, 
							    probedVariable));
      if (!fieldObject)
	raiseEvent (SourceMessage,
		    "Can not find field to be probed in the Java class !\n"); 
      fieldObject = (*jniEnv)->NewGlobalRef (jniEnv, fieldObject);

      fieldType = (*jniEnv)->CallObjectMethod(jniEnv,
                                              fieldObject, 
					      m_FieldGetType);      
      if (!fieldType)
	raiseEvent (SourceMessage,
		    "Unknown type of probed field!\n");
      fieldType = (*jniEnv)->NewGlobalRef (jniEnv, fieldType);
      {
	//ugly!
          jboolean classp (jclass matchClass)
            {
              return (*jniEnv)->IsSameObject (jniEnv, fieldType, matchClass);
            }
          (char *) probedType = [aZone alloc: 1];
          if (classp (c_Object))
            ((char *) probedType)[0] = _C_ID;
          else if (classp (c_String))
            ((char *) probedType)[0] = _C_CHARPTR;
          else if (classp (c_int))
            ((char *) probedType)[0] = _C_INT;
          else if (classp (c_short))
            ((char *) probedType)[0] = _C_SHT;
          else if (classp (c_long))
            ((char *) probedType)[0] = _C_LNG;
          else if (classp (c_boolean))
            ((char *) probedType)[0] = _C_UCHR;
          else if (classp (c_byte))
            ((char *) probedType)[0] = _C_UCHR;
          else if (classp (c_char))
            ((char *) probedType)[0] = _C_CHR;
          else if (classp (c_float))
            ((char *) probedType)[0] = _C_FLT;
          else if (classp (c_double))
            ((char *) probedType)[0] = _C_DBL;
          else if (classp (c_void))
            ((char *) probedType)[0] = _C_VOID;
          else
            ((char *) probedType)[0] = _C_ID;
      }
    


      interactiveFlag = YES;
      return self;
    }
#endif   

  ivarList = probedClass->ivars;
  
  // search the ivar list for the requested variable.
  i = 0;
  while (i < ivarList->ivar_count
         && strcmp (ivarList->ivar_list[i].ivar_name, probedVariable) != 0)
    i++;
  
  if (i == ivarList->ivar_count)
    { 
      // if not found
      if (SAFEPROBES)
        raiseEvent (WarningMessage, "Warning: variable not found\n");
      return nil;
    }
  else
    {
      char type;

      probedType = ivarList->ivar_list[i].ivar_type;
      type = probedType[0];
      dataOffset = ivarList->ivar_list[i].ivar_offset;
      
      if (type == _C_CHARPTR
          || type == _C_CHR
          || type == _C_UCHR
          || type == _C_SHT
          || type == _C_USHT
          || type == _C_INT
          || type == _C_UINT
          || type == _C_LNG
          || type == _C_ULNG
          || type == _C_LNG_LNG
          || type == _C_ULNG_LNG
          || type == _C_FLT
          || type == _C_DBL
          || type == _C_LNG_DBL)
        interactiveFlag = YES;
      else
        interactiveFlag = NO;

      // set up default formatting string for floating point and 
      // double types - defaults are set in the probeLibrary instance
      if  (type == _C_FLT || type == _C_DBL || type == _C_LNG_DBL)
        {
          char *buf = [aZone alloc: 16];

          sprintf (buf, "%%.%dg", [probeLibrary getDisplayPrecision]); 
          floatFormat = buf; // allocate memory for string
        }
      else if (type == _C_ARY_B)
        {
          void setup_array (unsigned theRank,
                            unsigned *theDims,
                            const char *theBaseType)
            {
              size_t size = sizeof (unsigned) * theRank;
              
              rank = theRank;
              dims = [aZone alloc: size];
              memcpy (dims, theDims, size);
              baseType = theBaseType;
            }
          process_array (probedType,
                         setup_array,
                         NULL, NULL,
                         NULL, NULL,
                         NULL,
                         NULL,
                         NULL);
        }
      return self;
    }
}

PHASE(Setting)

- setNonInteractive
{
  interactiveFlag = NO;

  return self;
}

- setStringReturnType: returnType
{
  stringReturnType = returnType;
  return self;
}

- setFloatFormat: (const char *)format
{
  if (probedType[0] == _C_FLT
      || probedType[0] == _C_DBL
      || probedType[0] == _C_LNG_DBL) 
    floatFormat = STRDUP (format);
  else
    raiseEvent (WarningMessage, 
                "%s is not a float or double\n",
                probedVariable);

  return self;
}

PHASE(Using)

- (const char *)getProbedVariable
{
  return probedVariable;
}

- (BOOL)getInteractiveFlag
{
  return interactiveFlag;
}

- (int)getDataOffset
{
  return dataOffset;
}

- (unsigned)getRank
{
  return rank;
}

- (unsigned *)getDims
{
  return dims;
}

- (const char *)getBaseType
{
  return baseType;
}

- clone: aZone
{
  VarProbe *new_probe;
  
  new_probe = [VarProbe createBegin: aZone];
  [new_probe setProbedClass: probedClass];
  [new_probe setProbedVariable: probedVariable];
  if (objectToNotify != nil) 
    [new_probe setObjectToNotify: objectToNotify];
  new_probe = [new_probe createEnd];
  
  [new_probe setStringReturnType: stringReturnType];
  [new_probe setFloatFormat: floatFormat];
  
  return new_probe;
}

// no guarantees about alignment here.
- (void *)probeRaw: anObject
{
#ifdef HAVE_JDK
  if (isJavaProxy)
    raiseEvent (SourceMessage, "Java objects do not permit raw probing!\n");
#endif
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);
  return (void *) anObject + dataOffset;
}

- (void *)probeAsPointer: anObject
{
  void *p;
  void *q = NULL;
  
#ifdef HAVE_JDK
  if (isJavaProxy)
    raiseEvent (SourceMessage, 
		"Java objects do not permit probing with pointers!\n");
#endif

  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name],
                  [anObject name]);
  
  p = ((char *)anObject) + dataOffset;

  switch (probedType[0])
    {
    case _C_ID:       q = (void *) *(id *) p; break;
    case _C_CLASS:    q = (void *) *(Class *) p; break;
    case _C_CHARPTR:
    case _C_PTR:      q = (void *) *(void **) p; break;
    case _C_CHR:      q = (void *) (PTRUINT) *(char *) p; break;
    case _C_UCHR:     q = (void *) (PTRUINT) *(unsigned char *) p; break;
    case _C_SHT:      q = (void *) (PTRUINT) *(short *) p; break;
    case _C_USHT:     q = (void *) (PTRUINT) *(unsigned short *) p; break;
    case _C_INT:      q = (void *) (PTRUINT) *(int *) p; break;
    case _C_UINT:     q = (void *) (PTRUINT) *(unsigned int *) p; break;
    case _C_LNG:      q = (void *) (PTRUINT) *(long *) p; break;
    case _C_ULNG:     q = (void *) (PTRUINT) *(unsigned long *) p; break;
   default:
      if (SAFEPROBES)
        raiseEvent (WarningMessage,
                    "Invalid type %s to retrieve as a pointer...\n",
                    probedType);
      break;
    }
  return q;
}

#ifdef HAVE_JDK
#define _TYPEP(type) ((*jniEnv)->IsSameObject (jniEnv, fieldType, c_##type))
#define _GETVALUE(type,uptype) \
    (*jniEnv)->Call##uptype##Method (jniEnv, \
                                     field, \
                                     m_FieldGet##uptype, \
                                     object)
#define TYPEP(type) _TYPEP(type)
#define GETVALUE(type, uptype) _GETVALUE(type, uptype)

int
java_probe_as_int (jobject fieldType, jobject field, jobject object)
{
  int res;

  if (TYPEP (boolean))
    res = (int) GETVALUE (boolean, Boolean);
  else if (TYPEP (char))
    res = (int) GETVALUE (char, Char);
  else if (TYPEP (short))
    abort ();
  else if (TYPEP (int))
    res = (int) GETVALUE (int, Int);
  else if (TYPEP (long))
    res = (int) GETVALUE (long, Long);
  else if (TYPEP (float))
    res = (int) GETVALUE (float, Float);
  else if (TYPEP (double))
    res = (int) GETVALUE (double, Double);
  else if (TYPEP (Object))
    abort ();
  else
    abort ();

  return res;
}

#endif

static int
probe_as_int (const char *probedType, const void *p)
{
  int i = 0;
  
  switch (probedType[0])
    {
    case _C_ID:   i = (long) *(id *) p; break;
    case _C_CHARPTR:
    case _C_PTR:  i = (long) *(void **) p; break;
      
    case _C_UCHR: i = (int) *(unsigned char *) p; break;
    case _C_CHR:  i = (int) *(char *) p; break;

    case _C_SHT:  i = (int) *(short *) p; break;
    case _C_USHT: i = (unsigned) *(unsigned short *) p; break;
      
    case _C_INT:  i = *(int *) p; break;
    case _C_UINT: i = *(unsigned *) p; break;

    case _C_LNG:  i = (int) *(long *) p; break;
    case _C_ULNG: i = (unsigned) *(unsigned long *) p; break;

    case _C_LNG_LNG:  i = (int) *(long long *) p; break;
    case _C_ULNG_LNG: i = (unsigned) *(unsigned long long *) p; break;

    case _C_FLT:     i = (int) *(float *) p; break;
    case _C_DBL:     i = (int) *(double *) p; break;
    case _C_LNG_DBL: i = (int) *(long double *) p; break;
      
    default:
      if (SAFEPROBES)
        raiseEvent (WarningMessage,
                    "Invalid type `%s' to retrieve as an int...\n",
                    probedType);
      break;
    }
  return i;
}

- (int)probeAsInt: anObject
{
  const void *p;

#ifdef HAVE_JDK
  if (isJavaProxy)
    return java_probe_as_int (fieldType,
                              fieldObject,
                              SD_FINDJAVA (jniEnv, anObject));
#endif
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);
  
  p = ((const void *)anObject) + dataOffset;

  return probe_as_int (probedType, p);
}

#ifdef HAVE_JDK
double
java_probe_as_double (jobject fieldType, jobject field, jobject object)
{
  double res;

  if (TYPEP (boolean))
    res = (double) GETVALUE (boolean, Boolean);
  else if (TYPEP (char))
    res = (double) GETVALUE (char, Char);
  else if (TYPEP (short))
    abort ();
  else if (TYPEP (int))
    res = (double) GETVALUE (int, Int);
  else if (TYPEP (long))
    res = (double) GETVALUE (long, Long);
  else if (TYPEP (float))
    res = (double) GETVALUE (float, Float);
  else if (TYPEP (double))
    res = (double) GETVALUE (double, Double);
  else if (TYPEP (Object))
    abort ();
  else
    abort ();

  return res;
}
#endif

static double
probe_as_double (const char *probedType, const void *p)
{
  double d = 0.0;

  switch (probedType[0])
    {
    case _C_UCHR: d = (double) *(unsigned char *) p; break;
    case _C_CHR:  d = (double) *(char *) p; break;
      
    case _C_SHT:  d = (double) *(short *) p; break;
    case _C_USHT: d = (double) *(unsigned short *) p; break;

    case _C_INT:  d = (double) *(int *) p; break;
    case _C_UINT: d = (double) *(unsigned int *) p; break;
      
    case _C_LNG:  d = (double) *(long *) p; break;
    case _C_ULNG: d = (double) *(unsigned long *) p; break;

    case _C_LNG_LNG:  d = (double) *(long long *) p; break;
    case _C_ULNG_LNG: d = (double) *(unsigned long long *) p; break;
      
    case _C_FLT:  d = (double) *(float *) p; break;
    case _C_DBL:  d = (double) *(double *) p; break;
    case _C_LNG_DBL:  d = (double) *(long double *) p; break;
      
    default:
      if (SAFEPROBES)
        raiseEvent (WarningMessage,
                    "Invalid type `%s' to retrieve as a double...\n",
                    probedType);
      break;
    }
  return d;
}

- (double)probeAsDouble: anObject
{
  const void *p;

#ifdef HAVE_JDK
  if (isJavaProxy)
    return java_probe_as_double (fieldType,
                                 fieldObject,
                                 SD_FINDJAVA (jniEnv, anObject));
#endif  
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name],
                  [anObject name]);
  
  p = ((const void *)anObject) + dataOffset;

  return probe_as_double (probedType, p);
}

- (const char *)probeAsString: anObject Buffer: (char *)buf
{
  // by default - use precision set by -setFormatFloat 
  // as number of digits to use in formatting the string
  [self probeAsString: anObject Buffer: buf withFullPrecision: 0];
  return buf;
}

#ifdef HAVE_JDK
#define _GETSTROBJECT(type, uptype) \
  (*jniEnv)->CallStaticObjectMethod (jniEnv, \
                                     c_String, \
                                     m_StringValueOf##uptype,  \
                                     GETVALUE (type, uptype))
#define GETSTROBJECT(type, uptype) _GETSTROBJECT(type, uptype)

const char *
java_probe_as_string (jclass fieldType, jobject field, jobject object,
		      char *buf, int precision)
{
  jobject str;
  jboolean isCopy;
  const char *result;
  
  if (TYPEP (boolean))
    str = GETSTROBJECT (boolean, Boolean);
  else if (TYPEP (char))
    str = GETSTROBJECT (char, Char);
  else if (TYPEP (short))
    abort ();
  else if (TYPEP (int))
    str = GETSTROBJECT (int, Int);
  else if (TYPEP (long))
    str = GETSTROBJECT (long, Long);
  else if (TYPEP (float))
    str = GETSTROBJECT (float, Float);
  else if (TYPEP (double))
    str = GETSTROBJECT (double, Double);
  else
    str = GETSTROBJECT (object, Object);
  
  result = (*jniEnv)->GetStringUTFChars (jniEnv, str, &isCopy);
  strcpy (buf, result);
  if (isCopy)
    (*jniEnv)->ReleaseStringUTFChars (jniEnv, str, result);
  return buf;
}

#endif

- (const char *)probeAsString: anObject
                       Buffer: (char *)buf 
            withFullPrecision: (int)precision
{
  const void *p;
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      sprintf (buf, "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]);
  
#ifdef HAVE_JDK
  if (isJavaProxy)
    return java_probe_as_string (fieldType,
                                 fieldObject, 
                                 SD_FINDJAVA (jniEnv, anObject), 
                                 buf,
                                 precision);
#endif

  p = (const char *)anObject + dataOffset; // probeData
  
  switch (probedType[0])
    {
    case _C_ID:
      if (!(*(id *)p))
        sprintf (buf, "nil");
      else 
        {
          const char *name = NULL;
          
          if ([*(id *)p respondsTo: @selector (getDisplayName)])
            name = [*(id *) p getDisplayName];

          if (!name)
            name = [*(id *) p name];
          strcpy (buf, name);
        }
      break;
    case _C_CLASS:
      if (!(*(Class *) p))
        sprintf (buf, "nil");
      else
        sprintf (buf, "%s", (*(Class *) p)->name );
      break;
    case _C_PTR:
      sprintf (buf, "0x%p", *(void **) p);
      break;
    case _C_UCHR:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%u '%c'",(unsigned) *(unsigned char *) p,
                *(unsigned char *) p);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'",*(unsigned char *) p);
      else if (stringReturnType == IntString)
        sprintf (buf, "%u", (unsigned) *(unsigned char *) p);
      else
        raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case _C_CHR:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%d '%c'",(int) *(char *) p, *(char *) p);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'",*(char *) p);
      else if (stringReturnType == IntString)
        sprintf (buf, "%d",(int) *(char *) p);
      else
       raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case _C_SHT:
      sprintf (buf, "%hd", *(short *) p);
      break;
    case _C_USHT:
      sprintf (buf, "%hu", *(unsigned short *) p);
      break;
    case _C_INT:
      sprintf (buf, "%d", *(int *) p);
      break;
    case _C_UINT:
      sprintf (buf, "%u", *(unsigned *) p);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case _C_LNG_LNG:
#endif
    case _C_LNG:
      sprintf (buf, "%ld", *(long *)p);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case _C_ULNG_LNG:
#endif
    case _C_ULNG:
      sprintf (buf, "%lu", *(unsigned long *) p);
      break;
#if defined(LLFMT) && (SIZEOF_LONG_LONG > SIZEOF_LONG)
    case _C_LNG_LNG:
      sprintf (buf, "%" LLFMT "d", *(long long *)p);
      break;
    case _C_ULNG_LNG:
      sprintf (buf, "%" LLFMT "u", *(unsigned long long *) p);
      break;
#endif
    case _C_FLT:
      if (precision)
        sprintf (buf, "%.*g", [probeLibrary getSavedPrecision],
                 (double) (*(float *) p));
      else
        sprintf (buf, floatFormat, (double) (*(float *) p));
      break;
    case _C_DBL:
      if (precision)
        sprintf (buf, "%.*g", [probeLibrary getSavedPrecision],
                *(double *) p);
      else
        sprintf (buf, floatFormat, *(double *) p);
      break;
    case _C_LNG_DBL:
      if (precision)
        sprintf (buf, "%.*g", [probeLibrary getSavedPrecision],
                 (double) *(long double *) p);
      else
        sprintf (buf, floatFormat, (double) *(long double *) p);
      break;
    case _C_CHARPTR:
      sprintf (buf, "%s", *(char **) p ? *(char **) p : "<NULL>");
      break;
    default:
      sprintf (buf, "..."); 
      break;
    }
  return buf;
}

- iterateAsDouble: anObject using: (void (*) (unsigned rank, unsigned *vec, double val))func
{
  unsigned vec[rank];
  unsigned di;
  const void *ary = (const void *) anObject + dataOffset;
  
  void start_dim (unsigned dimnum)
    {
      vec[dimnum] = 0;
      di = dimnum;
    }
  void end_dim (void)
    {
      if (di > 0)
        vec[di - 1]++;
    }
  void end_element (void)
    {
      vec[di]++;
    }
  void output_type (const char *type, unsigned offset, void *data)
    {
      func (rank, vec, ((double *)ary)[offset]);
    }
  process_array (probedType,
                 NULL,
                 start_dim, end_dim,
                 NULL, end_element,
                 output_type,
                 ary,
                 NULL);
  return self;
}

- iterateAsInteger: anObject using: (void (*) (unsigned rank, unsigned *vec, int val))func
{
  unsigned vec[rank];
  unsigned di;
  const void *ary = (const void *) anObject + dataOffset;

  void start_dim (unsigned dimnum)
    {
      vec[dimnum] = 0;
      di = dimnum;
    }
  void end_dim (void)
    {
      if (di > 0)
        vec[di - 1]++;
    }
  void end_element (void)
    {
      vec[di]++;
    }
  void output_type (const char *type, unsigned offset, void *data)
    {
      func (rank, vec, ((int *) ary)[offset]);
    }

  process_array (probedType,
                 NULL,
                 start_dim, end_dim,
                 NULL, end_element,
                 output_type,
                 ary,
                 NULL);
  return self;
}

// sets the probed to whatever is pointed to by newValue. Use the
// type information to try to do this intelligently.
- setData: anObject To: (void *)newValue
{
  const void *p;

#ifdef HAVE_JDK
  if (isJavaProxy)
    raiseEvent (SourceMessage, "Setting probed fields in Java object from a void pointer to new value is not implemented!\n");
#endif

  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);
  
  p = (const char *)anObject + dataOffset;		  // probeData
  
  switch (probedType[0])
    {
    case _C_ID:   *(id *)p = *(id *)newValue; break;
    case _C_CHARPTR:
    case _C_PTR:  *(void **)p = *(void **)newValue; break;
      
    case _C_UCHR: *(unsigned char *)p = *(unsigned char *)newValue; break;
    case _C_CHR:  *(char *)p = *(char *)newValue; break;
    case _C_SHT:  *(short *)p = *(short *)newValue; break;
    case _C_USHT: *(unsigned short *)p = *(unsigned short *)newValue; break;
    case _C_INT:  *(int *)p = *(int *)newValue; break;
    case _C_UINT: *(unsigned int *)p = *(unsigned int *)newValue; break;
    case _C_LNG:  *(long *)p = *(long *)newValue; break;
    case _C_ULNG: *(unsigned long *)p = *(unsigned long *)newValue; break;
    case _C_LNG_LNG:  *(long long *)p = *(long long *)newValue; break;
    case _C_ULNG_LNG: *(unsigned long long *)p = *(unsigned long long *)newValue; break;
    case _C_FLT:  *(float *)p = *(float *)newValue; break;
    case _C_DBL:  *(double *)p = *(double *)newValue; break;
    case _C_LNG_DBL:  *(long double *)p = *(long double *)newValue; break;
      
    default:
      if (SAFEPROBES)
        raiseEvent (WarningMessage, "Invalid type `%s' to set\n", probedType);
      break;
    }
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;
          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: anObject
                       via: self
                       withProbeType: "VarProbe"
                       on: probedVariable
                       ofType: probedType[0]
                       withData: newValue];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: anObject
                        via: self
                        withProbeType: "VarProbe"
                        on: probedVariable
                        ofType: probedType[0]
                        withData: newValue];
    }
  return self;
}

#ifdef HAVE_JDK
void
setFieldFromString (id anObject, jobject field, 
		    jclass fieldType, const char * value)
{

  unsigned classcmp(jclass matchClass, jclass fieldType) 
    {
      return ((*jniEnv)->IsSameObject (jniEnv, fieldType, matchClass));
    }

  if (classcmp (fieldType, c_boolean))
    {
      jobject boolObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      boolObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Boolean, 
					   m_BooleanValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 boolObject);      
    }
  else if (classcmp (fieldType, c_char))
    {
      jchar javaChar = value[0];
      
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSetChar, 
				 SD_FINDJAVA (jniEnv, anObject),
				 javaChar);      
    }
  else if (classcmp (fieldType, c_byte))
    {
      jobject byteObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      byteObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Byte, 
					   m_ByteValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 byteObject);      
    }
  else if (classcmp (fieldType, c_int))
    {
      jobject intObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      intObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Integer, 
					   m_IntegerValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 intObject);      
    }
  else if (classcmp (fieldType, c_short))
    {
      jobject shortObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      shortObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Short, 
					   m_ShortValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 shortObject);      
    }
  else if (classcmp (fieldType, c_long))
    {
      jobject longObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      longObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Long, 
					   m_LongValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 longObject);      
    }
  else if (classcmp (fieldType, c_float))
    {
      jobject floatObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      floatObject = 
	(*jniEnv)->CallStaticObjectMethod (jniEnv, c_Float, 
					   m_FloatValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 floatObject);      
    }
  else if (classcmp (fieldType, c_double))
    {
      jobject doubleObject;
      jobject javaString;
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      doubleObject = 
 	(*jniEnv)->CallStaticObjectMethod (jniEnv,
                                           c_Double, 
					   m_DoubleValueOf,
					   javaString);        
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 doubleObject);
    }
  else if (classcmp (fieldType, c_String))
    {
      jobject javaString;
      
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_FINDJAVA (jniEnv, anObject),
				 javaString);      
    }
}
#endif

// sets data to the string passed in. Some duplicated code with
// setData:To:, but it's not too bad. Note we don't allow setting
// pointers here, because textual representations of pointers are
// strange. That's probably not a good idea.
- (BOOL)setData: anObject ToString: (const char *)s
{
  union {
    char c;
    short s;
    unsigned short us;
    int i;
    unsigned int ui;
    float f;
    double d;
    long double ld;
    long l;
    unsigned long ul;
    long long ll;
    unsigned long long ull;
  } value;
  int rc = 0;
  void *p;
#ifdef HAVE_JDK

  if (isJavaProxy)
    {
      setFieldFromString (anObject, fieldObject, fieldType, s);
      rc = 1;
    }
  else
    {
#endif  
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);

  p = (char *) anObject + dataOffset;		  // probeData

  switch (probedType[0])
    {
    case _C_CHR:
      if (stringReturnType == CharString)
        {
          if ((rc = sscanf (s, "'%c'", &value.c)) == 1)
            *(char *) p = value.c;
	} 
      else 
        {
	  if ((rc = sscanf (s, "%d", &value.i)) == 1)
	    *(char *) p = value.i;
	}
      break;
      
    case _C_UCHR:
      if (stringReturnType == CharString)
        {
          if ((rc = sscanf (s, "'%c'", &value.c)) == 1)
            *(char *) p = value.c;
        }
      else 
        {
          if ((rc = sscanf (s, "%u", &value.i)) == 1)
            *(unsigned char *) p = value.i;
	}
      break;
      
  case _C_CHARPTR:
      *(char **) p = STRDUP (s) ;
      rc = (*(char **) p != NULL);
      break;

    case _C_SHT:
      if ((rc = sscanf (s, "%hd", &value.s)) == 1) 
        *(short *) p = value.s; 
      break;
      
    case _C_USHT:
      if ((rc = sscanf (s, "%hu", &value.us)) == 1) 
        *(unsigned short *) p = value.us; 
      break;
      
    case _C_INT:
      if ((rc = sscanf (s, "%d", &value.i)) == 1) 
        *(int *) p = value.i; 
      break;
      
    case _C_UINT:
      if ((rc = sscanf (s, "%u", &value.ui)) == 1) 
        *(unsigned int *) p = value.ui; 
      break;

    case _C_LNG:
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case _C_LNG_LNG:
#endif
      if ((rc = sscanf (s, "%ld", &value.l)) == 1) 
        *(long *) p = value.i; 
      break;
      
    case _C_ULNG:
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case _C_ULNG_LNG:
#endif
      if ((rc = sscanf (s, "%lu", &value.ul)) == 1) 
        *(unsigned long *) p = value.ul; 
      break;

#if defined(LLFMT) && (SIZEOF_LONG_LONG > SIZEOF_LONG)
    case _C_LNG_LNG:
      if ((rc = sscanf (s, "%" LLFMT "d", &value.ll)) == 1) 
        *(long *) p = value.ll; 
      break;
      
    case _C_ULNG_LNG:
      if ((rc = sscanf (s, "%" LLFMT "u", &value.ull)) == 1) 
        *(unsigned long *) p = value.ull; 
      break;
#endif
      
    case _C_FLT:
      if ((rc = sscanf (s, "%f", &value.f)) == 1) 
        *(float *) p = value.f; 
      break;

    case _C_DBL:
      if ((rc = sscanf (s, "%lf", &value.d)) == 1) 
        *(double *) p = value.d; 
      break;

    case _C_LNG_DBL:
      {
        double val;
        
        if ((rc = sscanf (s, "%lf", &val)) == 1)
          {
            value.ld = val;
            *(double *) p = value.ld; 
          }
        break;
      }
    default:
      if (SAFEPROBES)
        raiseEvent (WarningMessage, "Invalid type %s to set\n", probedType);
      break;
  }

#ifdef HAVE_JDK
  // closes else branch  of if (isJavaProxy)
    }
#endif

  if (rc != 1 && SAFEPROBES)
    {
      raiseEvent (WarningMessage,
                  "Error scanning for value in string %s\n",
                  s);
      return NO;
    }
  
  if (objectToNotify != nil)
    {
      if ([objectToNotify respondsTo: M(forEach:)])
        {
          id index, tempObj;

          index = [objectToNotify begin: scratchZone];
          while ((tempObj = [index next]) != nil)
            {
              [tempObj eventOccurredOn: anObject
                       via: self
                       withProbeType: "VarProbe"
                       on: probedVariable
                       ofType: probedType[0]
                       withData: (void *)s];
            }
          [index drop];
        }
      else 
        [objectToNotify eventOccurredOn: anObject
                        via: self
                        withProbeType: "VarProbe"
                        on: probedVariable
                        ofType: probedType[0]
                        withData: (void *)s];
    }
  return YES;
}

- (void)drop
{
#ifdef HAVE_JDK
  if (isJavaProxy)
    {
      (*jniEnv)->DeleteGlobalRef (jniEnv, fieldObject);
      (*jniEnv)->DeleteGlobalRef (jniEnv, fieldType);
      (*jniEnv)->DeleteGlobalRef (jniEnv, classObject);
    }

#endif

  if (probedVariable)
    FREEBLOCK (probedVariable);
  if (dims)
    FREEBLOCK (dims);
  [super drop];
}

@end
