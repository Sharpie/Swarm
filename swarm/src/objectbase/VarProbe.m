// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/VarProbe.h>
#import <defobj.h> // raiseEvent, WarningMessage, STRDUP, FREEBLOCK
#import <defobj/defalloc.h> // getZone

#include <objc/objc-api.h>
#include <misc.h> // strcmp, strcpy, sprintf, sscanf

#include <defobj/internal.h> // objc_process_array

#include <swarmconfig.h> // HAVE_JDK, PTRUINT

#ifdef HAVE_JDK
#import "../defobj/java.h" // SD_JAVA_FIND_OBJECT_JAVA, SD_JAVA_FIND_OBJECT_OBJC, JNI
#import "../defobj/javavars.h" // c_*, m_*
#endif

#import "local.h"

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
        FREEBLOCK (probedVariable);
    }
  probedVariable = STRDUP (aVariable);
  return self;
}

- setProbedCOMgetter: (COMmethod)getter setter: (COMmethod)setter
{
  getterMethod = getter;
  setterMethod = setter;
  language = LanguageCOM;
  return self;
}

- (void)_typeSetup_
{
  switch (fcall_type_for_objc_type (probedType[0]))
    {
    case fcall_type_float:
    case fcall_type_double:
    case fcall_type_long_double:
      {
        // set up default formatting string for floating point and 
        // double types - defaults are set in the probeLibrary instance
        char *buf = [getZone (self) alloc: 16];
        
        sprintf (buf, "%%.%dg", [probeLibrary getDisplayPrecision]); 
        floatFormat = buf; // allocate memory for string
      }
    case fcall_type_boolean:
    case fcall_type_string:
    case fcall_type_schar:
    case fcall_type_uchar:
    case fcall_type_sshort:
    case fcall_type_ushort:
    case fcall_type_sint:
    case fcall_type_uint:
    case fcall_type_slong:
    case fcall_type_ulong:
    case fcall_type_slonglong:
    case fcall_type_ulonglong:
      interactiveFlag = YES;
      break;
    case fcall_type_object:
    case fcall_type_class:
    case fcall_type_selector:
    case fcall_type_jobject:
    case fcall_type_jstring:
    case fcall_type_void:
      interactiveFlag = NO;
      break;
    }
}

#ifdef HAVE_JDK
- _setupJavaVarProbe_
{
  jobject lref;

  fieldObject = 0;
  classObject = SD_JAVA_FIND_OBJECT_JAVA (probedClass);
  if (!classObject)
    raiseEvent (SourceMessage,
                "Java class to be probed cannot be found.\n");      
  {
    jobject probedVariableStr = (*jniEnv)->NewStringUTF (jniEnv, 
                                                         probedVariable);
    lref = 
      (*jniEnv)->CallObjectMethod (jniEnv,
                                   classObject, 
                                   m_ClassGetField,
                                   probedVariableStr);
    (*jniEnv)->DeleteLocalRef (jniEnv, probedVariableStr);
  }
  if (lref)
    fieldObject = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  
  lref = (*jniEnv)->CallObjectMethod (jniEnv,
                                      fieldObject, 
                                      m_FieldGetType);      
  if (!lref)
    raiseEvent (SourceMessage, "Unknown type of probed field `%s'\n",
                probedVariable);
  
  fieldType = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);

  probedType = objc_type_for_fcall_type (fcall_type_for_java_class (fieldType));
  [self _typeSetup_];
  return fieldObject ? self : nil;
}
#endif

- _setupCOMVarProbe_
{
  probedType =
    objc_type_for_fcall_type (COM_method_param_fcall_type (getterMethod, 0));
  [self _typeSetup_];
  return self;
}

- _setupObjcVarProbe_
{
  IvarList_t ivarList;
  unsigned i;
  
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
        raiseEvent (WarningMessage,
                    "Warning: variable `%s' not found\n",
                    probedVariable);
      return nil;
    }
  else
    {
      probedType = ivarList->ivar_list[i].ivar_type;
      [self _typeSetup_]; 
      dataOffset = ivarList->ivar_list[i].ivar_offset;

      if (*probedType == _C_ARY_B)
        {
          void setup_array (unsigned theRank,
                            unsigned *theDims,
                            fcall_type_t theBaseType)
            {
              size_t size = sizeof (unsigned) * theRank;
              
              rank = theRank;
              dims = [getZone (self) alloc: size];
              memcpy (dims, theDims, size);
              baseType = objc_type_for_fcall_type (theBaseType);
            }
          char objcArraySubtype = *objc_array_subtype (probedType, NULL);
          
          if (objcArraySubtype == _C_UNION_B 
              || objcArraySubtype == _C_STRUCT_B)
            {
              rank = 0;
              dims = NULL;
              baseType = objc_type_for_fcall_type (fcall_type_void);
              raiseEvent (WarningMessage,
                          "Probing of unions and structs not supported.\n"
                          "[class: `%s', variable: `%s']\n",
                          probedClass->name, probedVariable);
            }
          else
            objc_process_array (probedType,
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

- createEnd
{
  [super createEnd];

  if (SAFEPROBES)
    if (!(probedVariable || (getterMethod && setterMethod)) || !probedClass)
      raiseEvent (WarningMessage, 
                  "VarProbe object was not properly initialized.\n");

  if (language == LanguageCOM)
    return [self _setupCOMVarProbe_];
#ifdef HAVE_JDK
  else if (language == LanguageJava)
    return [self _setupJavaVarProbe_];
#endif   
  else if (language == LanguageObjc)
    return [self _setupObjcVarProbe_];
    abort ();
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
  if (language == LanguageJava || language == LanguageCOM)
    raiseEvent (SourceMessage,
                "COM & Java objects do not permit raw probing.\n");
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
  if (language == LanguageJava || language == LanguageCOM)
    raiseEvent (SourceMessage, 
		"COM & Java objects do not permit probing with pointers.\n");
#endif

  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name],
                  [anObject name]);
  
  p = ((char *) anObject) + dataOffset;

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
                    "Invalid type `%s' to retrieve as a pointer...\n",
                    probedType);
      break;
    }
  return q;
}

#ifdef HAVE_JDK
#define _TYPEP(type) ((*jniEnv)->IsSameObject (jniEnv, fieldType, c_##type))
#define _GETVALUE(uptype) \
    (*jniEnv)->Call##uptype##Method (jniEnv, \
                                     field, \
                                     m_FieldGet##uptype, \
                                     object)
#define TYPEP(type) _TYPEP(type)
#define GETVALUE(uptype) _GETVALUE(uptype)

int
java_probe_as_int (jobject fieldType, jobject field, jobject object)
{
  int res;

  if (TYPEP (boolean))
    res = (int) GETVALUE (Boolean);
  else if (TYPEP (char))
    res = (int) GETVALUE (Char);
  else if (TYPEP (short))
    abort ();
  else if (TYPEP (int))
    res = (int) GETVALUE (Int);
  else if (TYPEP (long))
    res = (int) GETVALUE (Long);
  else if (TYPEP (float))
    res = (int) GETVALUE (Float);
  else if (TYPEP (double))
    res = (int) GETVALUE (Double);
  else if (TYPEP (Object))
    abort ();
  else
    abort ();

  return res;
}

#endif

#define CONVERT(ftype, type)                              \
  switch (ftype)                                          \
    {                                                     \
    case fcall_type_void:                                 \
    case fcall_type_object:                               \
    case fcall_type_class:                                \
    case fcall_type_string:                               \
    case fcall_type_selector:                             \
    case fcall_type_jobject:                              \
    case fcall_type_jstring:                              \
      abort ();                                           \
    case fcall_type_boolean:                              \
      ret = (type) p->boolean;                            \
      break;                                              \
    case fcall_type_uchar:                                \
      ret = (type) p->uchar;                              \
      break;                                              \
    case fcall_type_schar:                                \
      ret = (type) p->schar;                              \
      break;                                              \
    case fcall_type_ushort:                               \
      ret = (type) p->ushort;                             \
      break;                                              \
    case fcall_type_sshort:                               \
      ret = (type) p->sshort;                             \
      break;                                              \
    case fcall_type_uint:                                 \
      ret = (type) p->uint;                               \
      break;                                              \
    case fcall_type_sint:                                 \
      ret = (type) p->sint;                               \
      break;                                              \
    case fcall_type_ulong:                                \
      ret = (type) p->ulong;                              \
      break;                                              \
    case fcall_type_slong:                                \
      ret = (type) p->slong;                              \
      break;                                              \
    case fcall_type_ulonglong:                            \
      ret = (type) p->ulonglong;                          \
      break;                                              \
    case fcall_type_slonglong:                            \
      ret = (type) p->slonglong;                          \
      break;                                              \
    case fcall_type_float:                                \
      ret = (type) p->_float;                             \
      break;                                              \
    case fcall_type_double:                               \
      ret = (type) p->_double;                            \
      break;                                              \
    case fcall_type_long_double:                          \
      ret = (type) p->_long_double;                       \
      break;                                              \
    }

#define COM_CONVERT(type) CONVERT (COM_method_param_fcall_type (getterMethod, 0), type)
#define OBJC_CONVERT(type) CONVERT (fcall_type_for_objc_type (probedType[0]), type)

static int
COM_probe_as_int (COMmethod getterMethod, COMobject cObj)
{
  void *params = COM_create_params (1);
  types_t retBuf;
  types_t *p = &retBuf;
  int ret = 0;

  COM_set_return (params, 0, fcall_type_sint, p);
  COM_method_invoke (getterMethod, cObj, params);

  COM_CONVERT (int);
  
  COM_free_params (params);
  return retBuf.sint;
}

static int
objc_probe_as_int (const char *probedType, const types_t *p)
{
  int ret = 0;

  OBJC_CONVERT (int);

  return ret;
}

- (int)probeAsInt: anObject
{
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);
  if (language == LanguageCOM)
    return COM_probe_as_int (getterMethod, SD_COM_FIND_OBJECT_COM (anObject));
#ifdef HAVE_JDK
  else if (language == LanguageJava)
    return java_probe_as_int (fieldType,
                              fieldObject,
                              SD_JAVA_FIND_OBJECT_JAVA (anObject));
#endif
  else if (language == LanguageObjc)
    return objc_probe_as_int (probedType, (types_t *) ((void *) anObject) + dataOffset);
  else
    abort ();
}

#ifdef HAVE_JDK
double
java_probe_as_double (jobject fieldType, jobject field, jobject object)
{
  double res;

  if (TYPEP (boolean))
    res = (double) GETVALUE (Boolean);
  else if (TYPEP (char))
    res = (double) GETVALUE (Char);
  else if (TYPEP (short))
    abort ();
  else if (TYPEP (int))
    res = (double) GETVALUE (Int);
  else if (TYPEP (long))
    res = (double) GETVALUE (Long);
  else if (TYPEP (float))
    res = (double) GETVALUE (Float);
  else if (TYPEP (double))
    res = (double) GETVALUE (Double);
  else if (TYPEP (Object))
    abort ();
  else
    abort ();

  return res;
}
#endif


static double
COM_probe_as_double (COMmethod getterMethod, COMobject cObj)
{
  void *params = COM_create_params (1);
  types_t retBuf;
  const types_t *p = &retBuf;
  double ret = 0.0;

  COM_method_set_return (getterMethod, params, &retBuf);
  COM_method_invoke (getterMethod, cObj, params);

  COM_CONVERT (double);

  COM_free_params (params);
  return ret;
}

static double
objc_probe_as_double (const char *probedType, const types_t *p)
{
  double ret = 0.0;

  OBJC_CONVERT (double);
  
  return ret;
}

- (double)probeAsDouble: anObject
{
  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name],
                  [anObject name]);
  
  if (language == LanguageCOM)
    return COM_probe_as_double (getterMethod,
                                SD_COM_FIND_OBJECT_COM (anObject));
#ifdef HAVE_JDK
  else if (language == LanguageJava)
    return java_probe_as_double (fieldType,
                                 fieldObject,
                                 SD_JAVA_FIND_OBJECT_JAVA (anObject));
#endif  
  else if (language == LanguageObjc)
    return objc_probe_as_double (probedType,
                                 (const types_t *)
                                 ((const void *) anObject) + dataOffset);
  else
    abort ();
}

- (const char *)probeAsString: anObject Buffer: (char *)buf
{
  // by default - use precision set by -setFormatFloat 
  // as number of digits to use in formatting the string
  [self probeAsString: anObject Buffer: buf withFullPrecision: NO];
  return buf;
}

- (id <String>)probeAsString: anObject
{
  char buf[1024];

  [self probeAsString: anObject Buffer: buf withFullPrecision: NO];
  return [String create: getZone (self) setC: buf];
}

#ifdef HAVE_JDK
#if 0
// This approach stopped working for some reason with JDK (2000-08-11, mgd)

#define _GETSTROBJECT(type, uptype) \
  (*jniEnv)->CallStaticObjectMethod (jniEnv, \
                                     c_String, \
                                     m_StringValueOf##uptype,  \
                                     GETVALUE (uptype))
#define GETSTROBJECT(type, uptype) _GETSTROBJECT(type, uptype)
#endif

#define GETSTR(type,uptype,sig,fmt, fmttype)                          \
   {                                                                  \
     type val;                                                        \
     char buf[64];                                                    \
                                                                      \
     fid = (*jniEnv)->GetFieldID (jniEnv, class, fieldName, sig);     \
     val = (*jniEnv)->Get##uptype##Field (jniEnv, object, fid);       \
                                                                      \
     sprintf (buf, fmt, (fmttype) val);                               \
     str = (*jniEnv)->NewStringUTF (jniEnv, buf);                     \
   }

#define GETSTRFULLPREC(type,uptype,sig)                               \
   {                                                                  \
     type val;                                                        \
     char buf[64];                                                    \
                                                                      \
     fid = (*jniEnv)->GetFieldID (jniEnv, class, fieldName, sig);     \
     val = (*jniEnv)->Get##uptype##Field (jniEnv, object, fid);       \
                                                                      \
     sprintf (buf, "%.*g", (int) precision, (double) val);            \
     str = (*jniEnv)->NewStringUTF (jniEnv, buf);                     \
   }


void
java_probe_as_string (jclass fieldType, jobject field, jobject object,
                      const char *fmt, unsigned precision,
		      char *buf)
{
  jobject str;
  jboolean isCopy;
  const char *result;
  jfieldID fid;
  jstring name = (*jniEnv)->CallObjectMethod (jniEnv,
                                              field,
                                              m_FieldGetName);
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, object);

  const char *fieldName = java_copy_string (name);
  
  (*jniEnv)->DeleteLocalRef (jniEnv, name);

  if (TYPEP (boolean))
    {
      fid = (*jniEnv)->GetFieldID (jniEnv, class, fieldName, "Z");
      if ((*jniEnv)->GetBooleanField (jniEnv, object, fid))
        str = (*jniEnv)->NewStringUTF (jniEnv, "true");
      else
        str = (*jniEnv)->NewStringUTF (jniEnv, "false");
    }
  else if (TYPEP (byte))
    GETSTR (unsigned char, Byte, "B", "%u", unsigned)
  else if (TYPEP (char))
    GETSTR (char, Char, "C", "%c", char)
  else if (TYPEP (short))
    GETSTR (short, Short, "S", "%hd", short)
  else if (TYPEP (int))
    GETSTR (int, Int, "I", "%d", int)
  else if (TYPEP (long))
    GETSTR (long, Long, "J", "%ld", long)
  else if (TYPEP (float))
    if (fmt)
      GETSTR (float, Float, "F", fmt, float)
    else
      GETSTRFULLPREC (float, Float, "F")
  else if (TYPEP (double))
    if (fmt)
      GETSTR (double, Double, "D", fmt, double)
    else
      GETSTRFULLPREC (double, Double, "D")
  else if (TYPEP (String))
    {
      fid = (*jniEnv)->GetFieldID (jniEnv,
                                   class,
                                   fieldName,
                                   "Ljava/lang/String;");

      str = (*jniEnv)->GetObjectField (jniEnv, object, fid);
    }
  else
    str = (*jniEnv)->CallObjectMethod (jniEnv, fieldType, m_ClassGetName);
  result = (*jniEnv)->GetStringUTFChars (jniEnv, str, &isCopy);
  strcpy (buf, result);
  if (isCopy)
    (*jniEnv)->ReleaseStringUTFChars (jniEnv, str, result);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  (*jniEnv)->DeleteLocalRef (jniEnv, str);
  SFREEBLOCK (fieldName);
}

#endif

static void
string_convert (fcall_type_t type, const types_t *p,
                const char *floatFormat, unsigned precision,
                id <Symbol> stringReturnType,
                char *buf)
{
  switch (type)
    {
    case fcall_type_object:
      if (!p->object)
        sprintf (buf, "nil");
      else 
        {
          const char *name = NULL;
          
          if ([p->object respondsTo: @selector (getDisplayName)])
            name = [p->object getDisplayName];

          if (!name)
            name = [p->object name];
          strcpy (buf, name);
        }
      break;
    case fcall_type_class:
      if (!p->_class)
        sprintf (buf, "nil");
      else
        sprintf (buf, "%s", p->_class->name);
      break;
    case fcall_type_uchar:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%u '%c'", 
                 (unsigned) p->uchar,
                 p->uchar);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'", p->uchar);
      else if (stringReturnType == IntString)
        sprintf (buf, "%u", (unsigned) p->uchar);
      else
        raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case fcall_type_schar:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%d '%c'",
                 (int) p->schar,
                 p->schar);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'", p->schar);
      else if (stringReturnType == IntString)
        sprintf (buf, "%d",(int) p->schar);
      else
       raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case fcall_type_ushort:
      sprintf (buf, "%hu", p->ushort);
      break;
    case fcall_type_sshort:
      sprintf (buf, "%hd", p->sshort);
      break;
    case fcall_type_sint:
      sprintf (buf, "%d", p->sint);
      break;
    case fcall_type_uint:
      sprintf (buf, "%u", p->uint);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_ulonglong:
#endif
    case fcall_type_ulong:
      sprintf (buf, "%lu", p->ulong);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_slonglong:
#endif
    case fcall_type_slong:
      sprintf (buf, "%ld", p->slong);
      break;
#if defined(LLFMT) && (SIZEOF_LONG_LONG > SIZEOF_LONG)
    case fcall_type_slonglong:
      sprintf (buf, "%" LLFMT "d", p->slonglong);
      break;
    case fcall_type_ulonglong:
      sprintf (buf, "%" LLFMT "u", p->ulonglong);
      break;
#endif
    case fcall_type_float:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision,
                 (double) p->_float);
      else
        sprintf (buf, floatFormat, p->_float);
      break;
    case fcall_type_double:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision, p->_double);
      else
        sprintf (buf, floatFormat, p->_double);
      break;
    case fcall_type_long_double:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision,
                 (double) p->_long_double);
      else
        sprintf (buf, floatFormat, (double) p->_long_double);
      break;
    case fcall_type_string:
      sprintf (buf, "%s", p->string ? p->string : "<NULL>");
      break;
    default:
      sprintf (buf, "..."); 
      break;
    }
}

static void
COM_probe_as_string (COMmethod getterMethod,
                     COMobject cObj,
                     const char *fmt, unsigned precision,
                     id <Symbol> stringReturnType,
                     char *buf)
{
  void *params = COM_create_params (1);
  types_t retBuf;

  COM_set_return (params, 0, fcall_type_string, &retBuf);
  COM_method_invoke (getterMethod, cObj, params);

  string_convert (COM_method_param_fcall_type (getterMethod, 0),
                  &retBuf,
                  fmt, precision,
                  stringReturnType,
                  buf);

  COM_free_params (params);
}

- (const char *)probeAsString: anObject
                       Buffer: (char *)buf 
            withFullPrecision: (BOOL)fullPrecisionFlag
{
  const char *fmt = fullPrecisionFlag ? NULL : floatFormat;
  unsigned precision = [probeLibrary getSavedPrecision];
  
  if (safety)
    if (![anObject isKindOf: probedClass])
      sprintf (buf, "VarProbe for class %s tried on class %s\n",
               [probedClass name], [anObject name]);
  
  if (language == LanguageCOM)
    COM_probe_as_string (getterMethod,
                         SD_COM_FIND_OBJECT_COM (anObject),
                         fmt, precision,
                         stringReturnType,
                         buf);
#ifdef HAVE_JDK
  else if (language == LanguageJava)
    java_probe_as_string (fieldType,
                          fieldObject, 
                          SD_JAVA_FIND_OBJECT_JAVA (anObject), 
                          fmt, precision,
                          buf);
#endif
  else if (language == LanguageObjc)
    string_convert (fcall_type_for_objc_type (probedType [0]),
                    (void *) anObject + dataOffset,
                    fmt, precision,
                    stringReturnType,
                    buf);
  else
    abort ();
  return buf;
}
  
#ifdef HAVE_JDK
id
java_probe_as_object (jclass fieldType, jobject field, jobject object)
{
  fcall_type_t type = fcall_type_for_java_class (fieldType);
  jobject jobj;
  id ret;

  if (type != fcall_type_object)
    raiseEvent (WarningMessage,
                "Invalid type `%c' to retrieve object from a Java object",
                type);

  jobj = GETVALUE (Object);
  ret = SD_JAVA_FIND_OBJECT_OBJC (jobj);
  (*jniEnv)->DeleteLocalRef (jniEnv, jobj);
  return ret;
}
#endif


static id
COM_probe_as_object (COMmethod getterMethod, COMobject cObj)
{
  void *params = COM_create_params (1);
  types_t retBuf;
  id ret;

  COM_method_set_return (getterMethod, params, &retBuf);
  COM_method_invoke (getterMethod, cObj, params);

  ret = SD_COM_FIND_OBJECT_OBJC (retBuf.object);  // a pointer, anyway
  
  COM_free_params (params);
  return ret;
}


- probeObject: anObject
{
  if (probedType[0] != _C_ID)
    raiseEvent (WarningMessage,
                "Invalid type `%s' to retrieve as an object",
                probedType);

#ifdef HAVE_JDK
  if (language == LanguageJava)
    return java_probe_as_object (fieldType,
                                 fieldObject,
                                 SD_JAVA_FIND_OBJECT_JAVA (anObject));
#endif
  if (language == LanguageCOM)
    return COM_probe_as_object (getterMethod, SD_COM_FIND_OBJECT_COM (anObject));
  else if (language == LanguageObjc)
    return *(id *) [self probeRaw: anObject];
  else
    abort ();
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
  void output_type (fcall_type_t type, unsigned offset, void *data)
    {
      func (rank, vec, ((double *) ary)[offset]);
    }
  objc_process_array (probedType,
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
  void output_type (fcall_type_t type, unsigned offset, void *data)
    {
      func (rank, vec, ((int *) ary)[offset]);
    }

  objc_process_array (probedType,
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
  if (language == LanguageJava)
    raiseEvent (SourceMessage,
                "Setting probed fields in Java object from a void pointer to new value is not implemented.\n");
#endif

  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);
  
  p = (const char *)anObject + dataOffset;		  // probeData
  
  switch (probedType[0])
    {
    case _C_ID:   *(id *) p = *(id *) newValue; break;
    case _C_CHARPTR:
    case _C_PTR:  *(void **) p = *(void **) newValue; break;
      
    case _C_UCHR: *(unsigned char *) p = *(unsigned char *) newValue; break;
    case _C_CHR:  *(char *) p = *(char *) newValue; break;
    case _C_SHT:  *(short *) p = *(short *) newValue; break;
    case _C_USHT: *(unsigned short *) p = *(unsigned short *) newValue; break;
    case _C_INT:  *(int *) p = *(int *) newValue; break;
    case _C_UINT: *(unsigned int *) p = *(unsigned int *) newValue; break;
    case _C_LNG:  *(long *) p = *(long *) newValue; break;
    case _C_ULNG: *(unsigned long *) p = *(unsigned long *) newValue; break;
    case _C_LNG_LNG:  *(long long *) p = *(long long *) newValue; break;
    case _C_ULNG_LNG:
      *(unsigned long long *) p = *(unsigned long long *) newValue; break;
    case _C_FLT:  *(float *) p = *(float *) newValue; break;
    case _C_DBL:  *(double *) p = *(double *) newValue; break;
    case _C_LNG_DBL:  *(long double *) p = *(long double *) newValue; break;
      
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
static void
java_setFieldFromString (id anObject, jobject field, 
                         jclass fieldType, const char * value)
{

  unsigned classcmp (jclass matchClass, jclass fieldType) 
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 boolObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, boolObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
    }
  else if (classcmp (fieldType, c_char))
    {
      jchar javaChar = value[0];
      
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSetChar, 
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 byteObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, byteObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
      
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 intObject);      
      (*jniEnv)->DeleteLocalRef (jniEnv, intObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 shortObject);      
      (*jniEnv)->DeleteLocalRef (jniEnv, shortObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 longObject);      
      (*jniEnv)->DeleteLocalRef (jniEnv, longObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 floatObject);      
      (*jniEnv)->DeleteLocalRef (jniEnv, floatObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
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
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 doubleObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, doubleObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
    }
  else if (classcmp (fieldType, c_String))
    {
      jobject javaString;
      
      javaString = (*jniEnv)->NewStringUTF (jniEnv, value);
      
      (*jniEnv)->CallVoidMethod (jniEnv, field, m_FieldSet,
				 SD_JAVA_FIND_OBJECT_JAVA (anObject),
				 javaString);      
      (*jniEnv)->DeleteLocalRef (jniEnv, javaString);
    }
}
#endif


BOOL
convert_from_string (fcall_type_t type,
                     id <Symbol> stringReturnType,
                     const char *s,
                     types_t *out)
{
  BOOL ret = NO;

  switch (type)
    {
    case fcall_type_boolean:
      out->boolean = (strcmp (s, "true") == 0);
      ret = YES;
      break;

    case fcall_type_uchar:
      if (stringReturnType == CharString)
        ret = (sscanf (s, "'%c'", &out->uchar) == 1);
      else 
        {
          unsigned val;
          
          ret = (sscanf (s, "%u", &val) == 1);
          out->uchar = (unsigned char) val;
	}
      break;

    case fcall_type_schar:
      if (stringReturnType == CharString)
        ret = (sscanf (s, "'%c'", &out->schar) == 1);
      else 
        {
         int val;
         
         ret = (sscanf (s, "%d", &val) == 1);
         out->schar = (char) ret;
	}
      break;

    case fcall_type_ushort:
      ret = (sscanf (s, "%hu", &out->ushort) == 1);
      break;
      
    case fcall_type_sshort:
      ret = (sscanf (s, "%hd", &out->sshort) == 1);
      break;

    case fcall_type_uint:
      ret = (sscanf (s, "%u", &out->uint) == 1);
      break;
      
    case fcall_type_sint:
      ret = (sscanf (s, "%d", &out->sint) == 1);
      break;

    case fcall_type_ulong:
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_ulonglong:
#endif
      ret = (sscanf (s, "%lu", &out->ulong) == 1);
      break;

    case fcall_type_slong:
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_slonglong:
#endif
      ret = (sscanf (s, "%ld", &out->slong) == 1);
      break;
      
#if defined(LLFMT) && (SIZEOF_LONG_LONG > SIZEOF_LONG)
    case fcall_type_ulonglong:
      ret = (sscanf (s, "%" LLFMT "u", &out->ulonglong) == 1);
      break;

    case fcall_type_slonglong:
      ret = (sscanf (s, "%" LLFMT "d", &out->slonglong) == 1);
      break;
#endif

    case fcall_type_float:
      ret = (sscanf (s, "%f", &out->_float) == 1);
      break;

    case fcall_type_double:
      ret = (sscanf (s, "%lf", &out->_double) == 1);
      break;

    case fcall_type_long_double:
      {
        double val;
        
        ret = (sscanf (s, "%lf", &val) == 1);
        out->_long_double = (long double) val;
      }
      break;
      
    case fcall_type_string:
      out->string = SSTRDUP (s);
      ret = YES;
      break;

    case fcall_type_void:
    case fcall_type_object:
    case fcall_type_class:
    case fcall_type_selector:
    case fcall_type_jobject:
    case fcall_type_jstring:
      abort ();
    }

  return ret;
}

// sets data to the string passed in. Some duplicated code with
// setData:To:, but it's not too bad. Note we don't allow setting
// pointers here, because textual representations of pointers are
// strange. That's probably not a good idea.
- (BOOL)setData: anObject ToString: (const char *)s
{
  BOOL ret;

  if (safety)
    if (![anObject isKindOf: probedClass])
      raiseEvent (WarningMessage,
                  "VarProbe for class %s tried on class %s\n",
                  [probedClass name], [anObject name]);

  if (language == LanguageCOM)
    {
      void *params = COM_create_params (1);
      types_t val;
      fcall_type_t type = COM_method_param_fcall_type (setterMethod, 0);

      ret = convert_from_string (type, stringReturnType, s, &val);
      COM_set_arg (params, 0, type, &val);
      COM_method_invoke (setterMethod, SD_COM_FIND_OBJECT_COM (anObject), params);
      COM_free_params (params);
    }
#ifdef HAVE_JDK
  else if (language == LanguageJava)
    {
      java_setFieldFromString (anObject, fieldObject, fieldType, s);
      ret = YES;
    }
#endif
  else if (language == LanguageObjc)
    ret = convert_from_string (fcall_type_for_objc_type (probedType[0]),
                               stringReturnType,
                               s, (void *) anObject + dataOffset);
  else
    abort ();
  
  if (!ret && SAFEPROBES)
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
                       withData: (void *) s];
            }
          [index drop];
        }
      else
        [objectToNotify eventOccurredOn: anObject
                        via: self
                        withProbeType: "VarProbe"
                        on: probedVariable
                        ofType: probedType[0]
                        withData: (void *) s];
    }
  return YES;
}

- (void)drop
{
#ifdef HAVE_JDK
  if (language == LanguageJava)
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

- (void)describe: stream
{
  [super describe: stream];
  [stream catC: "variableName: "];
  [stream catC: probedVariable];
  [stream catC: "\n"];
}

@end
