#include <swarmconfig.h>
#ifdef HAVE_JDK
#import "java.h"
#import <defobj/JavaProxy.h>
#import <defobj/JavaClassProxy.h>
#import <defobj/JavaCollection.h>
#import <defobj/defalloc.h>
#include <misc.h>

// mframe_build_signature
#ifdef GNUSTEP
#include <Foundation/NSMethodSignature.h>
#else
#ifdef USE_MFRAME
#include <objc/mframe.h>
#endif
#endif

#import "internal.h" // FCALL_TYPE_COUNT, objc_type_for_fcall_type

#define extern
#import "javavars.h"
#undef extern

#include <misc.h> // stpcpy

static BOOL initFlag = NO;

static jobject proxyClassLoader = 0;

static const char *java_type_signature[FCALL_TYPE_COUNT] = {
  "V", "C", "C", "C", "S", "S",
  "I", "I", "J", "J", 

  /* bogus */
  "J", "J",

  "F", "D",
  "D", // long double
  "X", // Object
  "X", // Class
  "Ljava/lang/String;", 
  "Lswarm/Selector;",
  "Ljava/lang/Object;",
  "Ljava/lang/String;",
  "Lswarm/Selector;",
  "X" // iid
};

externvar id id_JavaClassProxy;


const char *
java_signature_for_fcall_type (fcall_type_t type)
{
  return java_type_signature[type];
}

static BOOL
fcall_type_for_java_signature (const char *signature, fcall_type_t *typeptr)
{
  unsigned i;

  for (i = 0; i < FCALL_TYPE_COUNT; i++)
    if (strcmp (signature, java_type_signature[i]) == 0)
      {
        if (i == fcall_type_uint || i == fcall_type_ushort)
          continue;
        *typeptr = i;
        return YES;
      }
  return NO;
}

static void
fill_signature (char *buf, const char *className)
{
  char *bp = buf;
  const char *cp = className;

  *bp++ = 'L';
  while (*cp)
    {
      *bp = (*cp == '.') ? '/' : *cp;
      bp++;
      cp++;
    }
  *bp++ = ';';
  *bp = '\0';
}

static BOOL
exactclassp (jclass class, jclass matchClass)
{
  return (*jniEnv)->IsSameObject (jniEnv, class, matchClass);
}

static BOOL
classp (jclass class, jclass matchClass)
{
  jobject clazz;
  
  if ((*jniEnv)->IsSameObject (jniEnv, class, matchClass))
    return YES;
  else
    {
      jclass nextClass;
      
      for (clazz = (*jniEnv)->GetSuperclass (jniEnv, class);
           clazz;
           nextClass = (*jniEnv)->GetSuperclass (jniEnv, clazz), 
             (*jniEnv)->DeleteLocalRef (jniEnv, clazz),
             clazz = nextClass)
        if ((*jniEnv)->IsSameObject (jniEnv, clazz, matchClass))
          {
            (*jniEnv)->DeleteLocalRef (jniEnv, clazz);
            return YES;
          }
      return NO;
    }
}

static const char *
java_signature_for_class (jclass class)
{
  const char *type;

  if (classp (class, c_Selector))
    type = "Lswarm/Selector;";
  else if (classp (class, c_String))
    type = "Ljava/lang/String;";
  else if (classp (class, c_Class))
    type = "Ljava/lang/Class;";
  else if (exactclassp (class, c_int))
    type = "I";
  else if (exactclassp (class, c_short))
    type = "S";
  else if (exactclassp (class, c_long))
    type = "J";
  else if (exactclassp (class, c_boolean))
    type = "Z";
  else if (exactclassp (class, c_byte))
    type = "B";
  else if (exactclassp (class, c_char))
    type = "C";
  else if (exactclassp (class, c_float))
    type = "F";
  else if (exactclassp (class, c_double))
    type = "D";
  else if (exactclassp (class, c_void))
    type = "V";
  else if ((*jniEnv)->CallBooleanMethod (jniEnv, class, m_ClassIsArray))
    {
      char *cp;
      type = java_get_class_name (class);
      
      for (cp = (char *) type; *cp; cp++)
        if (*cp == '.')
          *cp = '/';
      return type;
    }
  else
    {
      const char *name = java_get_class_name (class);
      char *buf = [scratchZone alloc: 1 + strlen (name) + 1 + 1];

      fill_signature (buf, name);
      FREECLASSNAME (name);
      return buf;
    }
  return SSTRDUP (type);
}

BOOL
java_objc_proxy_p (jclass class)
{
  return classp (class, c_ObjCProxy);
}

static fcall_type_t
java_getTypeInfo (jobject javaObj, unsigned *rankptr, unsigned *dims)
{
  unsigned rank = 0;
  fcall_type_t elementType = fcall_type_void;
  void checkArray (jobject obj)
    {
      jclass class = (*jniEnv)->GetObjectClass (jniEnv, obj);
      jboolean isArray =
        (*jniEnv)->CallBooleanMethod (jniEnv, class, m_ClassIsArray);
      unsigned len;
      
      if (isArray)
        {
          const char *sig = java_signature_for_class (class);

          len = (*jniEnv)->GetArrayLength (jniEnv, obj);
          
          if (dims)
            dims[rank] = len;
          rank++;
          
          if (len > 0 && sig[1] == '[')
            {
              jobject subobj =
                (*jniEnv)->GetObjectArrayElement (jniEnv, obj, 0);
              
              checkArray (subobj);
              (*jniEnv)->DeleteLocalRef (jniEnv, subobj);
            }
          else if (!fcall_type_for_java_signature (&sig[1], &elementType))
            abort ();
          
          SFREEBLOCK (sig);
        }
      else
        elementType = fcall_type_for_java_class (class);
          
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  checkArray (javaObj);
  *rankptr = rank;

  return elementType;
}

static void
java_expandArray (jobject fullary, void *inbuf)
{
  unsigned char *buf = inbuf;
  unsigned offset = 0;
  
  void permute (jobject obj)
    {
      jclass lref = (*jniEnv)->GetObjectClass (jniEnv, obj);
      const char *sig = java_signature_for_class (lref);
      jsize len = (*jniEnv)->GetArrayLength (jniEnv, obj);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      
      if (sig[1] == '[')
        {
          int i;

          for (i = 0; i < len; i++)
            {
              jobject subobj =
                (*jniEnv)->GetObjectArrayElement (jniEnv, obj, i);

              permute (subobj);
              (*jniEnv)->DeleteLocalRef (jniEnv, subobj);
            }
        }
      else if (sig[1] == 'L')
        {
          *((id *) &buf[offset]) = SD_JAVA_ENSURE_OBJECT_OBJC (obj);
          offset += sizeof (id);
        }
      else
        {
          fcall_type_t type;

          jboolean isCopy;
          size_t size;
          void *ptr;

          if (!fcall_type_for_java_signature (&sig[1], &type))
            abort ();

          size = fcall_type_size (type) * len;

          switch (type)
            {
            case fcall_type_boolean:
              ptr = (*jniEnv)->GetBooleanArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseBooleanArrayElements (jniEnv, obj, ptr,
                                                      JNI_ABORT);
              break;
            case fcall_type_uchar:
              ptr = (*jniEnv)->GetByteArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseByteArrayElements (jniEnv, obj, ptr,
                                                   JNI_ABORT);
              break;
            case fcall_type_schar:
              ptr = (*jniEnv)->GetCharArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseCharArrayElements (jniEnv, obj, ptr,
                                                   JNI_ABORT);
              break;
            case fcall_type_sshort:
              ptr = (*jniEnv)->GetShortArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseShortArrayElements (jniEnv, obj, ptr,
                                                    JNI_ABORT);
              break;
            case fcall_type_sint:
              ptr = (*jniEnv)->GetIntArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseIntArrayElements (jniEnv, obj, ptr,
                                                  JNI_ABORT);
              break;
            case fcall_type_slonglong:
              ptr = (*jniEnv)->GetLongArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseLongArrayElements (jniEnv, obj, ptr,
                                                   JNI_ABORT);
              break;
            case fcall_type_float:
              ptr = (*jniEnv)->GetFloatArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseFloatArrayElements (jniEnv, obj, ptr,
                                                    JNI_ABORT);
              break;
            case fcall_type_double:
              ptr = (*jniEnv)->GetDoubleArrayElements (jniEnv, obj, &isCopy);
              memcpy (&buf[offset], ptr, size);
              (*jniEnv)->ReleaseDoubleArrayElements (jniEnv, obj, ptr,
                                                     JNI_ABORT);
              break;
            default:
              abort ();
            }
          offset += size;
        }
      SFREEBLOCK (sig);
    }
  permute (fullary);
}

static BOOL
java_modifier_usable_p (int modifier)
{
  return (((*jniEnv)->CallStaticBooleanMethod (jniEnv,
					       c_Modifier,
					       m_ModifierIsPublic,
					       modifier)
	   == JNI_TRUE)
	  &&
	  ((*jniEnv)->CallStaticBooleanMethod (jniEnv,
					       c_Modifier,
					       m_ModifierIsStatic,
					       modifier))
	  == JNI_FALSE);
}

BOOL
java_field_usable_p (jobject field)
{
  return
    java_modifier_usable_p ((*jniEnv)->CallIntMethod (jniEnv,
						      field,
						      m_FieldGetModifiers));
}

BOOL
java_method_usable_p (jobject method)
{
  return
    java_modifier_usable_p ((*jniEnv)->CallIntMethod (jniEnv,
						    method,
						    m_MethodGetModifiers));
}

#define _GETVALUE(uptype) \
    (*jniEnv)->Get##uptype##Field (jniEnv, \
                                   javaObject, \
                                   fid)
#define GETVALUE(uptype) _GETVALUE(uptype)

static void
map_java_class_ivars_internal (jclass class,
                               void (*process_array_ivar) (const char *name, jfieldID fid),
                               void (*process_ivar) (const char *name, jfieldID fid, fcall_type_t type))
{
  jclass superClass = (*jniEnv)->GetSuperclass (jniEnv, class);
  jarray fields;
  jsize count;
  int fi;
  
  if (superClass)
    {
      map_java_class_ivars_internal (superClass,
                                     process_array_ivar, process_ivar);
      (*jniEnv)->DeleteLocalRef (jniEnv, superClass);
    }
  
  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              class,
                                              m_ClassGetDeclaredFields)))
    abort();
      
  count = (*jniEnv)->GetArrayLength (jniEnv, fields);
      
  for (fi = 0; fi < count; fi++)
    {
      jobject name, field;
      jboolean isCopy;
      const char *namestr;
          
      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, fi);
      if (!field)
        raiseEvent (SourceMessage, "field %u unavailable", fi);
      if (java_field_usable_p (field))
	{
	  name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
	  namestr = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
	  {
	    jobject lref =
	      (*jniEnv)->CallObjectMethod (jniEnv,
					   field,
					   m_FieldGetType);
	    fcall_type_t type = fcall_type_for_java_class (lref);
	    const char *sig = java_signature_for_class (lref);
	    jboolean isArray = 
	      (*jniEnv)->CallBooleanMethod (jniEnv, lref, m_ClassIsArray);
	    jfieldID fid =
	      (*jniEnv)->GetFieldID (jniEnv, class, namestr, sig);
            
	    (*jniEnv)->DeleteLocalRef (jniEnv, lref);
	    if (!fid)
	      abort ();
            
	    SFREEBLOCK (sig);
	    if (isArray)
              process_array_ivar (namestr, fid);
	    else
              process_ivar (namestr, fid, type);
	    if (isCopy)
	      (*jniEnv)->ReleaseStringUTFChars (jniEnv, name, namestr);
	    (*jniEnv)->DeleteLocalRef (jniEnv, name);
	  }
	}
      (*jniEnv)->DeleteLocalRef (jniEnv, field);
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, fields);
}

void
map_java_class_ivars (jclass class,
                      void (*process_ivar) (const char *name,
                                            fcall_type_t type))
{
  void process_array_ivar (const char *name, jfieldID fid)
    {
      // skip (unknown until there is an object)
    }
  void process_simple_ivar (const char *name, jfieldID fid, fcall_type_t type)
    {
      process_ivar (name, type);
    }
  map_java_class_ivars_internal (class,
                                 process_array_ivar,
                                 process_simple_ivar);
}

void
map_java_ivars (jobject javaObject,
                void (*process_object) (const char *name,
                                        fcall_type_t type,
                                        void *ptr,
                                        unsigned rank,
                                        unsigned *dims))
{
  void process_array_ivar (const char *name, jfieldID fid)
    {
      jobject obj = GETVALUE (Object);
      fcall_type_t type;
      unsigned rank;
      
      java_getTypeInfo (obj, &rank, NULL);
      {
        unsigned dims[rank], i;
        unsigned count = 1;
        
        type = java_getTypeInfo (obj, &rank, dims);
        
        for (i = 0; i < rank; i++)
          count *= dims[i];
        {
          unsigned buf[fcall_type_size (type) * count];
          
          java_expandArray (obj, buf);
          process_object (name, type, buf, rank, dims);
        }
      }
      (*jniEnv)->DeleteLocalRef (jniEnv, obj);
    }
  
  void process_simple_ivar (const char *name, jfieldID fid, fcall_type_t type)
    {
      types_t val;
      
      switch (type)
        {
        case fcall_type_boolean:
          val.boolean = GETVALUE (Boolean);
          break;
        case fcall_type_schar:
          val.schar = GETVALUE (Char);
          break;
        case fcall_type_uchar:
          val.uchar = GETVALUE (Byte);
          break;
        case fcall_type_sshort:
          val.sshort = GETVALUE (Short);
          break;
        case fcall_type_sint:
          val.sint = GETVALUE (Int);
          break;
        case fcall_type_slonglong:
          val.slonglong = GETVALUE (Long);
          break;
        case fcall_type_float:
          val._float = GETVALUE (Float);
          break;
        case fcall_type_double:
          val._double = GETVALUE (Double);
          break;
        case fcall_type_object:
          {
            jobject obj = GETVALUE (Object);
            
            val.object = SD_JAVA_ENSURE_OBJECT_OBJC (obj);
            if (obj)
              (*jniEnv)->DeleteLocalRef (jniEnv, obj);
          }
          break;
        case fcall_type_string:
          {
            BOOL isCopy;
            jobject string = GETVALUE (Object);
            const char *utf =
              (*jniEnv)->GetStringUTFChars (jniEnv, string, &isCopy);
            
            val.string = SSTRDUP (utf);
            if (isCopy)
              (*jniEnv)->ReleaseStringUTFChars (jniEnv, string, utf);
            (*jniEnv)->DeleteLocalRef (jniEnv, string);
          }
          break;
        case fcall_type_selector:
          {
            jobject sel = GETVALUE (Object);
            
            val.object = SD_JAVA_FIND_OBJECT_OBJC (sel);
            (*jniEnv)->DeleteLocalRef (jniEnv, sel);
          }
          break;
        case fcall_type_void:
        case fcall_type_ushort:
        case fcall_type_uint:
        case fcall_type_ulong:
        case fcall_type_slong:
        case fcall_type_ulonglong:
        case fcall_type_long_double:
        case fcall_type_class:
        case fcall_type_jobject:
        case fcall_type_jstring:
        case fcall_type_jselector:
        case fcall_type_iid:
          abort ();
        }
      process_object (name, type, &val, 0, NULL);
    }
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
  map_java_class_ivars_internal (class,
                                 process_array_ivar,
                                 process_simple_ivar);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
}
                        
static jfieldID
class_java_find_field (jclass javaClass, const char *fieldName,
                       fcall_type_t *typePtr, BOOL *isArrayPtr)
{
  jarray fields;
  jsize count;
  int i;
  BOOL match = NO;
  jobject field = NULL;
  jobject name;
  const char *namestr;
  jboolean isCopy;

  void release (void)
    {
      if (isCopy)
        (*jniEnv)->ReleaseStringUTFChars (jniEnv, name, namestr);
      (*jniEnv)->DeleteLocalRef (jniEnv, name);
      (*jniEnv)->DeleteLocalRef (jniEnv, field);
    }

  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              javaClass,
                                              m_ClassGetFields)))
    abort();
  
  count = (*jniEnv)->GetArrayLength (jniEnv, fields);

  for (i = 0; i < count; i++)
    {
      field = (*jniEnv)->GetObjectArrayElement (jniEnv, fields, i);
      if (!field)
        raiseEvent (SourceMessage, "field %u unavailable", i);
      name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
      namestr = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
      match = (strcmp (namestr, fieldName) == 0);
      if (match)
        break;
      else
        release ();
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, fields);
  if (match)
    {
      jobject lref = (*jniEnv)->CallObjectMethod (jniEnv,
                                                  field,
                                                  m_FieldGetType);
      const char *sig = java_signature_for_class (lref);
      fcall_type_t type = fcall_type_for_java_class (lref);
      jfieldID fid;
      jboolean isArray =
        (*jniEnv)->CallBooleanMethod (jniEnv, lref, m_ClassIsArray);
      
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      fid = (*jniEnv)->GetFieldID (jniEnv, javaClass, namestr, sig);
      if (!fid)
        abort ();
      SFREEBLOCK (sig);
      release ();
      if (typePtr)
        *typePtr = type;
      if (isArrayPtr)
        *isArrayPtr = isArray;
      return fid;
    }
  else
    return NULL;
}

static void
java_storeArray (jobject javaObject,
                 fcall_type_t type, unsigned rank, unsigned *dims, void *buf)
{
  jobject values[rank];
  unsigned vec[rank];
  unsigned di;
    
  void start_dim (unsigned dimnum)
    {
      di = dimnum;
      vec[di] = 0;
      if (dimnum > 0)
        values[di] =
          (*jniEnv)->GetObjectArrayElement (jniEnv,
                                            values[di - 1],
                                            vec[di - 1]);
    }
  void end_dim (void)
    {
      if (di == rank - 1)
        {
          unsigned len = vec[di];
          switch (type)
            {
            case fcall_type_boolean:
              (*jniEnv)->SetBooleanArrayRegion (jniEnv,
                                                values[di],
                                                0, len,
                                                (jboolean *) buf);
              break;
            case fcall_type_uchar:
              (*jniEnv)->SetByteArrayRegion (jniEnv,
                                             values[di],
                                             0, len,
                                             (jbyte *) buf);
              break;
            case fcall_type_schar:
              (*jniEnv)->SetCharArrayRegion (jniEnv,
                                             values[di],
                                             0, len,
                                             (jchar *) buf);
              break;
            case fcall_type_sshort:
              (*jniEnv)->SetShortArrayRegion (jniEnv,
                                              values[di],
                                              0, len,
                                              (jshort *) buf);
              break;
            case fcall_type_sint:
              (*jniEnv)->SetIntArrayRegion (jniEnv,
                                            values[di],
                                            0, len,
                                            (jint *) buf);
              break;
            case fcall_type_slonglong:
              (*jniEnv)->SetLongArrayRegion (jniEnv,
                                             values[di],
                                             0, len,
                                             (jlong *) buf);
              break;
            case fcall_type_float:
              (*jniEnv)->SetFloatArrayRegion (jniEnv,
                                              values[di],
                                              0, len,
                                              (jfloat *) buf);
              break;
            case fcall_type_double:
              (*jniEnv)->SetDoubleArrayRegion (jniEnv,
                                               values[di],
                                               0, len,
                                               (jdouble *) buf);
              break;
            default:
              abort ();
            }
        }
    }
  void end_element (void)
    {
      vec[di]++;
    }
    
  values[0] = javaObject;
  process_array (rank, dims, type,
                 start_dim, end_dim, NULL, end_element, NULL,
                 buf, NULL);
}

const char *
java_ensure_selector_type_signature (jobject jsel)
{
  unsigned argCount, typeSigLen = 0;

  jobject typeSignature =
    (*jniEnv)->GetObjectField (jniEnv, jsel, f_typeSignatureFid);

  if (!typeSignature)
    {
      jobject argTypes = 
        (*jniEnv)->GetObjectField (jniEnv, jsel, f_argTypesFid);

      argCount = (*jniEnv)->GetArrayLength (jniEnv, argTypes);
      {
        const char *argSigs[argCount];
        const char *retSig;
        char *sig, *p;
        unsigned ai;
        
        typeSigLen = 1;
        for (ai = 0; ai < argCount; ai++)
          {
            jclass member =
              (*jniEnv)->GetObjectArrayElement (jniEnv, argTypes, ai);
            
            argSigs[ai] = java_signature_for_class (member);
            typeSigLen += strlen (argSigs[ai]);
            (*jniEnv)->DeleteLocalRef (jniEnv, member);
          }
        typeSigLen++;

        {
          jobject retType =
            (*jniEnv)->GetObjectField (jniEnv, jsel, f_retTypeFid);
          
          retSig = java_signature_for_class (retType);
          (*jniEnv)->DeleteLocalRef (jniEnv, retType);
        }
        typeSigLen += strlen (retSig);
        
        sig = [scratchZone alloc: typeSigLen + 1];
        
        p = sig;
        *p++ = '(';
        for (ai = 0; ai < argCount; ai++)
          p = stpcpy (p, argSigs[ai]);
        *p++ = ')';
        p = stpcpy (p, retSig);

        for (ai = 0; ai < argCount; ai++)
          [scratchZone free: (void *) argSigs[ai]];
	[scratchZone free: (void *) retSig];

        {
          jobject str = (*jniEnv)->NewStringUTF (jniEnv, sig);
          (*jniEnv)->SetObjectField (jniEnv,
                                     jsel,
                                     f_typeSignatureFid,
                                     str);
          (*jniEnv)->DeleteLocalRef (jniEnv, str);
        }
        (*jniEnv)->DeleteLocalRef (jniEnv, argTypes);
        return sig;
      }
    }
  else
    {
      jboolean copyFlag;
      const char *sig;

      const char *utf =
        (*jniEnv)->GetStringUTFChars (jniEnv, typeSignature, &copyFlag);

      sig = SSTRDUP (utf);
      if (copyFlag)
        (*jniEnv)->ReleaseStringUTFChars (jniEnv, typeSignature, utf);
      (*jniEnv)->DeleteLocalRef (jniEnv, typeSignature);
      return sig;
    }
}

fcall_type_t
fcall_type_for_java_class (jclass class)
{
  fcall_type_t type;

  if (classp (class, c_Selector))
    type = fcall_type_selector;
  else if (classp (class, c_String))
    type = fcall_type_string;
  else if (classp (class, c_Class))
    type = fcall_type_class;
  else if (exactclassp (class, c_int))
    type = fcall_type_sint;
  else if (exactclassp (class, c_short))
    type = fcall_type_sshort;
  else if (exactclassp (class, c_long))
    type = fcall_type_slong;
  else if (exactclassp (class, c_boolean))
    type = fcall_type_boolean;
  else if (exactclassp (class, c_byte))
    type = fcall_type_uchar;
  else if (exactclassp (class, c_char))
    type = fcall_type_schar;
  else if (exactclassp (class, c_float))
    type = fcall_type_float;
  else if (exactclassp (class, c_double))
    type = fcall_type_double;
  else if (exactclassp (class, c_void))
    type = fcall_type_void;
  else
    type = fcall_type_object;
  return type;
}

const char *
java_get_class_name (jclass class)
{
  jobject string;
  const char *ret;

  if (!(string = (*jniEnv)->CallObjectMethod (jniEnv, class, m_ClassGetName)))
    abort ();

  ret = JAVA_COPY_STRING (string);
  (*jniEnv)->DeleteLocalRef (jniEnv, string);
  return ret;
}

void
java_object_setVariable (jobject javaObject, const char *ivarName,
                         fcall_type_t dataType, unsigned rank, unsigned *dims,
                         void *inbuf)
{
  if (!javaObject)
    abort ();
  {
    jclass javaClass = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
    jfieldID fid;
    fcall_type_t type;
    BOOL isArray;
    
    if (!javaClass)
      abort ();
    fid = class_java_find_field (javaClass, ivarName, &type, &isArray);
    if (!fid)
      raiseEvent (WarningMessage,
                  "field `%s' was not found in java class `%s'\n",
                  ivarName,
                  java_get_class_name (javaClass));
    else
      {
        if (isArray)
          {
            jobject ary = 0;

            if (rank > 1)
              abort ();

            switch (dataType)
              {
              case fcall_type_boolean:
                ary = (*jniEnv)->NewBooleanArray (jniEnv, dims[0]);
                break;
              case fcall_type_uchar:
                ary = (*jniEnv)->NewByteArray (jniEnv, dims[0]);
                break;
              case fcall_type_schar:
                ary = (*jniEnv)->NewCharArray (jniEnv, dims[0]);
                break;
              case fcall_type_sshort:
                ary = (*jniEnv)->NewShortArray (jniEnv, dims[0]);
                break;
              case fcall_type_sint:
                ary = (*jniEnv)->NewIntArray (jniEnv, dims[0]);
                break;
              case fcall_type_slong:
                ary = (*jniEnv)->NewLongArray (jniEnv, dims[0]);
                break;
              case fcall_type_float:
                ary = (*jniEnv)->NewFloatArray (jniEnv, dims[0]);
                break;
              case fcall_type_double:
                ary = (*jniEnv)->NewDoubleArray (jniEnv, dims[0]);
                break;
              default:
                abort ();
              }
            java_storeArray (ary, dataType, rank, dims, inbuf);
            (*jniEnv)->SetObjectField (jniEnv, javaObject, fid, ary);
            (*jniEnv)->DeleteLocalRef (jniEnv, ary);
          }
        else if (rank == 0 || (rank == 1 && dims[0] == 1))
          {
#define _SETVALUE(uptype,value) \
    (*jniEnv)->Set##uptype##Field (jniEnv, javaObject, fid, value)
#define SETVALUE(uptype, value) _SETVALUE(uptype, value)
        
            types_t *buf = inbuf;

            // dataType of void means unspecified
            // (e.g. from object_setVariable)
            if (dataType != fcall_type_void && type != dataType)
              raiseEvent (InvalidArgument,
                          "Type mismatch setting `%s' data %u ivar %u",
                          ivarName,
                          (unsigned) dataType, (unsigned) type);

            switch (type)
              {
              case fcall_type_object:
                SETVALUE (Object, SD_JAVA_ENSURE_OBJECT_JAVA (buf->object));
                break;
              case fcall_type_class:
                SETVALUE (Object, SD_JAVA_FIND_CLASS_JAVA (buf->_class));
                break;
              case fcall_type_string:
                {
                  jobject lref = (*jniEnv)->NewStringUTF (jniEnv, buf->string);
                  
                  SETVALUE (Object, lref);
                  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
                }
                break;
              case fcall_type_long_double:
                abort ();
              case fcall_type_double:
                SETVALUE (Double, buf->_double);
                break;
              case fcall_type_float:
                SETVALUE (Float, buf->_float);
                break;
              case fcall_type_boolean:
                SETVALUE (Boolean, buf->boolean);
                break;
              case fcall_type_sint:
                SETVALUE (Int, buf->sint);
                break;
              case fcall_type_sshort:
                SETVALUE (Short, buf->sshort);
                break;
              case fcall_type_slonglong:
                SETVALUE (Long, buf->slonglong);
                break;
              case fcall_type_uchar:
                SETVALUE (Byte, buf->uchar);
                break;
              case fcall_type_schar:
                SETVALUE (Char, buf->schar);
              case fcall_type_void:
              case fcall_type_ushort:
              case fcall_type_uint:
              case fcall_type_ulong:
              case fcall_type_slong:
              case fcall_type_ulonglong:
              case fcall_type_selector:
              case fcall_type_jobject:
              case fcall_type_jstring:
              case fcall_type_jselector:
              case fcall_type_iid:
                abort ();
              }
#undef SETVALUE
#undef _SETVALUE
          }
        else
          raiseEvent (InvalidArgument,
                      "ivar %s fcall_type: %u rank: %u dims[0]: %u\n",
                      ivarName, type, rank, dims[0]);
      }
    (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
  }
}

fcall_type_t
java_object_ivar_type (jobject javaObject, const char *ivarName, BOOL *isArrayPtr)
{
  jclass javaClass;
  fcall_type_t type;

  if (!javaObject)
    abort ();
  javaClass = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
  if (!class_java_find_field (javaClass, ivarName, &type, isArrayPtr))
    abort ();
  (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
  return type;
}

static jobject
get_swarmEnvironment_field (jobject swarmEnvironment,
			    const char *fieldName)
{
  jobject fieldObject, ret;
  jstring str = (*jniEnv)->NewStringUTF (jniEnv, fieldName);

  if (!(fieldObject =
	(*jniEnv)->CallObjectMethod (jniEnv,
                                     c_SwarmEnvironmentImpl,
                                     m_ClassGetDeclaredField,
                                     str)))
    raiseEvent (InternalError, "Could not find field `%s'\n", fieldName);
  (*jniEnv)->DeleteLocalRef (jniEnv, str);
  
  ret = (*jniEnv)->CallObjectMethod (jniEnv,
                                     fieldObject,
                                     m_FieldGetObject,
                                     swarmEnvironment);
  (*jniEnv)->DeleteLocalRef (jniEnv, fieldObject);
  return ret;
}

static void
create_bootstrap_refs (void)
{
  jclass lref;

  if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/Primitives")))
    abort ();
  c_Primitives = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  
  if (!(m_PrimitivesGetTypeMethod =
        (*jniEnv)->GetStaticMethodID (jniEnv, c_Primitives, "getTypeMethod",
                                      "(Ljava/lang/String;)Ljava/lang/reflect/Method;")))
    abort ();
  
  if (!(lref = (*jniEnv)->FindClass (jniEnv, "java/lang/reflect/Method")))
    abort ();
  c_Method = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);

  if (!(m_MethodGetReturnType =
        (*jniEnv)->GetMethodID (jniEnv, c_Method, "getReturnType",
                                "()Ljava/lang/Class;")))
    abort();
}

static jclass
get_java_lang_class (const char *name)
{
  char class_name_buf[10 + strlen (name) + 1];
  char *p;
  jclass ret, clazz;
  
  p = stpcpy (class_name_buf, "java/lang/");
  p = stpcpy (p, name);
  if (!(clazz = (*jniEnv)->FindClass (jniEnv, class_name_buf)))
    abort ();
  
  ret = (*jniEnv)->NewGlobalRef (jniEnv, clazz);
  (*jniEnv)->DeleteLocalRef (jniEnv, clazz);
  return ret;
}

static void
create_class_refs (void)
{
  jobject lref;

  jclass get_primitive (const char *name)
    {
#if 0
      return get_type_field_for_class (get_java_lang_class (name));
#else
      jobject nameString = (*jniEnv)->NewStringUTF (jniEnv, name);
      jobject method;
      jclass lref, returnType;
      
      if (!(method = (*jniEnv)->CallStaticObjectMethod (jniEnv,
                                                        c_Primitives,
                                                        m_PrimitivesGetTypeMethod,
                                                        nameString)))
        abort ();
      (*jniEnv)->DeleteLocalRef (jniEnv, nameString);
      if (!(lref = (*jniEnv)->CallObjectMethod (jniEnv,
                                                method,
                                                m_MethodGetReturnType,
                                                method)))
        abort ();
      returnType = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      return returnType;
#endif
    }
  if (!initFlag)
   {

      c_char = get_primitive ("Character");
      c_byte = get_primitive ("Byte");
      c_int = get_primitive ("Integer");
      c_short = get_primitive ("Short");
      c_long = get_primitive ("Long");
      c_float = get_primitive ("Float");
      c_double = get_primitive ("Double");
      c_void = get_primitive ("Void");
      c_boolean = get_primitive ("Boolean");

      c_Boolean = get_java_lang_class ("Boolean");
      c_Char = get_java_lang_class ("Character");
      c_Byte= get_java_lang_class ("Byte");
      c_Integer = get_java_lang_class ("Integer");
      c_Short = get_java_lang_class ("Short");
      c_Long = get_java_lang_class ("Long");
      c_Float = get_java_lang_class ("Float");
      c_Double = get_java_lang_class ("Double");
     
      c_String = get_java_lang_class ("String");
      c_Object = get_java_lang_class ("Object");
      c_Class = get_java_lang_class ("Class");

      if (!(lref = (*jniEnv)->FindClass (jniEnv, "java/lang/reflect/Field")))
        abort ();
      c_Field = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv,
                                         "java/lang/reflect/Modifier")))
	abort ();
      c_Modifier = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv, "java/util/Collection")))
        abort ();
      c_Collection = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/BaseImpl")))
        abort ();
      c_Base = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/Selector")))
        abort ();
      c_Selector = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      
      if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/PhaseCImpl")))
        abort ();
      c_PhaseCImpl = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv,
                                         "swarm/SwarmEnvironmentImpl")))
	abort ();
      c_SwarmEnvironmentImpl = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if ((lref = (*jniEnv)->FindClass (jniEnv,
                                        "swarm/ProxyClassLoader")))
        {
          c_ProxyClassLoader = (*jniEnv)->NewGlobalRef (jniEnv, lref);
          (*jniEnv)->DeleteLocalRef (jniEnv, lref);
          
          if (!(lref = (*jniEnv)->FindClass (jniEnv,
                                             "swarm/ObjCProxy")))
            abort ();
          c_ObjCProxy = (*jniEnv)->NewGlobalRef (jniEnv, lref);
          (*jniEnv)->DeleteLocalRef (jniEnv, lref);
        }
      else
        (*jniEnv)->ExceptionClear (jniEnv);
      initFlag = YES;
   }
}

static void 
create_method_refs (void)
{
  jmethodID findMethodID (const char *name, jclass clazz)
    {
      char sig[31 + strlen (name) + 1 + 1];
      char *p;

      jmethodID res;
      p = stpcpy (sig, "(Ljava/lang/String;)Ljava/lang/");
      p = stpcpy (p, name);
      p = stpcpy (p, ";");

      if (!(res = (*jniEnv)->GetStaticMethodID (jniEnv, clazz, "valueOf", sig)))
	abort ();

      return res;
    }   
  
  m_BooleanValueOf = findMethodID ("Boolean", c_Boolean);
  
  m_ByteValueOf = findMethodID ("Byte", c_Byte);
  
  m_IntegerValueOf = findMethodID ("Integer", c_Integer);
  
  m_ShortValueOf = findMethodID ("Short", c_Short);
  
  m_LongValueOf = findMethodID ("Long", c_Long);
  
  m_FloatValueOf = findMethodID ("Float", c_Float);
  
  m_DoubleValueOf = findMethodID ("Double", c_Double);
  
  if (!(m_StringValueOfBoolean = 
      (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                    "(Z)Ljava/lang/String;")))
    abort ();
  
  if (!(m_StringValueOfChar = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                      "(C)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfInt = 
      (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                    "(I)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfLong = 
      (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                    "(J)Ljava/lang/String;")))
    abort ();
  
  if (!(m_StringValueOfFloat = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                      "(F)Ljava/lang/String;")))
    abort ();
  
  if (!(m_StringValueOfDouble = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                      "(D)Ljava/lang/String;")))
    abort ();
  
  if (!(m_StringValueOfObject = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_String, "valueOf", 
                                      "(Ljava/lang/Object;)Ljava/lang/String;")))
    abort ();
  
  if (!(m_FieldSet = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "set", 
                                "(Ljava/lang/Object;Ljava/lang/Object;)V")))
    abort ();
  
  if (!(m_FieldSetChar = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "setChar", 
                                "(Ljava/lang/Object;C)V")))
    abort ();

  if (!(m_FieldSetFloat = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "setFloat", 
                                "(Ljava/lang/Object;F)V")))
    abort ();
  
  if (!(m_FieldSetDouble = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "setDouble", 
                                "(Ljava/lang/Object;D)V")))
    abort ();

  if (!(m_ClassGetField =
      (*jniEnv)->GetMethodID (jniEnv, c_Class, "getField",
                              "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort ();

  if (!(m_ClassGetDeclaredField =
      (*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredField",
                              "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort ();
  
  if (!(m_ClassGetDeclaredFields =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredFields",
                                "()[Ljava/lang/reflect/Field;")))
    abort ();
  
  if (!(m_ClassGetDeclaredMethods =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredMethods",
                                "()[Ljava/lang/reflect/Method;")))
    abort ();

  if (!(m_ClassGetFields =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getFields",
                                "()[Ljava/lang/reflect/Field;")))
    abort ();
  
  if (!(m_ClassGetName = 
	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getName", "()Ljava/lang/String;")))
    abort ();
  
  if (!(m_ClassIsArray =
	(*jniEnv)->GetMethodID (jniEnv, c_Class, "isArray", "()Z")))
    abort ();

  if (!(m_FieldGetName = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "getName", "()Ljava/lang/String;")))
    abort ();

  if (!(m_FieldGetType =
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "getType", "()Ljava/lang/Class;")))
    abort ();
  
  if (!(m_FieldGetBoolean =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getBoolean", 
                              "(Ljava/lang/Object;)Z")))
    abort ();
  
  if (!(m_FieldGetChar =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getChar", 
                              "(Ljava/lang/Object;)C")))
    abort ();

  if (!(m_FieldGetShort =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getShort", 
                              "(Ljava/lang/Object;)S")))
    abort ();

  if (!(m_FieldGetInt =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getInt", 
                              "(Ljava/lang/Object;)I")))
    abort ();

  if (!(m_FieldGetLong =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getLong", 
                              "(Ljava/lang/Object;)J")))
    abort ();
  
  if (!(m_FieldGetFloat =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getFloat", 
                              "(Ljava/lang/Object;)F")))
    abort ();

  if (!(m_FieldGetDouble =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getDouble", 
                              "(Ljava/lang/Object;)D")))
    abort ();

  if (!(m_FieldGetObject =
        (*jniEnv)->GetMethodID (jniEnv, c_Field, "get",
                                "(Ljava/lang/Object;)Ljava/lang/Object;")))
    abort ();

  if (!(m_FieldGetModifiers =
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "getModifiers", "()I")))
    abort ();

  if (!(m_MethodGetName =
	(*jniEnv)->GetMethodID (jniEnv, c_Method, "getName",
                                "()Ljava/lang/String;")))
    abort ();

  if (!(m_MethodGetModifiers =
        (*jniEnv)->GetMethodID (jniEnv, c_Method, "getModifiers", "()I")))
    abort ();

  if (!(m_ModifierIsPublic = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_Modifier, "isPublic", "(I)Z")))
    abort ();

  if (!(m_ModifierIsStatic = 
        (*jniEnv)->GetStaticMethodID (jniEnv, c_Modifier, "isStatic", "(I)Z")))
    abort ();
  
  if (!(m_SelectorConstructor =
	(*jniEnv)->GetMethodID (jniEnv, c_Selector, "<init>", 
                                "(Ljava/lang/Class;Ljava/lang/String;Z)V")))
    abort ();

  if (!(m_HashCode =
        (*jniEnv)->GetMethodID (jniEnv, c_Object, "hashCode", "()I")))
    abort ();
  
  if (!(m_Equals =
        (*jniEnv)->GetMethodID (jniEnv, c_Object, "equals",
                                "(Ljava/lang/Object;)Z")))
    abort ();
  
  if (!(m_PhaseCImpl_copy_creating_phase_to_using_phase = 
        (*jniEnv)->GetMethodID (jniEnv,
                                c_PhaseCImpl,
                                "_copy_creating_phase_to_using_phase",
                                "()V")))
    abort ();

  if (c_ProxyClassLoader)
    if (!(m_ProxyClassLoaderLoadClass =
          (*jniEnv)->GetMethodID (jniEnv, c_ProxyClassLoader, "loadClass",
                                  "(Ljava/lang/String;)Ljava/lang/Class;")))
      abort ();
}

static void
create_field_refs (void)
{
  if (!(f_objcPtrFid = (*jniEnv)->GetFieldID (jniEnv, c_Base, "objcPtr", "I")))
    abort ();
  if (!(f_nameFid = (*jniEnv)->GetFieldID (jniEnv, c_Selector, "signature", "Ljava/lang/String;")))
    abort ();
  if (!(f_retTypeFid = (*jniEnv)->GetFieldID (jniEnv, c_Selector, "retType", "Ljava/lang/Class;")))
    abort ();
  if (!(f_argTypesFid = (*jniEnv)->GetFieldID (jniEnv, c_Selector, "argTypes", "[Ljava/lang/Class;")))
    abort ();
  if (!(f_typeSignatureFid = (*jniEnv)->GetFieldID (jniEnv, c_Selector, "typeSignature", "Ljava/lang/String;")))
    abort ();
  if (!(f_objcFlagFid = (*jniEnv)->GetFieldID (jniEnv, c_Selector, "objcFlag", "Z")))
    abort ();
  
  if (!(f_nextPhase = (*jniEnv)->GetFieldID (jniEnv, c_PhaseCImpl, "nextPhase", 
                                             "Ljava/lang/Object;")))
    abort();
}

static void
create_object_refs ()
{
  jmethodID mid;
  jobject lref;
  
  if (c_ProxyClassLoader)
    {
      if (!(mid = (*jniEnv)->GetMethodID (jniEnv, c_ProxyClassLoader,
                                          "<init>", "()V")))
        abort ();
      lref = (*jniEnv)->NewObject (jniEnv, c_ProxyClassLoader, mid);
      proxyClassLoader = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
    }
}

void
java_create_refs (void)
{
  create_bootstrap_refs ();
  create_class_refs ();
  create_method_refs ();
  create_field_refs ();
  create_object_refs ();
}

static void
associate (jobject swarmEnvironment, const char *fieldName, id objcObject)
{
  jobject lref;
  
  lref = get_swarmEnvironment_field (swarmEnvironment, fieldName);
  if (!lref)
    raiseEvent (InternalError, "Could not find field name `%s'\n",
                fieldName);
  SD_JAVA_ADD_OBJECT (lref, objcObject);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
}

#define ASSOCIATE(fieldName) associate (swarmEnvironment, #fieldName, fieldName)

void
swarm_directory_java_associate_objects_startup (jobject swarmEnvironment)
{
  ASSOCIATE (scratchZone);
  ASSOCIATE (globalZone);
}

void
swarm_directory_java_associate_objects (jobject swarmEnvironment)
{
  extern BOOL swarmGUIMode;

  ASSOCIATE (arguments);

  if (hdf5Archiver)
    ASSOCIATE (hdf5Archiver);
  ASSOCIATE (lispArchiver);
  if (hdf5AppArchiver)
    ASSOCIATE (hdf5AppArchiver);
  ASSOCIATE (lispAppArchiver);

  {
    extern id <Symbol> LanguageCOM, LanguageJava, LanguageObjc;

    ASSOCIATE (LanguageCOM);
    ASSOCIATE (LanguageJava);
    ASSOCIATE (LanguageObjc);
  }

  {
    extern id <Symbol> Start, Member, End;

    ASSOCIATE (Start);
    ASSOCIATE (Member);
    ASSOCIATE (End);
  }

  {
    extern id <Symbol> Randomized;
    extern id <Symbol> Sequential;
    
    ASSOCIATE (Randomized);
    ASSOCIATE (Sequential);
  }

  {
    extern id probeLibrary;
    
    ASSOCIATE (probeLibrary);
  }
   
  {
    extern id randomGenerator, uniformIntRand, uniformDblRand;
    
    ASSOCIATE (randomGenerator);
    ASSOCIATE (uniformIntRand);
    ASSOCIATE (uniformDblRand);
  }

#ifndef DISABLE_GUI
#ifndef GNUSTEP
  if (swarmGUIMode)
    {
      extern id probeDisplayManager;
      
      extern id <Symbol> ControlStateRunning, ControlStateStopped,
        ControlStateStepping, ControlStateQuit,ControlStateNextTime;
      
      ASSOCIATE (probeDisplayManager);
      
      ASSOCIATE (ControlStateRunning);
      ASSOCIATE (ControlStateStopped);
      ASSOCIATE (ControlStateStepping);
      ASSOCIATE (ControlStateQuit);
      ASSOCIATE (ControlStateNextTime);
    }
#endif
#endif
  
  {
    extern id <Symbol> Initialized, Running, Stopped, Holding, Released, 
      Terminated, Completed;

    ASSOCIATE (Initialized);
    ASSOCIATE (Running);
    ASSOCIATE (Stopped);
    ASSOCIATE (Holding);
    ASSOCIATE (Released);
    ASSOCIATE (Terminated);
    ASSOCIATE (Completed);
  }
  {
    jfieldID fid;
    
    if (!(fid = (*jniEnv)->GetFieldID (jniEnv, c_SwarmEnvironmentImpl, "guiFlag", "Z")))
      abort ();
    
    (*jniEnv)->SetBooleanField (jniEnv, swarmEnvironment, fid, (jboolean) swarmGUIMode);
  }
}

unsigned
swarm_directory_java_hash_code (jobject javaObject)
{
  int hashCode;

  hashCode = (*jniEnv)->CallIntMethod (jniEnv, javaObject, m_HashCode);
  return (hashCode < 0 ? - hashCode : hashCode) % DIRECTORY_SIZE;
}


static ObjectEntry *
swarm_directory_java_find (jobject javaObject)
{
  if (javaObject)
    {
      id ret;
      unsigned index = swarm_directory_java_hash_code (javaObject);
      id <Map> m = swarmDirectory->javaTable[index];
      ObjectEntry *findEntry = JAVA_FIND_OBJECT_ENTRY (javaObject);
      ret = m ? [m at: findEntry] : nil;
      return ret;
    }
  return nil;
}

id
swarm_directory_java_find_swarm_objc (jobject javaObject)
{
  return (id) (*jniEnv)->GetIntField (jniEnv, javaObject, f_objcPtrFid);
}

static id
find_swarm_objc_safe (jobject javaObject)
{
  id result;

  if (javaObject)
    {
      jclass class = (*jniEnv)->GetObjectClass (jniEnv, javaObject);

      result = (classp (class, c_Base)
                ? swarm_directory_java_find_swarm_objc (javaObject)
                : nil);
      (*jniEnv)->DeleteLocalRef (jniEnv, class);
    }
  else
    result = nil;

  return result;
}

id
swarm_directory_java_ensure_objc (jobject javaObject)
{
  id result;

  if (!javaObject)
    result = nil;
  else if(!(result = find_swarm_objc_safe (javaObject)))
    {
      ObjectEntry *resultEntry; 
      
      resultEntry = swarm_directory_java_find (javaObject);
      
      if ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_String))
        {
          jboolean isCopy;
          const char *utf, *str;
          
          utf = (*jniEnv)->GetStringUTFChars (jniEnv, javaObject, &isCopy);
          str = ZSTRDUP (getZone (swarmDirectory), utf);
          if (isCopy)
            (*jniEnv)->ReleaseStringUTFChars (jniEnv, javaObject, utf);
          
          if (resultEntry)
            {
              const char *last = (const char *) resultEntry->object;

              resultEntry = SD_JAVA_SWITCHOBJC (resultEntry->foreignObject.java, (id) str);
	      ZFREEBLOCK (getZone (swarmDirectory), (void *) last);
            }
          else
            resultEntry = SD_JAVA_ADD_STRING (javaObject, str);
        }
      else if (!resultEntry)
        resultEntry =
          SD_JAVA_ADD_OBJECT (javaObject, 
                              ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_Collection)
                               ? [JavaCollection create: globalZone]
                               : [JavaProxy create: globalZone]));
      
      result = resultEntry->object;
    }
  return result;
}

static jobject
java_instantiate (jclass clazz)
{
  jmethodID mid;

  if (!(mid = (*jniEnv)->GetMethodID (jniEnv, clazz, "<init>", "()V")))
    abort ();
  return (*jniEnv)->NewObject (jniEnv, clazz, mid);
}

ObjectEntry *
java_instantiate_pair (jclass clazz)
{
  id proxy;
  ObjectEntry *entry;
  jobject lref = java_instantiate (clazz);
  
  proxy = [JavaProxy create: globalZone];
  entry = SD_JAVA_ADD_OBJECT (lref, proxy);
  (*jniEnv)->DeleteLocalRef (jniEnv, lref);
  return entry;
}

jclass
java_find_class (const char *javaClassName, BOOL failFlag)
{
  jobject ret;
  jobject throwable;
  size_t i, len = strlen (javaClassName);
  char buf[len + 1];

  for (i = 0; i < len; i++)
    buf[i] = (javaClassName[i] == '.') ? '/' : javaClassName[i];
  buf[len] = '\0';
  
  (*jniEnv)->ExceptionClear (jniEnv);
  ret = (*jniEnv)->FindClass (jniEnv, buf);
  
  if (failFlag)
    {
      if ((throwable = (*jniEnv)->ExceptionOccurred (jniEnv)) != NULL)
        (*jniEnv)->ExceptionDescribe (jniEnv);
    }
  else
    (*jniEnv)->ExceptionClear (jniEnv);
  return ret;
}

static jclass
find_java_wrapper_class (Class class)
{
  const char *name = language_independent_class_name_for_objc_class (class);
  jclass ret = 0;

  if (name)
    {
      ret = java_find_class (name, YES);
      FREECLASSNAME (name);
    }
  else if (proxyClassLoader)
    {
#if SWARM_OBJC_DONE
      jstring str = (*jniEnv)->NewStringUTF (jniEnv, class->name);
#else
      jstring str = (*jniEnv)->NewStringUTF (jniEnv, swarm_class_getName(class));
#endif

      ret = (*jniEnv)->CallObjectMethod (jniEnv,
                                         proxyClassLoader,
                                         m_ProxyClassLoaderLoadClass,
                                         str);
      (*jniEnv)->DeleteLocalRef (jniEnv, str);
    }
  return ret;
}


jobject
swarm_directory_objc_ensure_java (id object)
{
  jobject jobj;

  if (!object)
    return 0;

  jobj = SD_JAVA_FIND_OBJECT_JAVA (object);
  if (!jobj)
    {
      Class class = getClass (object);
      jclass javaClass = SD_JAVA_FIND_CLASS_JAVA (class);
      jobject lref;

      if (!javaClass) // e.g., a native class and no class loader (no kawa.jar)
        abort ();
      lref = java_instantiate (javaClass);

      jobj = SD_JAVA_ADD_OBJECT_JAVA (lref, object);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
    }
  return jobj;
}

id
swarm_directory_java_find_objc (jobject javaObject)
{
  if (!javaObject)
    return nil;
  else
    {
      id swarmObj = find_swarm_objc_safe (javaObject);

      if (swarmObj)
        return swarmObj;
      else
        {
          ObjectEntry *entry = swarm_directory_java_find (javaObject);
          
          return entry ? entry->object : nil;
        }
    }
}

jobject
swarm_directory_java_next_phase (jobject jobj)
{
  (*jniEnv)->CallVoidMethod (jniEnv, jobj, 
                             m_PhaseCImpl_copy_creating_phase_to_using_phase);
  return (*jniEnv)->GetObjectField (jniEnv, jobj, f_nextPhase);
}

static int
java_compareDirectoryEntries (DirectoryEntry *obj1, DirectoryEntry* obj2)
{
#if 0
  return (int) !(*jniEnv)->CallBooleanMethod (jniEnv,
                                              obj1->foreignObject.java,
                                              m_Equals,
                                              obj2->foreignObject.java);
#else
  return (int) !(*jniEnv)->IsSameObject (jniEnv,
                                         obj1->foreignObject.java,
                                         obj2->foreignObject.java);
#endif
}

static id <Map>
createDirectoryEntryMap (void)
{
  return [[[Map createBegin: getZone (swarmDirectory)]
            setCompareFunction: java_compareDirectoryEntries]
           createEnd];
}

static void
swarm_directory_switch_java_entry (ObjectEntry *entry, jobject javaObject)
{
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, javaObject);

  if (classp (class, c_Base))
    {
      (*jniEnv)->SetIntField (jniEnv, javaObject, f_objcPtrFid,
                              (jint) entry->object);
      entry->foreignObject.java = javaObject;
    }
 else
   {
      unsigned index;
      id <Map> m;
      id <Map> *javaTable = swarmDirectory->javaTable;
      
      index = swarm_directory_java_hash_code (entry->foreignObject.java);
      m = javaTable[index];
      [m remove: entry];
      (*jniEnv)->DeleteGlobalRef (jniEnv, entry->foreignObject.java);
      
      index = swarm_directory_java_hash_code (javaObject);
      entry->foreignObject.java = javaObject;
      if (!javaTable[index])
        javaTable[index] = createDirectoryEntryMap ();
      
      [javaTable[index] at: entry insert: entry];
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
}

ObjectEntry *
swarm_directory_java_switch_phase (Object_s *nextPhase,
                                   jobject currentJavaPhase)
{
  jobject lref = SD_JAVA_NEXTPHASE (currentJavaPhase);
  Object_s *currentPhase = SD_JAVA_FIND_OBJECT_OBJC (currentJavaPhase);
  ObjectEntry *retEntry;
  jobject nextJavaPhase = (*jniEnv)->NewGlobalRef (jniEnv, lref);

  (*jniEnv)->DeleteLocalRef (jniEnv, lref);

  if (currentPhase != nextPhase)
    {
      id entry = JAVA_OBJECT_ENTRY (currentJavaPhase, nextPhase);

      nextPhase->foreignEntry = entry;

      swarm_directory_switch_java_entry (entry, nextJavaPhase);

      swarm_directory_entry_drop (currentPhase->foreignEntry);
      currentPhase->foreignEntry = NULL;

      retEntry = entry;
    }
  else
    {
      swarm_directory_switch_java_entry (nextPhase->foreignEntry,
                                         nextJavaPhase);

      retEntry = nextPhase->foreignEntry;
    }
  return retEntry;
}

SEL
swarm_directory_java_ensure_selector (jobject jsel)
{
  SEL sel;

  if (!jsel)
    sel = NULL;
  else if (!(sel = (SEL) SD_JAVA_FIND_OBJECT_OBJC (jsel)))
    {
      char *name, *p;
      int i;
      jboolean copyFlag;
      jboolean objcFlag;
      jarray argTypes;
      jsize argCount;
        
      objcFlag = (*jniEnv)->GetBooleanField (jniEnv, jsel, f_objcFlagFid);
      argTypes = (*jniEnv)->GetObjectField (jniEnv, jsel, f_argTypesFid);
      argCount = (*jniEnv)->GetArrayLength (jniEnv, argTypes);

      {
        jstring string;
        const char *utf;
        
        string = (*jniEnv)->GetObjectField (jniEnv, jsel, f_nameFid);
        utf = (*jniEnv)->GetStringUTFChars (jniEnv, string, &copyFlag);
        
        if (objcFlag)
          {
            size_t len = strlen (utf);
            BOOL needTrailingColon = argCount > 0 && utf[len - 1] != '$';
            
            p = name = [scratchZone alloc: len + (int) needTrailingColon + 1];
            strcpy (name, utf);
            while (*p)
              {
                if (*p == '$')
                  *p = ':';
                p++;
              }
            if (needTrailingColon)
	    {
              *p++ = ':';
              *p = '\0';
            }
          }
        else
          {
            name = [scratchZone alloc: strlen (utf) + argCount + 1];
            p = stpcpy (name, utf);
            for (i = 0; i < argCount; i++)
              *p++ = ':';
            *p = '\0';
          }
        if (copyFlag)
          (*jniEnv)->ReleaseStringUTFChars (jniEnv, string, utf);
        (*jniEnv)->DeleteLocalRef (jniEnv, string);
      }

      {
        jsize ti;
        char signatureBuf[(argCount + 3) * 3 + 1], *p = signatureBuf;
          
        void add_type (fcall_type_t type)
          {
            const char *objctype =  objc_type_for_fcall_type (type);

            p = stpcpy (p, objctype);
            *p++ = '0';
            *p = '\0';
            [globalZone free: (void *) objctype];
          }
        void add (jclass class)
          {
            add_type (fcall_type_for_java_class (class));
          }
        {
          jobject retType = (*jniEnv)->GetObjectField (jniEnv, jsel, f_retTypeFid);
          
          add (retType);
          (*jniEnv)->DeleteLocalRef (jniEnv, retType);
        }
        add_type (fcall_type_object);
        add_type (fcall_type_selector);

        for (ti = 0; ti < argCount; ti++)
          {
            jobject lref = (*jniEnv)->GetObjectArrayElement (jniEnv, argTypes, ti);

            add (lref);
            (*jniEnv)->DeleteLocalRef (jniEnv, lref);
          }
      
        sel = swarm_sel_getUidWithType (name);
        {
          BOOL needSelector = NO;
          
          if (sel)
            {
              if (!swarm_sel_getTypedUid (name, signatureBuf))
                {
#if 1
                  raiseEvent (WarningMessage,
                              "Method `%s' type (%s) differs from Swarm "
                              "method's type (%s)\n",
			      name, signatureBuf, swarm_sel_getTypeEncoding (sel));
#endif
                  needSelector = YES;
                }
              
            }
          else
            needSelector = YES;
          
          if (needSelector)
            {
              const char *type =
                mframe_build_signature (signatureBuf, NULL, NULL, NULL);
              
              sel = swarm_sel_registerTypedName (name, type);
            }
        }
      }
      
      SD_JAVA_ADD_SELECTOR (jsel, sel);

      (*jniEnv)->DeleteLocalRef (jniEnv, argTypes);
      SFREEBLOCK (name);
    }
  return sel;
}

jclass
swarm_directory_objc_find_class_java (Class class)
{
  if (swarmDirectory)
    {
      ObjectEntry *entry = swarm_directory_objc_find_class (class);
      
      if (!entry)
        {
          if (![class respondsTo: M(isJavaProxy)])
            {
              jclass lref, javaClass;
              
              lref = find_java_wrapper_class (class);
              javaClass = (jclass) SD_JAVA_ADD_CLASS_JAVA (lref, class);
              (*jniEnv)->DeleteLocalRef (jniEnv, lref);
              return javaClass;
            }
          else
            abort ();
        } 
      else
        return entry->foreignObject.java;
    }
  else
    return 0;
}

Class
swarm_directory_java_ensure_class (jclass javaClass)
{
  Class objcClass;

  if (!(objcClass = SD_JAVA_FIND_OBJECT_OBJC (javaClass)))
    {
      const char *className = java_get_class_name (javaClass);

      objcClass = objc_class_for_class_name (className);
      
      // if the corresponding class does not exist create new Java Proxy
      
      if (objcClass == nil)
        {
          objcClass = [JavaClassProxy create: globalZone];
          
          (void) SD_JAVA_ADD_CLASS_JAVA (javaClass, objcClass);
        }
      FREECLASSNAME (className);
    }
  return objcClass;
}

jobject
swarm_directory_objc_find_object_java (id object)
{
  ObjectEntry *entry = swarm_directory_objc_find_object (object);
  
  if (entry)
    {
      if (entry->type == foreign_java)
        return entry->foreignObject.java;
    }
  return NULL;
}

jobject
swarm_directory_objc_find_selector_java (SEL sel)
{
  SelectorEntry *entry = swarm_directory_objc_find_selector (sel);

  if (entry)
    {
      if (entry->type == foreign_java)
        return entry->foreignObject.java;
    }
  return NULL;
}

jobject
swarm_directory_objc_ensure_selector_java (jclass jClass, SEL sel)
{
  SelectorEntry *entry = swarm_directory_objc_find_selector (sel);

  if (entry && entry->type == foreign_java)
    return entry->foreignObject.java;
  else
    {
      jobject jSelName = (*jniEnv)->NewStringUTF (jniEnv, swarm_sel_getName (sel));
      jobject jSel, ret;

      jSel =
        (*jniEnv)->NewObject (jniEnv, c_Selector, m_SelectorConstructor,
                              jClass,
                              jSelName, 
                              JNI_TRUE);
      if (jSel)
        {
          ret = SD_JAVA_ADD_SELECTOR (jSel, sel)->foreignObject.java;
          (*jniEnv)->DeleteLocalRef (jniEnv, jSelName);
          (*jniEnv)->DeleteLocalRef (jniEnv, jSel);
        }
      else
        {
          (*jniEnv)->ExceptionClear (jniEnv);
          ret = 0;
        }
      return ret;
    }
}

static void
add (ObjectEntry *entry)
{
  unsigned index;
  id <Map> m;
  id *javaTable = swarmDirectory->javaTable;
  
  index = swarm_directory_java_hash_code (entry->foreignObject.java);
  m = javaTable[index];
  
  if (m == nil)
    {
      m = createDirectoryEntryMap ();
      javaTable[index] = m;
    }
  [m at: entry insert: entry];
}

ObjectEntry *
swarm_directory_java_add_object (jobject lref, Object_s *object)
{
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, lref);
  jobject javaObject = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  ObjectEntry *entry;

  entry = JAVA_OBJECT_ENTRY (javaObject, object);

  if (classp (class, c_Base))
    (*jniEnv)->SetIntField (jniEnv, lref, f_objcPtrFid, (jint) object);
  else
    add (entry);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  object->foreignEntry = entry;
  return entry;
}

ObjectEntry *
swarm_directory_java_add_class (jobject lref, Class oClass)
{
  ObjectEntry *entry;
  jclass jClass = (*jniEnv)->NewGlobalRef (jniEnv, lref);

  entry = JAVA_OBJECT_ENTRY (jClass, (id) oClass);

  add (entry);
  avl_probe (swarmDirectory->class_tree, entry);
  return entry;
}

SelectorEntry *
swarm_directory_java_add_selector (jobject lref, SEL sel)
{
  unsigned index;
  id <Map> m;
  SelectorEntry *entry;
  jobject javaObject = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  id *javaTable = swarmDirectory->javaTable;
  
  entry = JAVA_SELECTOR_ENTRY (javaObject, sel);
  index = swarm_directory_java_hash_code (javaObject);
  m = javaTable[index];

  if (m == nil)
    {
      m = createDirectoryEntryMap ();
      javaTable[index] = m;
    }
  [m at: entry insert: entry];
  avl_probe (swarmDirectory->selector_tree, entry);
  return entry;
}

ObjectEntry *
swarm_directory_java_switch_objc (Object_s *object, jobject javaObject)
{
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
  ObjectEntry *entry;

  if (classp (class, c_Base))
    {
      id prev = (id) (*jniEnv)->GetIntField (jniEnv, javaObject, f_objcPtrFid);
      (*jniEnv)->SetIntField (jniEnv, javaObject, f_objcPtrFid, (jint) object);
      
      entry = JAVA_OBJECT_ENTRY (javaObject, prev);
    }
  else
    {
      unsigned index;
      id <Map> m;
      id *javaTable = swarmDirectory->javaTable;
      
      index = swarm_directory_java_hash_code (javaObject);
      m = javaTable[index];
      entry = [javaTable[index] at: JAVA_FIND_OBJECT_ENTRY (javaObject)];
    }
  if (entry)
    {
      entry->object->foreignEntry = NULL;
      entry->object = object;
    }
  else
    SD_JAVA_ADD_STRING (javaObject, object);
  return entry;
}

Class
swarm_directory_java_find_class_named_objc (const char *className)
{
  jclass javaClass = java_find_class (className, NO);
  
  if (javaClass)
    {
      Class objcClass = swarm_directory_java_ensure_class (javaClass);

      (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
      return objcClass;
    }
  return Nil;
}

Class
swarm_directory_java_class_for_object_objc (jobject jobj)
{
  jclass jcls;
  const char *className;
  Class result;
  
  jcls = (*jniEnv)->GetObjectClass (jniEnv, jobj);
  className = java_class_name (jobj);
  result = objc_class_for_class_name (className);
  FREECLASSNAME (className);
  if (!result)
    if (!(result = SD_JAVA_FIND_OBJECT_OBJC (jcls)))
      result = SD_JAVA_ENSURE_CLASS_OBJC (jcls);
  (*jniEnv)->DeleteLocalRef (jniEnv, jcls);
  return result;
}

const char *
java_class_name (jobject obj)
{
  jclass class;
  const char *ret;

  if (!(class = (*jniEnv)->GetObjectClass (jniEnv, obj)))
    abort ();

  ret = java_get_class_name (class);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
  return ret;
}

const char *
java_copy_string (jstring javaString)
{
  if (javaString)
    {
      jboolean isCopy;
      const char *str =
        (*jniEnv)->GetStringUTFChars (jniEnv, javaString, &isCopy);
      const char *ret = SSTRDUP (str);
      
      if (isCopy)
        (*jniEnv)->ReleaseStringUTFChars (jniEnv, javaString, str);
      return ret;
    }
  return 0;
}

void
java_cleanup_strings (const char **stringArray, size_t count)
{
  size_t i;

  for (i = 0; i < count; i++)
    if (stringArray[i])
      SFREEBLOCK (stringArray[i]);
}

const char **
java_convert_string_array (jobjectArray ary)
{
  jsize size = (*jniEnv)->GetArrayLength (jniEnv, ary);
  jsize i;
  const char **ret = [scratchZone alloc: sizeof (const char *) * (size + 1)];
  
  for (i = 0; i < size; i++)
    {
      jstring string = (*jniEnv)->GetObjectArrayElement (jniEnv, ary, i);
      
      ret[i] = JAVA_COPY_STRING (string);
      (*jniEnv)->DeleteLocalRef (jniEnv, string);
    }
  ret[size] = NULL;
  return ret;
}


void
java_drop (jobject jobj)
{
  (*jniEnv)->DeleteGlobalRef (jniEnv, jobj);
}

#endif
