#import "java.h"
#import <defobj/JavaProxy.h>
#import <defobj/JavaCollection.h>
#import <defobj/defalloc.h>
#import <defobj/Program.h> // Type_c
#include <objc/mframe.h> // mframe_build_signature

#define extern
#import "javavars.h"
#undef extern

static BOOL initFlag = NO;

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
  "Ljava/lang/String;"
};

const char *
java_signature_for_fcall_type (fcall_type_t type)
{
  return java_type_signature[type];
}

#ifdef HAVE_JDK
static BOOL
fcall_type_for_java_signature (const char *signature, fcall_type_t *typeptr)
{
  unsigned i;

  for (i = 0; i < FCALL_TYPE_COUNT; i++)
    if (strcmp (signature, java_type_signature[i]) == 0)
      {
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
exactclassp (JNIEnv *env, jclass class, jclass matchClass)
{
  return (*env)->IsSameObject (env, class, matchClass);
}

static BOOL
classp (JNIEnv *env, jclass class, jclass matchClass)
{
  jobject clazz;
  
  if ((*env)->IsSameObject (env, class, matchClass))
    return YES;
  else
    {
      jclass nextClass;
      
      for (clazz = (*env)->GetSuperclass (env, class);
           clazz;
           nextClass = (*env)->GetSuperclass (env, clazz), 
             (*env)->DeleteLocalRef (env, clazz),
             clazz = nextClass)
        if ((*env)->IsSameObject (env, clazz, matchClass))
          {
            (*env)->DeleteLocalRef (env, clazz);
            return YES;
          }
      return NO;
    }
}

static const char *
java_signature_for_class (JNIEnv *env, jclass class)
{
  const char *type;

  if (classp (env, class, c_Selector))
    type = "Lswarm/Selector;";
  else if (classp (env, class, c_String))
    type = "Ljava/lang/String;";
  else if (classp (env, class, c_Class))
    type = "Ljava/lang/Class;";
  else if (exactclassp (env, class, c_int))
    type = "I";
  else if (exactclassp (env, class, c_short))
    type = "S";
  else if (exactclassp (env, class, c_long))
    type = "J";
  else if (exactclassp (env, class, c_boolean))
    type = "Z";
  else if (exactclassp (env, class, c_byte))
    type = "B";
  else if (exactclassp (env, class, c_char))
    type = "C";
  else if (exactclassp (env, class, c_float))
    type = "F";
  else if (exactclassp (env, class, c_double))
    type = "D";
  else if (exactclassp (env, class, c_void))
    type = "V";
  else if ((*jniEnv)->CallBooleanMethod (jniEnv, class, m_ClassIsArray))
    type = java_get_class_name (env, class);
  else
    {
      const char *name = java_get_class_name (env, class);
      char *buf = [scratchZone alloc: 1 + strlen (name) + 1 + 1];

      fill_signature (buf, name);
      return buf;
    }
  return SSTRDUP (type);
}

static fcall_type_t
java_getTypeInfo (jobject javaObj, unsigned *rankptr, unsigned *dims)
{
  unsigned rank = 0;
  fcall_type_t elementType;
  void checkArray (jobject obj)
    {
      jclass class = (*jniEnv)->GetObjectClass (jniEnv, obj);
      jboolean isArray =
        (*jniEnv)->CallBooleanMethod (jniEnv, class, m_ClassIsArray);
      unsigned len;
      
      if (isArray)
        {
          if ((len = (*jniEnv)->GetArrayLength (jniEnv, obj)) > 0)
            {
              const char *sig =
                java_signature_for_class (jniEnv, class);

              if (dims)
                dims[rank] = len;
              rank++;
              
              if (sig[1] == '[')
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
        }
      else
        elementType =
          fcall_type_for_java_class (jniEnv, class);
          
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
      const char *sig =
        java_signature_for_class (jniEnv, lref);
      jsize len = (*jniEnv)->GetArrayLength (jniEnv, obj);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      
      if (sig[1] == '[')
        {
          unsigned i;

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
          *((id *) &buf[offset]) = SD_JAVA_ENSUREOBJC (jniEnv, obj);
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

#define _GETVALUE(uptype) \
    (*jniEnv)->Get##uptype##Field (jniEnv, \
                                   javaObject, \
                                   fid)
#define GETVALUE(uptype) _GETVALUE(uptype)

void
map_java_ivars (jobject javaObject,
                void (*process_object) (const char *name,
                                        fcall_type_t type,
                                        void *ptr,
                                        unsigned rank,
                                        unsigned *dims))
{
  jarray fields;
  jsize count;
  unsigned fi;
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
      
  if (!(fields = (*jniEnv)->CallObjectMethod (jniEnv,
                                              class,
                                              m_ClassGetFields)))
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
      name = (*jniEnv)->CallObjectMethod (jniEnv, field, m_FieldGetName);
      namestr = (*jniEnv)->GetStringUTFChars (jniEnv, name, &isCopy);
      {
        jobject lref =
          (*jniEnv)->CallObjectMethod (jniEnv,
                                       field,
                                       m_FieldGetType);
        fcall_type_t type = 
          fcall_type_for_java_class (jniEnv, lref);
        const char *sig =
          java_signature_for_class (jniEnv, lref);
        jboolean isArray = 
          (*jniEnv)->CallBooleanMethod (jniEnv, lref, m_ClassIsArray);
        jfieldID fid =
          (*jniEnv)->GetFieldID (jniEnv, class, namestr, sig);
            
        (*jniEnv)->DeleteLocalRef (jniEnv, lref);
        if (!fid)
          abort ();
            
        SFREEBLOCK (sig);
        if (isArray)
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
                process_object (namestr, type, buf, rank, dims);
              }
            }
            (*jniEnv)->DeleteLocalRef (jniEnv, obj);
          }
        else
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
                      
                  val.object = SD_JAVA_ENSUREOBJC (jniEnv, obj);
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
                      
                  val.object = SD_JAVA_FINDOBJC (jniEnv, sel);
                  (*jniEnv)->DeleteLocalRef (jniEnv, sel);
                }
                break;
              default:
                abort ();
              }
            process_object (namestr, type, &val, 0, NULL);
          }
        (*jniEnv)->DeleteLocalRef (jniEnv, field);
        if (isCopy)
          (*jniEnv)->ReleaseStringUTFChars (jniEnv, name, namestr);
        (*jniEnv)->DeleteLocalRef (jniEnv, name);
      }
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, fields);
  (*jniEnv)->DeleteLocalRef (jniEnv, class);
}

static jfieldID
class_java_find_field (jclass javaClass, const char *fieldName,
                       fcall_type_t *typePtr, BOOL *isArrayPtr)
{
  jarray fields;
  jsize count;
  unsigned i;
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
      const char *sig = java_signature_for_class (jniEnv, lref);
      fcall_type_t type =
        fcall_type_for_java_class (jniEnv, lref);
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
          unsigned len = vec[di] + 1;
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
java_ensure_selector_type_signature (JNIEnv *env, jobject jsel)
{
  unsigned argCount, typeSigLen = 0;

  jobject typeSignature =
    (*env)->GetObjectField (env, jsel, f_typeSignatureFid);

  if (!typeSignature)
    {
      jobject argTypes = 
        (*env)->GetObjectField (env, jsel, f_argTypesFid);

      argCount = (*env)->GetArrayLength (env, argTypes);
      {
        const char *argSigs[argCount];
        const char *retSig;
        char *sig, *p;
        unsigned ai;
        
        typeSigLen = 1;
        for (ai = 0; ai < argCount; ai++)
          {
            jclass member =
              (*env)->GetObjectArrayElement (env, argTypes, ai);
            
            argSigs[ai] = java_signature_for_class (env, member);
            typeSigLen += strlen (argSigs[ai]);
            (*env)->DeleteLocalRef (env, member);
          }
        typeSigLen++;

        {
          jobject retType =
            (*env)->GetObjectField (env, jsel, f_retTypeFid);
          
          retSig = java_signature_for_class (env, retType);
          (*env)->DeleteLocalRef (env, retType);
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
          jobject str = (*env)->NewStringUTF (env, sig);
          (*env)->SetObjectField (env,
                                  jsel,
                                  f_typeSignatureFid,
                                  str);
          (*env)->DeleteLocalRef (env, str);
        }
        (*env)->DeleteLocalRef (env, argTypes);
        return sig;
      }
    }
  else
    {
      jboolean copyFlag;
      const char *sig;

      const char *utf =
        (*env)->GetStringUTFChars (env, typeSignature, &copyFlag);

      sig = SSTRDUP (utf);
      if (copyFlag)
        (*env)->ReleaseStringUTFChars (env, typeSignature, utf);
      (*env)->DeleteLocalRef (env, typeSignature);
      return sig;
    }
}

fcall_type_t
fcall_type_for_java_class (JNIEnv *env, jclass class)
{
  fcall_type_t type;

  if (classp (env, class, c_Selector))
    type = fcall_type_selector;
  else if (classp (env, class, c_String))
    type = fcall_type_string;
  else if (classp (env, class, c_Class))
    type = fcall_type_class;
  else if (exactclassp (env, class, c_int))
    type = fcall_type_sint;
  else if (exactclassp (env, class, c_short))
    type = fcall_type_sshort;
  else if (exactclassp (env, class, c_long))
    type = fcall_type_slong;
  else if (exactclassp (env, class, c_boolean))
    type = fcall_type_boolean;
  else if (exactclassp (env, class, c_byte))
    type = fcall_type_uchar;
  else if (exactclassp (env, class, c_char))
    type = fcall_type_schar;
  else if (exactclassp (env, class, c_float))
    type = fcall_type_float;
  else if (exactclassp (env, class, c_double))
    type = fcall_type_double;
  else if (exactclassp (env, class, c_void))
    type = fcall_type_void;
  else
    type = fcall_type_object;
  return type;
}

jclass
java_find_class (JNIEnv *env, const char *javaClassName, BOOL failFlag)
{
  jobject ret;
  jobject throwable;
  
  (*env)->ExceptionClear (env);
  ret = (*env)->FindClass (env, javaClassName);
  
  if (failFlag)
    {
      if ((throwable = (*env)->ExceptionOccurred (env)) != NULL)
        (*env)->ExceptionDescribe (env);
    }
  else
    (*env)->ExceptionClear (env);
  return ret;
}

const char *
java_get_class_name (JNIEnv *env, jclass class)
{
  jobject string;
  const char *ret;

  if (!(string = (*env)->CallObjectMethod (env, class, m_ClassGetName)))
    abort ();

  ret = swarm_directory_java_copy_string (env, string);
  (*env)->DeleteLocalRef (env, string);
  return ret;
}

void
java_object_setVariable (jobject javaObject, const char *ivarName, void *inbuf)
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
      abort ();
    
    if (isArray)
      {
        jobject ary = GETVALUE (Object);
        unsigned rank;
        
        {
          type = java_getTypeInfo (ary, &rank, NULL);
          {
            unsigned dims[rank];
            
            java_getTypeInfo (ary, &rank, dims);
            java_storeArray (ary, type, rank, dims, inbuf);
          }
        }
        (*jniEnv)->DeleteLocalRef (jniEnv, ary);
      }
    else
      {
#define _SETVALUE(uptype,value) \
    (*jniEnv)->Set##uptype##Field (jniEnv, javaObject, fid, value)
#define SETVALUE(uptype, value) _SETVALUE(uptype, value)
        
        types_t *buf = inbuf;
        switch (type)
          {
          case fcall_type_object:
            SETVALUE (Object, SD_JAVA_FINDJAVA (jniEnv, buf->object));
            break;
          case fcall_type_class:
            SETVALUE (Object, SD_JAVA_FINDJAVACLASS (jniEnv, buf->class));
            break;
          case fcall_type_string:
            SETVALUE (Object, (*jniEnv)->NewStringUTF (jniEnv, buf->string));
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
          default:
            raiseEvent (InvalidArgument,
                        "Unhandled fcall type `%d'", type);
            break;
          }
#undef SETVALUE
#undef _SETVALUE
        (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
      }
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
  return type;
}

unsigned
java_object_getVariableElementCount (jobject javaObject,
                                     const char *ivarName,
                                     fcall_type_t itype,
                                     unsigned irank,
                                     unsigned *idims)
{
  unsigned count = 1;
  
  if (!javaObject)
    abort ();
  {
    BOOL isArray;
    jclass javaClass = (*jniEnv)->GetObjectClass (jniEnv, javaObject);
    jfieldID fid =
      class_java_find_field (javaClass, ivarName, NULL, &isArray);
    
    if (!fid)
      abort ();
    
    if (isArray)
      {
        jobject ary = GETVALUE (Object);
        unsigned rank;
        fcall_type_t type = java_getTypeInfo (ary, &rank, NULL);
        
        if (rank != irank)
          raiseEvent (SourceMessage, "array rank mismatch %u != %u\n",
                      rank, irank);
        if (type != itype)
          raiseEvent (SourceMessage, "array type mismatch %u != %u\n",
                      type, itype);
        {
          unsigned dims[rank];
          unsigned i;
          
          java_getTypeInfo (ary, &rank, dims);
          for (i = 0; i < rank; i++)
            {
              if (dims[i] != idims[i])
                raiseEvent (SourceMessage,
                            "idims[%u] %u != dims[%u] %u",
                            i, idims[i], i, dims[i]);
              count *= dims[i];
            }
        }
      }
    (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
  }
  return count;
}

static jobject
get_swarmEnvironment_field (JNIEnv *env,
			    jobject swarmEnvironment,
			    const char *fieldName)
{
  jobject fieldObject, ret;
  jstring str = (*env)->NewStringUTF (env, fieldName);

  if (!(fieldObject =
	(*env)->CallObjectMethod (env,
				  c_SwarmEnvironment, 
				  m_ClassGetDeclaredField,
                                  str)))
    abort ();
  (*env)->DeleteLocalRef (env, str);
  
  ret = (*env)->CallObjectMethod (env,
                                  fieldObject,
                                  m_FieldGetObject,
                                  swarmEnvironment);
  (*env)->DeleteLocalRef (env, fieldObject);
  return ret;
}

static void
create_bootstrap_refs (JNIEnv *env)
{
  jclass lref;

  if (!(lref = (*env)->FindClass (env, "swarm/Primitives")))
    abort ();
  c_Primitives = (*env)->NewGlobalRef (env, lref);
  (*env)->DeleteLocalRef (env, lref);
  
  if (!(m_PrimitivesGetTypeMethod =
        (*env)->GetStaticMethodID (env, c_Primitives, "getTypeMethod",
                                   "(Ljava/lang/String;)Ljava/lang/reflect/Method;")))
    abort ();
  
  if (!(lref = (*env)->FindClass (env, "java/lang/reflect/Method")))
    abort ();
  c_Method = (*env)->NewGlobalRef (env, lref);
  (*env)->DeleteLocalRef (env, lref);

  if (!(m_MethodGetReturnType =
        (*env)->GetMethodID (env, c_Method, "getReturnType",
                             "()Ljava/lang/Class;")))
    abort();
}

static jclass
get_java_lang_class (JNIEnv *env, const char *name)
{
  char class_name_buf[10 + strlen (name) + 1];
  char *p;
  jclass ret, clazz;
  
  p = stpcpy (class_name_buf, "java/lang/");
  p = stpcpy (p, name);
  if (!(clazz = (*env)->FindClass (env, class_name_buf)))
    abort ();
  
  ret = (*env)->NewGlobalRef (env, clazz);
  (*env)->DeleteLocalRef (env, clazz);
  return ret;
}

static void
create_class_refs (JNIEnv *env)
{
  jobject lref;

  jclass get_primitive (const char *name)
    {
#if 0
      return get_type_field_for_class (env, get_java_lang_class (env, name));
#else
      jobject nameString = (*env)->NewStringUTF (env, name);
      jobject method;
      jclass lref, returnType;
      
      if (!(method = (*env)->CallStaticObjectMethod (env,
                                                     c_Primitives,
                                                     m_PrimitivesGetTypeMethod,
                                                     nameString)))
        abort ();
      (*env)->DeleteLocalRef (env, nameString);
      if (!(lref = (*env)->CallObjectMethod (env,
                                             method,
                                             m_MethodGetReturnType,
                                             method)))
        abort ();
      returnType = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);
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

      c_Boolean = get_java_lang_class (env, "Boolean");
      c_Char = get_java_lang_class (env, "Character");
      c_Byte= get_java_lang_class (env, "Byte");
      c_Integer = get_java_lang_class (env, "Integer");
      c_Short = get_java_lang_class (env, "Short");
      c_Long = get_java_lang_class (env, "Long");
      c_Float = get_java_lang_class (env, "Float");
      c_Double = get_java_lang_class (env, "Double");
     
      c_String = get_java_lang_class (env, "String");
      c_Object = get_java_lang_class (env, "Object");
      c_Class = get_java_lang_class (env, "Class");

      if (!(lref = (*env)->FindClass (env, "java/lang/reflect/Field")))
        abort ();
      c_Field = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env,
                                      "java/lang/reflect/Modifier")))
	abort ();
      c_Modifier = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env, "java/util/Collection")))
        abort ();
      c_Collection = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env, "swarm/Selector")))
        abort ();
      c_Selector = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);
      
      if (!(lref = (*env)->FindClass (env, "swarm/PhaseCImpl")))
        abort ();
      c_PhaseCImpl = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      if (!(lref = (*env)->FindClass (env,
                                      "swarm/SwarmEnvironment")))
	abort ();
      c_SwarmEnvironment = (*env)->NewGlobalRef (env, lref);
      (*env)->DeleteLocalRef (env, lref);

      initFlag = YES;
   }
}

static void 
create_method_refs (JNIEnv *env)
{
  jmethodID findMethodID (const char *name, jclass clazz)
    {
      char sig[31 + strlen (name) + 1 + 1];
      char *p;

      jmethodID res;
      p = stpcpy (sig, "(Ljava/lang/String;)Ljava/lang/");
      p = stpcpy (p, name);
      p = stpcpy (p, ";");

      if (!(res = (*env)->GetStaticMethodID (env, clazz, "valueOf", sig)))
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
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(Z)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfChar = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(C)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfInt = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(I)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfLong = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(J)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfFloat = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(F)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfDouble = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(D)Ljava/lang/String;")))
    abort ();

  if (!(m_StringValueOfObject = 
      (*env)->GetStaticMethodID (env, c_String, "valueOf", 
				 "(Ljava/lang/Object;)Ljava/lang/String;")))
    abort ();

  if (!(m_FieldSet = 
	(*env)->GetMethodID (env, c_Field, "set", 
			     "(Ljava/lang/Object;Ljava/lang/Object;)V")))
    abort();

  if (!(m_FieldSetChar = 
	(*env)->GetMethodID (env, c_Field, "setChar", 
			     "(Ljava/lang/Object;C)V")))
    abort();

  if (!(m_ClassGetField =
      (*env)->GetMethodID (env, c_Class, "getField",
			   "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();

  if (!(m_ClassGetDeclaredField =
      (*env)->GetMethodID (env, c_Class, "getDeclaredField",
			   "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();

  if (!(m_ClassGetDeclaredFields =
  	(*env)->GetMethodID (env, c_Class, "getDeclaredFields",
			     "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetDeclaredMethods =
  	(*env)->GetMethodID (env, c_Class, "getDeclaredMethods",
  		     "()[Ljava/lang/reflect/Method;")))
    abort();

  if (!(m_ClassGetFields =
  	(*env)->GetMethodID (env, c_Class, "getFields",
			     "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetName = 
	(*env)->GetMethodID (env, c_Class, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_ClassIsArray =
	(*env)->GetMethodID (env, c_Class, "isArray", "()Z")))
    abort();

  if (!(m_FieldGetName = 
	(*env)->GetMethodID (env, c_Field, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_FieldGetType =
	(*env)->GetMethodID (env, c_Field, "getType", "()Ljava/lang/Class;")))
    abort();
  
  if (!(m_FieldGetBoolean =
      (*env)->GetMethodID (env, c_Field, "getBoolean", 
			   "(Ljava/lang/Object;)Z")))
    abort();

  if (!(m_FieldGetChar =
      (*env)->GetMethodID (env, c_Field, "getChar", 
			   "(Ljava/lang/Object;)C")))
    abort();

  if (!(m_FieldGetShort =
      (*env)->GetMethodID (env, c_Field, "getShort", 
			   "(Ljava/lang/Object;)S")))
    abort();

  if (!(m_FieldGetInt =
      (*env)->GetMethodID (env, c_Field, "getInt", 
			   "(Ljava/lang/Object;)I")))
    abort();

  if (!(m_FieldGetLong =
      (*env)->GetMethodID (env, c_Field, "getLong", 
			   "(Ljava/lang/Object;)J")))
    abort();
  
  if (!(m_FieldGetFloat =
      (*env)->GetMethodID (env, c_Field, "getFloat", 
			   "(Ljava/lang/Object;)F")))
    abort();

  if (!(m_FieldGetDouble =
      (*env)->GetMethodID (env, c_Field, "getDouble", 
			   "(Ljava/lang/Object;)D")))
    abort();

  if (!(m_FieldGetObject =
        (*env)->GetMethodID (env, c_Field, "get",
                             "(Ljava/lang/Object;)Ljava/lang/Object;")))
    abort();
  
  if (!(m_MethodGetName =
	(*env)->GetMethodID (env, c_Method, "getName",
			     "()Ljava/lang/String;")))
    abort();

  if (!(m_MethodGetModifiers =
        (*env)->GetMethodID (env, c_Method, "getModifiers", "()I")))
    abort ();

  if (!(m_ModifierIsPublic = 
        (*env)->GetStaticMethodID (env, c_Modifier, "isPublic", "(I)Z")))
    abort ();

  if (!(m_ModifierIsStatic = 
        (*env)->GetStaticMethodID (env, c_Modifier, "isStatic", "(I)Z")))
    abort ();

  if (!(m_SelectorConstructor =
	(*env)->GetMethodID (env, c_Selector, "<init>", 
			     "(Ljava/lang/Class;Ljava/lang/String;Z)V")))
    abort();

  if (!(m_HashCode =
        (*env)->GetMethodID (env, c_Object, "hashCode", "()I")))
    abort ();

  if (!(m_Equals =
        (*env)->GetMethodID (env, c_Object, "equals",
                             "(Ljava/lang/Object;)Z")))
    abort ();
  
  if (!(m_PhaseCImpl_copy_creating_phase_to_using_phase = 
        (*env)->GetMethodID (env,
                             c_PhaseCImpl,
                             "_copy_creating_phase_to_using_phase",
                             "()V")))
    abort();
}


static void
create_field_refs (JNIEnv *env)
{

  if (!(f_nameFid = (*env)->GetFieldID (env, c_Selector, "signature", "Ljava/lang/String;")))
    abort ();
  if (!(f_retTypeFid = (*env)->GetFieldID (env, c_Selector, "retType", "Ljava/lang/Class;")))
    abort ();
  if (!(f_argTypesFid = (*env)->GetFieldID (env, c_Selector, "argTypes", "[Ljava/lang/Class;")))
    abort ();
  if (!(f_typeSignatureFid = (*env)->GetFieldID (env, c_Selector, "typeSignature", "Ljava/lang/String;")))
    abort ();
  if (!(f_objcFlagFid = (*env)->GetFieldID (env, c_Selector, "objcFlag", "Z")))
    abort ();

  if (!(f_nextPhase = (*env)->GetFieldID (env, c_PhaseCImpl, "nextPhase", 
                                          "Ljava/lang/Object;")))
    abort();
}

static void
create_refs (JNIEnv *env)
{
  create_bootstrap_refs (env);
  create_class_refs (env);
  create_method_refs (env);
  create_field_refs (env);
}

void
java_associate_objects (JNIEnv *env, jobject swarmEnvironment)
{
  void associate (const char *fieldName, id objcObject)
    {
      jobject lref = get_swarmEnvironment_field (env,
                                                 swarmEnvironment,
                                                 fieldName);
      SD_JAVA_ADD (env, lref, objcObject);
      (*env)->DeleteLocalRef (env, lref);
    }
#define ASSOCIATE(fieldName) associate (#fieldName, fieldName)
  create_refs (env);

  ASSOCIATE (globalZone);

  ASSOCIATE (hdf5Archiver);
  ASSOCIATE (lispArchiver);
  ASSOCIATE (hdf5AppArchiver);
  ASSOCIATE (lispAppArchiver);
  
  {
    extern id <Symbol> Randomized;
    extern id <Symbol> Sequential;
    
    ASSOCIATE (Randomized);
    ASSOCIATE (Sequential);
  }

  {
    extern id probeLibrary, probeDisplayManager;
    
    ASSOCIATE (probeLibrary);
    ASSOCIATE (probeDisplayManager);
  }
   
  {
    extern id randomGenerator, uniformIntRand, uniformDblRand;
    
    ASSOCIATE (randomGenerator);
    ASSOCIATE (uniformIntRand);
    ASSOCIATE (uniformDblRand);
  }

  {
    extern id <Symbol> ControlStateRunning, ControlStateStopped,
      ControlStateStepping, ControlStateQuit,ControlStateNextTime;
    
    ASSOCIATE (ControlStateRunning);
    ASSOCIATE (ControlStateStopped);
    ASSOCIATE (ControlStateStepping);
    ASSOCIATE (ControlStateQuit);
    ASSOCIATE (ControlStateNextTime);
  }

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
}

unsigned
swarm_directory_java_hash_code (JNIEnv *env, jobject javaObject)
{
  int hashCode;

  hashCode = (*env)->CallIntMethod (env, javaObject, m_HashCode);
  return (hashCode < 0 ? - hashCode : hashCode) % DIRECTORY_SIZE;
}

BOOL
java_selector_p (JNIEnv *env, jobject javaObject)
{
  return (*env)->IsInstanceOf (env, javaObject, c_Selector);
}


static DirectoryEntry *
swarm_directory_java_find (JNIEnv *env, jobject javaObject)
{
  if (javaObject)
    {
      id ret;
      unsigned index = swarm_directory_java_hash_code (env, javaObject);
      id <Map> m = swarmDirectory->table[index];
      DirectoryEntry *findEntry = JAVA_FINDENTRY (javaObject);
      
      findEntry->foreignObject.java = javaObject;
      ret = m ? [m at: findEntry] : nil;
      return ret;
    }
  return nil;
}

id
swarm_directory_java_ensure_objc (JNIEnv *env, jobject javaObject)
{
  if (!javaObject)
    return nil;
  else
    {
      DirectoryEntry *result; 
      
      if (!javaObject)
        return nil;
  
      result = swarm_directory_java_find (env, javaObject);
      
      if ((*env)->IsInstanceOf (env, javaObject, c_String))
        {
          jboolean isCopy;
          const char *utf, *str;
          
          utf = (*env)->GetStringUTFChars (env, javaObject, &isCopy);
          str = ZSTRDUP (getZone (swarmDirectory), utf);
          if (isCopy)
            (*env)->ReleaseStringUTFChars (env, javaObject, utf);
          
          if (result)
            {
              const char *last = (const char *) result->object;

              result = SD_JAVA_SWITCHOBJC (env, javaObject, (id) str);
	      ZFREEBLOCK (getZone (swarmDirectory), (void *) last);
            }
          else
            result = SD_JAVA_ADD (env, javaObject, (id) str);
        }
      else if (!result)
        result =
          SD_JAVA_ADD (env, javaObject, 
                       ((*env)->IsInstanceOf (env, javaObject, c_Collection)
                        ? [JavaCollection create: globalZone]
                        : [JavaProxy create: globalZone]));

      return result->object;
    }
}

jobject
swarm_directory_objc_ensure_java (JNIEnv *env, id object)
{
  DirectoryEntry *result; 

  if (!object)
    return 0;

  result = swarm_directory_objc_find (env, object);
  
  if (!result)
    {
      Class class = getClass (object);
      jclass javaClass = swarm_directory_objc_find_java_class (env, class);
      jobject lref = swarm_directory_java_instantiate (env, javaClass);
      
      result = SD_JAVA_ADD (env, lref, object);
      (*env)->DeleteLocalRef (env, lref);
      (*env)->DeleteLocalRef (env, javaClass);
    }
  else if (result->type != foreign_java)
    abort ();
  return result->foreignObject.java;
}

id
swarm_directory_java_find_objc (JNIEnv *env, jobject javaObject)
{
  DirectoryEntry *entry = swarm_directory_java_find (env, javaObject);

  return entry ? entry->object : nil;
}

jobject
swarm_directory_java_next_phase (JNIEnv *env, jobject jobj)
{
  (*env)->CallVoidMethod (env, jobj, 
                          m_PhaseCImpl_copy_creating_phase_to_using_phase);
  return (*env)->GetObjectField (env, jobj, f_nextPhase);
}

static int
java_compareDirectoryEntries (DirectoryEntry *obj1, DirectoryEntry* obj2)
{
#if 0
  printf ("`%s'%p/%p vs %p\n",
          getObjcName (env, obj1->javaObject, obj2->object),
          object, javaObject,
          ((DirectoryEntry *) obj2)->javaObject);
#endif
  return (int) !(*jniEnv)->CallBooleanMethod (jniEnv,
                                              obj1->foreignObject.java,
                                              m_Equals,
                                              obj2->foreignObject.java);
}

static id <Map>
createDirectoryEntryMap (JNIEnv *env)
{
  return [[[Map createBegin: getZone (swarmDirectory)]
            setCompareFunction: java_compareDirectoryEntries]
           createEnd];
}

static void
swarm_directory_switch_java_entry (JNIEnv *env, 
                                   DirectoryEntry *entry,
                                   jobject javaObject)
{
  unsigned index;
  id <Map> m;
  id <Map> *table = swarmDirectory->table;
  
  javaObject = (*env)->NewGlobalRef (env, javaObject);
  index = swarm_directory_java_hash_code (env, entry->foreignObject.java);
  m = table[index];
  [m remove: entry];
  (*env)->DeleteGlobalRef (env, entry->foreignObject.java);
  
  index = swarm_directory_java_hash_code (env, javaObject);
  entry->foreignObject.java = javaObject;
  if (!table[index])
    table[index] = createDirectoryEntryMap (env);

  [table[index] at: entry insert: entry];
}

DirectoryEntry *
swarm_directory_java_switch_phase (JNIEnv *env,
                                   id nextPhase,
                                   jobject currentJavaPhase)
{
  jobject nextJavaPhase = SD_JAVA_NEXTJAVAPHASE (env, currentJavaPhase);
  id currentPhase = SD_JAVA_FINDOBJC (env, currentJavaPhase);
  DirectoryEntry *retEntry;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  if (currentPhase != nextPhase)
    {
      id entry = JAVA_ENTRY (nextPhase, currentJavaPhase);

      DirectoryEntry **entryptr = 
        (DirectoryEntry **) avl_probe (objc_tree, entry);

      if (*entryptr != entry)
        abort ();

      swarm_directory_switch_java_entry (env, entry, nextJavaPhase);
      {
        id ret;
        
        ret = avl_delete (objc_tree, OBJC_FINDENTRY (currentPhase));
        if (!ret)
          abort ();
        swarm_directory_entry_drop (env, ret);
      }
      retEntry = *entryptr;
    }
  else
    {
      DirectoryEntry *entry = avl_find (objc_tree, OBJC_FINDENTRY (nextPhase));
      
      if (!entry)
        abort ();

      swarm_directory_switch_java_entry (env, entry, nextJavaPhase);
      
      retEntry = entry;
    }
  (*env)->DeleteLocalRef (env, nextJavaPhase);
  return retEntry;
}

SEL
swarm_directory_java_ensure_selector (JNIEnv *env, jobject jsel)
{
  SEL sel;

  if (!jsel)
    sel = NULL;
  else if (!(sel = (SEL) SD_JAVA_FINDOBJC (env, jsel)))
    {
      char *name, *p;
      unsigned i;
      jboolean copyFlag;
      jboolean objcFlag;
      jarray argTypes;
      jsize argCount;
        
      objcFlag = (*env)->GetBooleanField (env, jsel, f_objcFlagFid);
      argTypes = (*env)->GetObjectField (env, jsel, f_argTypesFid);
      argCount = (*env)->GetArrayLength (env, argTypes);

      {
        jstring string;
        const char *utf;
        
        string = (*env)->GetObjectField (env, jsel, f_nameFid);
        utf = (*env)->GetStringUTFChars (env, string, &copyFlag);
        
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
          (*env)->ReleaseStringUTFChars (env, string, utf);
        (*env)->DeleteLocalRef (env, string);
      }

      {
        jsize ti;
        char signatureBuf[(argCount + 3) * 2 + 1], *p = signatureBuf;
          
        void add_type (char type)
          {
            *p++ = type;
            *p++ = '0';
            *p = '\0';
          }
        void add (jclass class)
          {
            const char *type = 
              objc_type_for_fcall_type (fcall_type_for_java_class (env, class));
            add_type (*type);
            [globalZone free: (void *) type];
          }
        {
          jobject retType = (*env)->GetObjectField (env, jsel, f_retTypeFid);
          
          add (retType);
          (*env)->DeleteLocalRef (env, retType);
        }
        add_type (_C_ID);
        add_type (_C_SEL);

        for (ti = 0; ti < argCount; ti++)
          {
            jobject lref = (*env)->GetObjectArrayElement (env, argTypes, ti);

            add (lref);
            (*env)->DeleteLocalRef (env, lref);
          }
      
        sel = sel_get_any_typed_uid (name);
        if (sel)
          {
            if (!sel_get_typed_uid (name, signatureBuf))
              raiseEvent (WarningMessage,
                          "Method `%s' type (%s) differs from Swarm "
                          "method's type (%s)\n",
                          name, signatureBuf, sel->sel_types);
          }
        else
          {
            const char *type =
              mframe_build_signature (signatureBuf, NULL, NULL, NULL);

            sel = sel_register_typed_name (name, type);
          }
      }
      
      SD_JAVA_ADD (env, jsel, (id) sel);

      (*env)->DeleteLocalRef (env, argTypes);
      SFREEBLOCK (name);
    }
  return sel;
}

Class
swarm_directory_java_ensure_class (JNIEnv *env, jclass javaClass)
{
  Class objcClass;

  if (!(objcClass = SD_JAVA_FINDOBJC (env, javaClass)))
    {
      const char *className = java_get_class_name (env, javaClass);

      objcClass = objc_class_for_class_name (className);
      FREECLASSNAME (className);
      
      // if the corresponding class does not exist create new Java Proxy
      
      if (objcClass == nil)
        objcClass = [JavaProxy create: globalZone];
      SD_JAVA_ADD (env, (jobject) javaClass, (id) objcClass);
    }
  return objcClass;
}

const char *
swarm_directory_java_copy_string (JNIEnv *env, jstring javaString)
{
  jboolean isCopy;
  const char *str = (*env)->GetStringUTFChars (env, javaString, &isCopy);
  const char *ret = SSTRDUP (str);

  if (isCopy)
    (*env)->ReleaseStringUTFChars (env, javaString, str);
  return ret;
}

jobject
swarm_directory_objc_find_java (JNIEnv *env, id object)
{
  DirectoryEntry *entry = swarm_directory_objc_find (env, object);

  if (entry)
    {
      if (entry->type != foreign_java)
        abort ();
      return entry->foreignObject.java;
    }
  else
    return NULL;
}


DirectoryEntry *
swarm_directory_java_add (JNIEnv *env, id object, jobject lref)
{
  unsigned index;
  id <Map> m;
  DirectoryEntry *entry;
  jobject javaObject = (*env)->NewGlobalRef (env, lref);
  id *table = swarmDirectory->table;
  
  entry = JAVA_ENTRY (object, javaObject);
  index = swarm_directory_java_hash_code (env, javaObject);
  m = table[index];

  if (m == nil)
    {
      m = createDirectoryEntryMap (env);
      table[index] = m;
    }
  [m at: entry insert: entry];
  avl_probe (swarmDirectory->objc_tree, entry);
  return entry;
}

#if 0
DirectoryEntry *
swarm_directory_switch_java (JNIEnv *env,
                             id object,
                             jobject javaObject)
{
  DirectoryEntry *entry;
  
  if (!(entry = avl_find (swarmDirectory->objc_tree, OBJC_FINDENTRY (object))))
    abort ();
  swarm_directory_switch_java_entry (env, entry, javaObject);
  return entry;
}
#endif

DirectoryEntry *
swarm_directory_java_switch_objc (JNIEnv *env,
                                  id object,
                                  jobject javaObject)
{
  DirectoryEntry *entry;
  unsigned index;
  id <Map> m;
  id *table = swarmDirectory->table;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  index = swarm_directory_java_hash_code (env, javaObject);
  m = table[index];
  entry = [table[index] at: JAVA_FINDENTRY (javaObject)];
  if (!entry)
    abort ();
  
  if (!avl_delete (objc_tree, entry))
    abort ();
  entry->object = object;

  {
    void **foundEntry;
    
    foundEntry = avl_probe (objc_tree, entry);
    if (*foundEntry != entry)
      abort ();
  }
  return entry;
}

static const char *
java_classname_for_typename (JNIEnv *env, const char *typeName, BOOL usingFlag)
{
  if (strcmp (typeName, "Create_byboth") == 0)
    return DUPCLASSNAME ("swarm/CustomizedType");
  else
    {
      extern const char *swarm_lookup_module (const char *name);
      const char *module = swarm_lookup_module (typeName);
      size_t modulelen = module ? strlen (module) + 1 : 0;
      char javaClassName[5 + 1 + modulelen + strlen (typeName) + 5 + 1];
      char *p;
      
      p = stpcpy (javaClassName, "swarm/");
      if (module)
        {
          p = stpcpy (p, module);
          p = stpcpy (p, "/");
        }
      p = stpcpy (p, typeName);
      if (!usingFlag)
        p = stpcpy (p, "C");
      p = stpcpy (p, "Impl");
      return DUPCLASSNAME (javaClassName);
    }
}

static const char *
objcFindJavaClassName (JNIEnv *env, Class class)
{
  const char *javaClassName;
  
  if (getBit (class->info, _CLS_DEFINEDCLASS))
    {
      Type_c *typeImpl;
      Class_s *nextPhase;

      nextPhase = ((BehaviorPhase_s *) class)->nextPhase;
      typeImpl = [class getTypeImplemented];
      javaClassName = java_classname_for_typename (env,
						   typeImpl->name,
						   nextPhase == NULL); 
    }
  else
    {
      Type_c *typeImpl;
      typeImpl = [class getTypeImplemented];

      if (typeImpl)
        javaClassName =
	  java_classname_for_typename (env, typeImpl->name, YES);
      else 
        javaClassName =
	  java_classname_for_typename (env, class->name, YES);
    }
  return javaClassName;
}

jclass
swarm_directory_objc_find_java_class (JNIEnv *env, Class class)
{
  jclass ret;
  const char *javaClassName = objcFindJavaClassName (env, class);

  ret = java_find_class (env, javaClassName, YES);
  FREECLASSNAME (javaClassName);
  return ret;
}

jobject
swarm_directory_java_instantiate (JNIEnv *env, jclass clazz)
{
  jmethodID mid;

  if (!(mid = (*env)->GetMethodID (env, clazz, "<init>", "()V")))
    abort ();
  return (*env)->NewObject (env, clazz, mid);
}

const char *
swarm_directory_java_class_name (JNIEnv *env, jobject obj)
{
  jclass class;
  const char *ret;

  if (!(class = (*env)->GetObjectClass (env, obj)))
    abort ();

  ret = java_get_class_name (env, class);
  (*env)->DeleteLocalRef (env, class);
  return ret;
}

#endif
