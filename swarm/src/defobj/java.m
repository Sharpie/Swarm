#import "java.h"
#import <defobj/JavaProxy.h>
#import <defobj/JavaCollection.h>
#import <defobj/defalloc.h>
#import <defobj/Program.h> // Type_c
#include <objc/mframe.h> // mframe_build_signature
#import "internal.h" // FCALL_TYPE_COUNT

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
    type = java_get_class_name (class);
  else
    {
      const char *name = java_get_class_name (class);
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
              const char *sig = java_signature_for_class (class);

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
          *((id *) &buf[offset]) = SD_JAVA_ENSUREOBJC (obj);
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
                      
                  val.object = SD_JAVA_ENSUREOBJC (obj);
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
                      
                  val.object = SD_JAVA_FINDOBJC (sel);
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
            SETVALUE (Object, SD_JAVA_FINDJAVA (buf->object));
            break;
          case fcall_type_class:
            SETVALUE (Object, SD_JAVA_FINDJAVACLASS (buf->class));
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
get_swarmEnvironment_field (jobject swarmEnvironment,
			    const char *fieldName)
{
  jobject fieldObject, ret;
  jstring str = (*jniEnv)->NewStringUTF (jniEnv, fieldName);

  if (!(fieldObject =
	(*jniEnv)->CallObjectMethod (jniEnv,
                                     c_SwarmEnvironment, 
                                     m_ClassGetDeclaredField,
                                     str)))
    abort ();
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

      if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/Selector")))
        abort ();
      c_Selector = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      
      if (!(lref = (*jniEnv)->FindClass (jniEnv, "swarm/PhaseCImpl")))
        abort ();
      c_PhaseCImpl = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

      if (!(lref = (*jniEnv)->FindClass (jniEnv,
                                         "swarm/SwarmEnvironment")))
	abort ();
      c_SwarmEnvironment = (*jniEnv)->NewGlobalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);

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
    abort();
  
  if (!(m_FieldSetChar = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "setChar", 
                                "(Ljava/lang/Object;C)V")))
    abort();
  
  if (!(m_ClassGetField =
      (*jniEnv)->GetMethodID (jniEnv, c_Class, "getField",
                              "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();

  if (!(m_ClassGetDeclaredField =
      (*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredField",
                              "(Ljava/lang/String;)Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetDeclaredFields =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredFields",
                                "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetDeclaredMethods =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getDeclaredMethods",
                                "()[Ljava/lang/reflect/Method;")))
    abort();

  if (!(m_ClassGetFields =
  	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getFields",
                                "()[Ljava/lang/reflect/Field;")))
    abort();
  
  if (!(m_ClassGetName = 
	(*jniEnv)->GetMethodID (jniEnv, c_Class, "getName", "()Ljava/lang/String;")))
    abort();
  
  if (!(m_ClassIsArray =
	(*jniEnv)->GetMethodID (jniEnv, c_Class, "isArray", "()Z")))
    abort();

  if (!(m_FieldGetName = 
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "getName", "()Ljava/lang/String;")))
    abort();

  if (!(m_FieldGetType =
	(*jniEnv)->GetMethodID (jniEnv, c_Field, "getType", "()Ljava/lang/Class;")))
    abort();
  
  if (!(m_FieldGetBoolean =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getBoolean", 
                              "(Ljava/lang/Object;)Z")))
    abort();
  
  if (!(m_FieldGetChar =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getChar", 
                              "(Ljava/lang/Object;)C")))
    abort();

  if (!(m_FieldGetShort =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getShort", 
                              "(Ljava/lang/Object;)S")))
    abort();

  if (!(m_FieldGetInt =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getInt", 
                              "(Ljava/lang/Object;)I")))
    abort();

  if (!(m_FieldGetLong =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getLong", 
                              "(Ljava/lang/Object;)J")))
    abort();
  
  if (!(m_FieldGetFloat =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getFloat", 
                              "(Ljava/lang/Object;)F")))
    abort();

  if (!(m_FieldGetDouble =
      (*jniEnv)->GetMethodID (jniEnv, c_Field, "getDouble", 
                              "(Ljava/lang/Object;)D")))
    abort();

  if (!(m_FieldGetObject =
        (*jniEnv)->GetMethodID (jniEnv, c_Field, "get",
                                "(Ljava/lang/Object;)Ljava/lang/Object;")))
    abort();
  
  if (!(m_MethodGetName =
	(*jniEnv)->GetMethodID (jniEnv, c_Method, "getName",
                                "()Ljava/lang/String;")))
    abort();

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
    abort();

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
    abort();
}


static void
create_field_refs (void)
{

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
create_refs (void)
{
  create_bootstrap_refs ();
  create_class_refs ();
  create_method_refs ();
  create_field_refs ();
}

void
swarm_directory_java_associate_objects (jobject swarmEnvironment)
{
  void associate (const char *fieldName, id objcObject)
    {
      jobject lref = get_swarmEnvironment_field (swarmEnvironment, fieldName);
      SD_JAVA_ADD (lref, objcObject);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
    }
#define ASSOCIATE(fieldName) associate (#fieldName, fieldName)
  create_refs ();

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
swarm_directory_java_hash_code (jobject javaObject)
{
  int hashCode;

  hashCode = (*jniEnv)->CallIntMethod (jniEnv, javaObject, m_HashCode);
  return (hashCode < 0 ? - hashCode : hashCode) % DIRECTORY_SIZE;
}


static DirectoryEntry *
swarm_directory_java_find (jobject javaObject)
{
  if (javaObject)
    {
      id ret;
      unsigned index = swarm_directory_java_hash_code (javaObject);
      id <Map> m = swarmDirectory->table[index];
      DirectoryEntry *findEntry = JAVA_FINDENTRY (javaObject);
      ret = m ? [m at: findEntry] : nil;
      return ret;
    }
  return nil;
}

id
swarm_directory_java_ensure_objc (jobject javaObject)
{
  if (!javaObject)
    return nil;
  else
    {
      DirectoryEntry *result; 
      
      if (!javaObject)
        return nil;
  
      result = swarm_directory_java_find (javaObject);
      
      if ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_String))
        {
          jboolean isCopy;
          const char *utf, *str;
          
          utf = (*jniEnv)->GetStringUTFChars (jniEnv, javaObject, &isCopy);
          str = ZSTRDUP (getZone (swarmDirectory), utf);
          if (isCopy)
            (*jniEnv)->ReleaseStringUTFChars (jniEnv, javaObject, utf);
          
          if (result)
            {
              const char *last = (const char *) result->object;

              result = SD_JAVA_SWITCHOBJC (javaObject, (id) str);
	      ZFREEBLOCK (getZone (swarmDirectory), (void *) last);
            }
          else
            result = SD_JAVA_ADD (javaObject, (id) str);
        }
      else if (!result)
        result =
          SD_JAVA_ADD (javaObject, 
                       ((*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_Collection)
                        ? [JavaCollection create: globalZone]
                        : [JavaProxy create: globalZone]));
      
      return result->object;
    }
}

static jobject
java_instantiate (jclass clazz)
{
  jmethodID mid;

  if (!(mid = (*jniEnv)->GetMethodID (jniEnv, clazz, "<init>", "()V")))
    abort ();
  return (*jniEnv)->NewObject (jniEnv, clazz, mid);
}

jobject
swarm_directory_objc_ensure_java (id object)
{
  DirectoryEntry *result; 

  if (!object)
    return 0;

  result = swarm_directory_objc_find (object);
  
  if (!result)
    {
      Class class = getClass (object);
      jclass javaClass = swarm_directory_objc_find_java_class (class);
      jobject lref = java_instantiate (javaClass);
      
      result = SD_JAVA_ADD (lref, object);
      (*jniEnv)->DeleteLocalRef (jniEnv, lref);
      (*jniEnv)->DeleteLocalRef (jniEnv, javaClass);
    }
  else if (result->type != foreign_java)
    abort ();
  return result->foreignObject.java;
}

id
swarm_directory_java_find_objc (jobject javaObject)
{
  DirectoryEntry *entry = swarm_directory_java_find (javaObject);

  return entry ? entry->object : nil;
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
  printf ("`%s'%p/%p vs %p\n",
          getObjcName (obj1->javaObject, obj2->object),
          object, javaObject,
          ((DirectoryEntry *) obj2)->javaObject);
#endif
  return (int) !(*jniEnv)->CallBooleanMethod (jniEnv,
                                              obj1->foreignObject.java,
                                              m_Equals,
                                              obj2->foreignObject.java);
}

static id <Map>
createDirectoryEntryMap (void)
{
  return [[[Map createBegin: getZone (swarmDirectory)]
            setCompareFunction: java_compareDirectoryEntries]
           createEnd];
}

static void
swarm_directory_switch_java_entry (DirectoryEntry *entry, jobject javaObject)
{
  unsigned index;
  id <Map> m;
  id <Map> *table = swarmDirectory->table;
  
  javaObject = (*jniEnv)->NewGlobalRef (jniEnv, javaObject);
  index = swarm_directory_java_hash_code (entry->foreignObject.java);
  m = table[index];
  [m remove: entry];
  (*jniEnv)->DeleteGlobalRef (jniEnv, entry->foreignObject.java);
  
  index = swarm_directory_java_hash_code (javaObject);
  entry->foreignObject.java = javaObject;
  if (!table[index])
    table[index] = createDirectoryEntryMap ();

  [table[index] at: entry insert: entry];
}

DirectoryEntry *
swarm_directory_java_switch_phase (id nextPhase, jobject currentJavaPhase)
{
  jobject nextJavaPhase = SD_JAVA_NEXTJAVAPHASE (currentJavaPhase);
  id currentPhase = SD_JAVA_FINDOBJC (currentJavaPhase);
  DirectoryEntry *retEntry;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  if (currentPhase != nextPhase)
    {
      id entry = JAVA_ENTRY (nextPhase, currentJavaPhase);

      DirectoryEntry **entryptr = 
        (DirectoryEntry **) avl_probe (objc_tree, entry);

      if (*entryptr != entry)
        abort ();

      swarm_directory_switch_java_entry (entry, nextJavaPhase);
      {
        id ret;
        
        ret = avl_delete (objc_tree, OBJC_FINDENTRY (currentPhase));
        if (!ret)
          abort ();
        swarm_directory_entry_drop (ret);
      }
      retEntry = *entryptr;
    }
  else
    {
      DirectoryEntry *entry = avl_find (objc_tree, OBJC_FINDENTRY (nextPhase));
      
      if (!entry)
        abort ();

      swarm_directory_switch_java_entry (entry, nextJavaPhase);
      
      retEntry = entry;
    }
  (*jniEnv)->DeleteLocalRef (jniEnv, nextJavaPhase);
  return retEntry;
}

SEL
swarm_directory_java_ensure_selector (jobject jsel)
{
  SEL sel;

  if (!jsel)
    sel = NULL;
  else if (!(sel = (SEL) SD_JAVA_FINDOBJC (jsel)))
    {
      char *name, *p;
      unsigned i;
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
              objc_type_for_fcall_type (fcall_type_for_java_class (class));
            add_type (*type);
            [globalZone free: (void *) type];
          }
        {
          jobject retType = (*jniEnv)->GetObjectField (jniEnv, jsel, f_retTypeFid);
          
          add (retType);
          (*jniEnv)->DeleteLocalRef (jniEnv, retType);
        }
        add_type (_C_ID);
        add_type (_C_SEL);

        for (ti = 0; ti < argCount; ti++)
          {
            jobject lref = (*jniEnv)->GetObjectArrayElement (jniEnv, argTypes, ti);

            add (lref);
            (*jniEnv)->DeleteLocalRef (jniEnv, lref);
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
      
      SD_JAVA_ADD (jsel, (id) sel);

      (*jniEnv)->DeleteLocalRef (jniEnv, argTypes);
      SFREEBLOCK (name);
    }
  return sel;
}

Class
swarm_directory_java_ensure_class (jclass javaClass)
{
  Class objcClass;

  if (!(objcClass = SD_JAVA_FINDOBJC (javaClass)))
    {
      const char *className = java_get_class_name (javaClass);

      objcClass = objc_class_for_class_name (className);
      FREECLASSNAME (className);
      
      // if the corresponding class does not exist create new Java Proxy
      
      if (objcClass == nil)
        objcClass = [JavaProxy create: globalZone];
      SD_JAVA_ADD ((jobject) javaClass, (id) objcClass);
    }
  return objcClass;
}

jobject
swarm_directory_objc_find_java (id object)
{
  DirectoryEntry *entry = swarm_directory_objc_find (object);

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
swarm_directory_java_add (id object, jobject lref)
{
  unsigned index;
  id <Map> m;
  DirectoryEntry *entry;
  jobject javaObject = (*jniEnv)->NewGlobalRef (jniEnv, lref);
  id *table = swarmDirectory->table;
  
  entry = JAVA_ENTRY (object, javaObject);
  index = swarm_directory_java_hash_code (javaObject);
  m = table[index];

  if (m == nil)
    {
      m = createDirectoryEntryMap ();
      table[index] = m;
    }
  [m at: entry insert: entry];
  avl_probe (swarmDirectory->objc_tree, entry);
  return entry;
}

DirectoryEntry *
swarm_directory_java_switch_objc (id object, jobject javaObject)
{
  DirectoryEntry *entry;
  unsigned index;
  id <Map> m;
  id *table = swarmDirectory->table;
  avl_tree *objc_tree = swarmDirectory->objc_tree;
  
  index = swarm_directory_java_hash_code (javaObject);
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
java_classname_for_typename (const char *typeName, BOOL usingFlag)
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
objcFindJavaClassName (Class class)
{
  const char *javaClassName;
  
  if (getBit (class->info, _CLS_DEFINEDCLASS))
    {
      Type_c *typeImpl;
      Class_s *nextPhase;

      nextPhase = ((BehaviorPhase_s *) class)->nextPhase;
      typeImpl = [class getTypeImplemented];
      javaClassName = java_classname_for_typename (typeImpl->name,
						   nextPhase == NULL); 
    }
  else
    {
      Type_c *typeImpl;
      typeImpl = [class getTypeImplemented];

      if (typeImpl)
        javaClassName =
	  java_classname_for_typename (typeImpl->name, YES);
      else 
        javaClassName =
	  java_classname_for_typename (class->name, YES);
    }
  return javaClassName;
}

static jclass
java_find_class (const char *javaClassName, BOOL failFlag)
{
  jobject ret;
  jobject throwable;
  
  (*jniEnv)->ExceptionClear (jniEnv);
  ret = (*jniEnv)->FindClass (jniEnv, javaClassName);
  
  if (failFlag)
    {
      if ((throwable = (*jniEnv)->ExceptionOccurred (jniEnv)) != NULL)
        (*jniEnv)->ExceptionDescribe (jniEnv);
    }
  else
    (*jniEnv)->ExceptionClear (jniEnv);
  return ret;
}

jclass
swarm_directory_objc_find_java_class (Class class)
{
  jclass ret;
  const char *javaClassName = objcFindJavaClassName (class);

  ret = java_find_class (javaClassName, YES);
  FREECLASSNAME (javaClassName);
  return ret;
}

Class
swarm_directory_java_find_class_named (const char *className)
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
swarm_directory_java_class_for_object (id object)
{
  jobject jobj;
  
  if ((jobj = SD_JAVA_FINDJAVA (object)))
    {
      jclass jcls;
      const char *className;
      Class result;
      
      jcls = (*jniEnv)->GetObjectClass (jniEnv, jobj);
      className = java_class_name (jobj);
      result = objc_class_for_class_name (className);
      FREECLASSNAME (className);
      if (!result)
        if (!(result = SD_JAVA_FINDOBJC (jcls)))
          result = SD_JAVA_ENSUREOBJCCLASS (jcls);
      (*jniEnv)->DeleteLocalRef (jniEnv, jcls);
      return result;
    }
  abort ();
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
  jboolean isCopy;
  const char *str = (*jniEnv)->GetStringUTFChars (jniEnv, javaString, &isCopy);
  const char *ret = SSTRDUP (str);

  if (isCopy)
    (*jniEnv)->ReleaseStringUTFChars (jniEnv, javaString, str);
  return ret;
}

void
java_cleanup_strings (const char **stringArray, size_t count)
{
  size_t i;

  for (i = 0; i < count; i++)
    SFREEBLOCK (stringArray[i]);
}

void
java_drop (jobject jobj)
{
  (*jniEnv)->DeleteGlobalRef (jniEnv, jobj);
}

BOOL
java_selector_p (jobject javaObject)
{
  return (*jniEnv)->IsInstanceOf (jniEnv, javaObject, c_Selector);
}

#endif
