#import <awtobjc/JavaEnv.h>

jfieldID
getFieldID (JNIEnv *jniEnv, jclass classId, const char *field, const char *sig)
{
  jfieldID fid = (*jniEnv)->GetFieldID (jniEnv, classId, field, sig);
  
  if (fid == 0)
    {
      fprintf (stderr, "error: can't find field %s\n", field);
      exit(1);
    }
  fprintf (stderr, "Field: %s\n", field);
  
  return fid;
}

jfieldID
getStaticFieldID (JNIEnv *jniEnv, jclass classId, const char *field, const char *sig)
{
  jfieldID fid = (*jniEnv)->GetStaticFieldID (jniEnv, classId, field, sig);
  
  if (fid == 0)
    {
      fprintf (stderr, "error: can't find field %s\n", field);
      exit (1);
    }
  
  fprintf (stderr, "Field: %s\n", field);
  
  return fid;
}

int
getStaticIntField (JNIEnv *jniEnv, jclass classId, const char *field)
{
  jfieldID fid = getStaticFieldID (jniEnv, classId, field, "I");
  int val = (*jniEnv)->GetStaticIntField (jniEnv, classId, fid);

  fprintf (stderr, "getStaticInt %s=%d\n", field, val);

  return val;
}

int
getIntField (JNIEnv *jniEnv, jobject obj, const char *field)
{
  jclass class = (*jniEnv)->GetObjectClass (jniEnv, obj);
  jfieldID fid = getFieldID (jniEnv, class, field, "I");
  return (int)(*jniEnv)->GetIntField (jniEnv, obj, fid);    
}

jmethodID
findMethod (JNIEnv *jniEnv, jclass classId, const char *methodName, const char *signature)
{
  jmethodID methodId = (*jniEnv)->GetMethodID (jniEnv, classId, methodName, signature);
  
  if (methodId == 0)
    {
      fprintf(stderr, "warning: can't find method %s\n", methodName);
      return methodId;
    }
  
  return methodId;
}

jmethodID
findStaticMethod (JNIEnv *jniEnv, jclass classId, const char *methodName, const char *signature)
{
  jmethodID methodId = (*jniEnv)->GetStaticMethodID (jniEnv, classId, methodName, signature);
  
  if (methodId == 0)
    {
      fprintf(stderr, "warning: can't find static method %s\n", methodName);
      return methodId;
    }
  
  return methodId;
}

jclass
findClass (JNIEnv *jniEnv, const char *className)
{
  return (*jniEnv)->FindClass (jniEnv, className);
}
