#include <jni.h>

jfieldID getFieldID (JNIEnv *jniEnv, jclass classId, const char *field, const char *sig);

jfieldID getStaticFieldID (JNIEnv *jniEnv, jclass classId, const char *field, const char *sig);

int getIntField (JNIEnv *jniEnv, jobject obj, const char *field);
int getStaticIntField (JNIEnv *jniEnv, jclass jclass, const char *field);

jmethodID findMethod (JNIEnv *jniEnv, jclass classId, const char *name, const char *signature);

jmethodID findStaticMethod (JNIEnv *jniEnv, jclass classId, const char *name, const char *signature);

jclass findClass (JNIEnv *env, const char *name);
