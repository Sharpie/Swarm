#include <jni.h>
#import <objectbase/SwarmObject.h>
#import <collections/List.h>

@interface JavaObject: SwarmObject
{
  id env;
  JNIEnv *jniEnv;
  jclass classId;
  jobject jobj;
  id <List> arguments;
}

+ createBegin: aZone;
- setClassIdFromName: (const char *)className;
- setClassIdFromSwarmName: (const char *)className;
- setClassIdFromAWTName: (const char *)className;
- addInt: (int)value;
- addString: (const char *)str;
- addObject: object;
- createBackingObject;
- createEnd;

- (jobject)getJobject;
- (jclass)getJavaClass;
- (const char *)getJavaClassNameFor: (jobject)object;

// Various flavors of static method invocation.
- (jobject)callObjectStaticMethod: (jmethodID)mid;

- (jmethodID)findMethod: (const char *)method signature: (const char *)sig;
- (jmethodID)findStaticMethod: (const char *)method signature: (const char *)sig;
- (jfieldID)getFieldID: (const char *)field signature: (const char *)sig;
- (jfieldID)getStaticFieldID: (const char *)field signature: (const char *)sig;

// Retrieve field contents.
- (int)getIntField: (jobject)obj Field: (const char *)field;
- (int)getStaticIntField: (const char *)field;

- (jint)callIntMethod: (jmethodID)methodId;
- (void)callVoidMethod: (jmethodID)methodId;
- (void)callVoidMethod: (jmethodID)methodId: (int)val1;
- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2;
- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
		       : (int)val3;
- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
                      : (int)val3 : (int)val4;
- (void)callVoidMethod: (jmethodID)methodId : (int)val1 : (int)val2 
                      : (int)val3 : (int)val4 : (int)val5;
- (void)callVoidMethod: (jmethodID)methodId
                      O: (jobject)val1 : (int)val2 : (int)val3;
- (void)callVoidMethod: (jmethodID)methodId D: (double)val1 D: (double)val2;
- (void)callVoidMethod: (jmethodID)methodId
                     D: (double)val1 D: (double)val2
                     D: (double)val3 D: (double)val4;
- (void)callVoidMethod: (jmethodID)methodId S: (const char *)val1;
- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 S: (const char *)val2;
- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 I: (int)val2;
- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 
                     D: (double)val2
                     D: (double)val3;
- (void)callVoidMethod: (jmethodID)methodId
                     S: (const char *)val1 
                     S: (const char *)val2
                     D: (double)val3;
- (jobject)callObjectMethod: (jmethodID)methodId;
- (jobject)callObjectMethod: (jmethodID)methodId O: (jobject)arg1;
@end
