#include <jni.h>
#include <objc/Object.h>

extern id javaEnv;

@interface JavaEnv: Object
{
  JNIEnv *jniEnv;
  JavaVM *jvm;
  JDK1_1InitArgs vm_args;
}

+ new;

- init;
- (void)free;

- (JNIEnv *)getJNIEnv;
- (jclass)findClass: (const char *)className;

@end
