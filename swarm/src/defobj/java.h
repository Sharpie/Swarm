#import <defobj/directory.h> // JNIEnv, jclass, jobject, DIRECTORY_SIZE, DirectoryEntry
#import "internal.h"

extern const char *java_signature_for_fcall_type (fcall_type_t type);
extern fcall_type_t fcall_type_for_java_class (JNIEnv *env, jclass class);
extern const char *java_ensure_selector_type_signature (JNIEnv *env, jobject jsel);
extern const char *java_get_class_name (JNIEnv *env, jclass class);
extern jclass java_find_class (JNIEnv *env, const char *javaClassName, BOOL failFlag);
extern void java_object_setVariable (jobject obj, const char *ivarName, void *inbuf);
extern void map_java_ivars (jobject javaObject,
                            void (*process_object) (const char *name,
                                                    fcall_type_t type,
                                                    void *ptr,
                                                    unsigned rank,
                                                    unsigned *dims));
extern fcall_type_t java_object_ivar_type (jobject javaObject, const char *ivarName, BOOL *isArrayPtr);
extern unsigned java_object_getVariableElementCount (jobject javaObject,
                                                     const char *ivarName,
                                                     fcall_type_t itype,
                                                     unsigned irank,
                                                     unsigned *idims);
extern void java_associate_objects (JNIEnv *env, jobject swarmEnvironment);
extern unsigned swarm_directory_java_hash_code (JNIEnv *env, jobject javaObject);
extern BOOL java_selector_p (JNIEnv *env, jobject javaObject);
extern id swarm_directory_java_ensure_objc (JNIEnv *env, jobject javaObject);
extern jobject swarm_directory_objc_ensure_java (JNIEnv *env, id object);
extern id swarm_directory_java_find_objc (JNIEnv *env, jobject javaObject);
extern jobject swarm_directory_java_next_phase (JNIEnv *env, jobject jobj);
extern DirectoryEntry *swarm_directory_java_switch_phase (JNIEnv *env, id nextPhase, jobject currentJavaPhase);
extern SEL swarm_directory_java_ensure_selector (JNIEnv *env, jobject jsel);
extern Class swarm_directory_java_ensure_class (JNIEnv *env, jclass javaClass);
extern const char *swarm_directory_java_copy_string (JNIEnv *env, jstring javaString);
extern jobject swarm_directory_objc_find_java (JNIEnv *env, id object);
extern DirectoryEntry *swarm_directory_java_switch_objc (JNIEnv *env,
                                                         id object,
                                                         jobject javaObject);
extern DirectoryEntry *swarm_directory_java_add (JNIEnv *env, id object, jobject lref);
extern const char *swarm_directory_java_class_name (JNIEnv *env, jobject obj);
extern jclass swarm_directory_find_java_class (JNIEnv *env, const char *javaClassName, BOOL failFlag);
extern jclass swarm_directory_objc_find_java_class (JNIEnv *env, Class class);
extern jobject swarm_directory_java_instantiate (JNIEnv *jniEnv, jclass clazz);
extern const char *swarm_directory_java_class_name (JNIEnv *env, jobject obj);

