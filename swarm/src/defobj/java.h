#import <defobj/directory.h> // DIRECTORY_SIZE, DirectoryEntry

#include <swarmconfig.h>
#ifdef JNI_H_NEEDS_INT64
#define __int64 INT64
#endif
#ifdef __osf__
#define _REENTRANT
#endif

#undef SIZEOF_LONG /* Kaffe can define this (it's from swarmconfig.h) */
#include <jni.h>

extern JNIEnv *jniEnv;

extern const char *java_signature_for_fcall_type (fcall_type_t type);
extern fcall_type_t fcall_type_for_java_class (jclass class);
extern const char *java_ensure_selector_type_signature (jobject jsel);
extern const char *java_get_class_name (jclass class);
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

extern void swarm_directory_java_associate_objects (jobject swarmEnvironment);
extern unsigned swarm_directory_java_hash_code (jobject javaObject);
extern id swarm_directory_java_ensure_objc (jobject javaObject);
extern id swarm_directory_java_find_objc (jobject javaObject);
extern jobject swarm_directory_objc_ensure_java (id object);
extern jobject swarm_directory_java_next_phase (jobject jobj);
extern jobject swarm_directory_objc_find_java (id object);
extern Class swarm_directory_java_ensure_class (jclass javaClass);
extern Class swarm_directory_java_find_class_named (const char *className);
extern Class swarm_directory_java_class_for_object (id object);
extern DirectoryEntry *swarm_directory_java_switch_phase (id nextPhase, jobject currentJavaPhase);
extern DirectoryEntry *swarm_directory_java_switch_objc (id object, jobject javaObject);
extern DirectoryEntry *swarm_directory_java_add (id object, jobject lref);
extern jclass swarm_directory_find_java_class (const char *javaClassName, BOOL failFlag);
extern jclass swarm_directory_objc_find_java_class (Class class);
extern SEL swarm_directory_java_ensure_selector (jobject jsel);

extern const char *java_copy_string (jstring javaString);
extern void java_cleanup_strings (const char **stringArray, size_t count);
extern BOOL java_selector_p (jobject javaObject);
extern const char *java_class_name (jobject obj);
extern void java_drop (jobject jobj);


#define SD_JAVA_FINDOBJC(jobj)  swarm_directory_java_find_objc (jobj)
#define SD_JAVA_ENSUREOBJC(jobj) swarm_directory_java_ensure_objc (jobj)
#define SD_JAVA_FINDJAVA(objc) swarm_directory_objc_find_java (objc)
#define SD_JAVA_ENSUREJAVA(objc) swarm_directory_objc_ensure_java (objc)
#define SD_JAVA_FINDJAVACLASS(objcClass) swarm_directory_objc_find_java_class (objcClass)
#define SD_JAVA_ADD(jobj, objc) swarm_directory_java_add (objc, jobj)
#define SD_JAVA_ADDJAVA(jobj, objc) swarm_directory_java_add (objc, jobj)->foreignObject.java
#define SD_JAVA_NEXTPHASE(jobj, objc) swarm_directory_java_switch_phase (objc, jobj)->foreignObject.java
#define SD_JAVA_SWITCHOBJC(jobj, newobjc) swarm_directory_java_switch_objc (newobjc, jobj)
#define SD_JAVA_NEXTJAVAPHASE(jobj) swarm_directory_java_next_phase (jobj)
#define SD_JAVA_ENSUREOBJCMETHOD(jobj) swarm_directory_java_ensure_selector (jobj)
#define SD_JAVA_ENSUREOBJCCLASS(jclazz) swarm_directory_java_ensure_class (jclazz)

#define JAVA_COPY_STRING(javaString) java_copy_string (javaString)
#define JAVA_CLEANUP_STRINGS(stringArray) java_cleanup_strings (stringArray, sizeof (stringArray) / sizeof (const char *))

#define JAVA_ENTRY(theObject,theJavaObject) [[[[DirectoryEntry createBegin: globalZone] setJavaObject: theJavaObject] setObject: theObject] createEnd]
#define JAVA_OBJCENTRY(theObject) JAVA_ENTRY(theObject,0)
#define JAVA_JAVAENTRY(theJavaObject) JAVA_ENTRY(0,theJavaObject)
#define JAVA_FINDENTRY(theJavaObject) ({ DirectoryEntry *_findEntry  = alloca (sizeof (DirectoryEntry)); _findEntry->foreignObject.java = theJavaObject; _findEntry; })

