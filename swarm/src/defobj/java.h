#ifndef __defobj_java_h
#define __defobj_java_h
#import <Swarm/directory.h> // DIRECTORY_SIZE, ObjectEntry, SelectorEntry

#import <Swarm/swarmconfig.h>
#ifdef JNI_H_NEEDS_INT64
#define __int64 SWARM_INT64
#endif
#ifdef __osf__
#define _REENTRANT
#endif

#include <jni.h>

extern JNIEnv *jniEnv;

extern jclass java_find_class (const char *name, BOOL failFlag);
extern ObjectEntry *java_instantiate_pair (jclass clazz);
extern const char *java_signature_for_fcall_type (fcall_type_t type);
extern fcall_type_t fcall_type_for_java_class (jclass class);
extern const char *java_ensure_selector_type_signature (jobject jsel);
extern const char *java_get_class_name (jclass class);
extern void java_object_setVariable (jobject javaObject, const char *ivarName,
                                     fcall_type_t type,
                                     unsigned rank, unsigned *dims,
                                     void *inbuf);

extern void map_java_ivars (jobject javaObject,
                            void (*process_object) (const char *name,
                                                    fcall_type_t type,
                                                    void *ptr,
                                                    unsigned rank,
                                                    unsigned *dims));

extern void map_java_class_ivars (jclass class,
                                  void (*process_ivar) (const char *name,
                                                        fcall_type_t type));

extern fcall_type_t java_object_ivar_type (jobject javaObject, const char *ivarName, BOOL *isArrayPtr);

extern void java_create_refs ();
extern void swarm_directory_java_associate_objects_startup (jobject swarmEnvironment);
extern void swarm_directory_java_associate_objects (jobject swarmEnvironment);

extern void swarm_directory_java_associate_objects (jobject swarmEnvironment);
extern unsigned swarm_directory_java_hash_code (jobject javaObject);
extern id swarm_directory_java_ensure_objc (jobject javaObject);
extern id swarm_directory_java_find_objc (jobject javaObject);
extern id swarm_directory_java_find_swarm_objc (jobject javaObject);
extern jobject swarm_directory_java_next_phase (jobject jobj);
extern ObjectEntry *swarm_directory_java_switch_phase (Object_s *nextPhase, jobject currentJavaPhase);
extern ObjectEntry *swarm_directory_java_switch_objc (Object_s *object, jobject javaObject);
extern ObjectEntry *swarm_directory_java_add_object (jobject lref, Object_s *object);
extern ObjectEntry *swarm_directory_java_add_class (jobject lref, Class class);
extern SelectorEntry *swarm_directory_java_add_selector (jobject lref, SEL sel);
extern Class swarm_directory_java_ensure_class (jclass javaClass);
extern Class swarm_directory_java_find_class_named_objc (const char *className);
extern Class swarm_directory_java_class_for_object_objc (jobject jobj);
extern jobject swarm_directory_objc_ensure_java (id object);
extern jobject swarm_directory_objc_find_object_java (id object);
extern jobject swarm_directory_objc_find_selector_java (SEL sel);
extern jobject swarm_directory_objc_ensure_selector_java (jclass jClass, SEL sel);
extern jclass swarm_directory_objc_find_class_java (Class class);
extern SEL swarm_directory_java_ensure_selector (jobject jsel);

extern const char *java_copy_string (jstring javaString);
extern void java_cleanup_strings (const char **stringArray, size_t count);
extern const char **java_convert_string_array (jobjectArray javaString);

extern const char *java_class_name (jobject obj);
extern BOOL java_field_usable_p (jobject field);
extern BOOL java_method_usable_p (jobject method);
extern BOOL java_objc_proxy_p (jclass class);
extern void java_drop (jobject jobj);

#define SD_JAVA_FIND_OBJECT_SWARM_OBJC(jobj)  swarm_directory_java_find_swarm_objc (jobj)

#define SD_JAVA_FIND_OBJECT_OBJC(jobj)  swarm_directory_java_find_objc (jobj)
#define SD_JAVA_ENSURE_OBJECT_OBJC(jobj) swarm_directory_java_ensure_objc (jobj)
#define SD_JAVA_ENSURE_OBJECT_JAVA(objc) swarm_directory_objc_ensure_java (objc)
#define SD_JAVA_FIND_OBJECT_JAVA(objc) swarm_directory_objc_find_object_java (objc)
#define SD_JAVA_FIND_SELECTOR_JAVA(sel) swarm_directory_objc_find_selector_java (sel)
#define SD_JAVA_ENSURE_SELECTOR_JAVA(jClass, sel) swarm_directory_objc_ensure_selector_java (jClass, sel)
#define SD_JAVA_FIND_CLASS_JAVA(objcClass) swarm_directory_objc_find_class_java (objcClass)
#define SD_JAVA_ADD_OBJECT(jObj, oObj) swarm_directory_java_add_object (jObj, oObj)
#define SD_JAVA_ADD_SELECTOR(jsel, sel) swarm_directory_java_add_selector (jsel, sel)
#define SD_JAVA_ADD_STRING(jObj, str) swarm_directory_java_add_object (jObj, (id) str)

#define SD_JAVA_ADD_OBJECT_JAVA(jObj, oObj) swarm_directory_java_add_object (jObj, oObj)->foreignObject.java
#define SD_JAVA_ADD_CLASS_JAVA(jClass, oClass) ((jclass) swarm_directory_java_add_class (jClass, oClass)->foreignObject.java)

#define SD_JAVA_SWITCHPHASE(jobj, objc) swarm_directory_java_switch_phase (objc, jobj)->foreignObject.java
#define SD_JAVA_SWITCHOBJC(jobj, newobjc) swarm_directory_java_switch_objc (newobjc, jobj)
#define SD_JAVA_NEXTPHASE(jobj) swarm_directory_java_next_phase (jobj)
#define SD_JAVA_ENSURE_SELECTOR_OBJC(jobj) swarm_directory_java_ensure_selector (jobj)
#define SD_JAVA_ENSURE_CLASS_OBJC(jclazz) swarm_directory_java_ensure_class (jclazz)
#define SD_JAVA_INSTANTIATE(clazz) java_instantiate_pair (clazz)

#define JAVA_COPY_STRING(javaString) java_copy_string (javaString)
#define JAVA_CLEANUP_STRINGS(stringArray) java_cleanup_strings ((const char **) stringArray, sizeof (stringArray) / sizeof (const char *))
#define JAVA_CONVERT_STRING_ARRAY(jary) java_convert_string_array (jary)
#define JAVA_CLEANUP_STRING_ARRAY(stringArray, name) { java_cleanup_strings ((const char **) stringArray, (*jniEnv)->GetArrayLength (jniEnv, name)); [scratchZone free: (void *) stringArray]; }

#define JAVA_OBJECT_ENTRY(theJavaObject,theObject) [[[[ObjectEntry createBegin: _obj_GCFixedRootZone] setJavaObject: theJavaObject] setObject: theObject] createEnd]
#define JAVA_SELECTOR_ENTRY(theJavaObject,theSel) [[[[SelectorEntry createBegin: _obj_GCFixedRootZone] setJavaObject: theJavaObject] setSelector: theSel] createEnd]
#define JAVA_OBJCENTRY(theObject) JAVA_ENTRY(theObject,0)
#define JAVA_JAVAENTRY(theJavaObject) JAVA_ENTRY(0,theJavaObject)
#define JAVA_FIND_OBJECT_ENTRY(theJavaObject) ({ ObjectEntry *_findEntry  = alloca (sizeof (ObjectEntry)); _findEntry->foreignObject.java = theJavaObject; _findEntry; })

#endif
