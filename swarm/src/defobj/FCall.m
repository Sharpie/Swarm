// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         FCall.m
Description:  foreign function call
Library:      defobj
*/

#import "FCall.h"
#import <objc/objc-api.h>
#include <stdlib.h>


void 
init_java (void)
{
  javaEnv = [JavaEnv create: globalZone];
  jniEnv = [javaEnv getJNIEnv]; 
  java_static_call_functions[swarm_type_void] = 
      FFI_FN ((*jniEnv)->CallStaticVoidMethod);
  java_static_call_functions[swarm_type_uchar] = 
      FFI_FN ((*jniEnv)->CallStaticCharMethod);
  java_static_call_functions[swarm_type_schar] = 
      FFI_FN ((*jniEnv)->CallStaticCharMethod);
  java_static_call_functions[swarm_type_ushort] = 
      FFI_FN ((*jniEnv)->CallStaticShortMethod);
  java_static_call_functions[swarm_type_sshort] = 
      FFI_FN ((*jniEnv)->CallStaticShortMethod);
  java_static_call_functions[swarm_type_uint] = 
      FFI_FN ((*jniEnv)->CallStaticIntMethod);
  java_static_call_functions[swarm_type_sint] =
      FFI_FN ((*jniEnv)->CallStaticIntMethod);
  java_static_call_functions[swarm_type_ulong] =
      FFI_FN ((*jniEnv)->CallStaticLongMethod);
  java_static_call_functions[swarm_type_slong] = 
      FFI_FN ((*jniEnv)->CallStaticLongMethod);
  java_static_call_functions[swarm_type_float] =
      FFI_FN ((*jniEnv)->CallStaticFloatMethod);
  java_static_call_functions[swarm_type_double] = 
      FFI_FN ((*jniEnv)->CallStaticDoubleMethod);
  java_static_call_functions[swarm_type_pointer] = NULL;
  java_static_call_functions[swarm_type_string] = 
      FFI_FN ((*jniEnv)->CallStaticObjectMethod);
  java_static_call_functions[swarm_type_jobject] = 
      FFI_FN ((*jniEnv)->CallStaticObjectMethod);

  java_call_functions[swarm_type_void] = 
      FFI_FN ((*jniEnv)->CallVoidMethod);
  java_call_functions[swarm_type_uchar] = 
      FFI_FN ((*jniEnv)->CallCharMethod);
  java_call_functions[swarm_type_schar] = 
      FFI_FN ((*jniEnv)->CallCharMethod);
  java_call_functions[swarm_type_ushort] = 
      FFI_FN ((*jniEnv)->CallShortMethod);
  java_call_functions[swarm_type_sshort] = 
      FFI_FN ((*jniEnv)->CallShortMethod);
  java_call_functions[swarm_type_uint] = 
      FFI_FN ((*jniEnv)->CallIntMethod);
  java_call_functions[swarm_type_sint] =
      FFI_FN ((*jniEnv)->CallIntMethod);
  java_call_functions[swarm_type_ulong] =
      FFI_FN ((*jniEnv)->CallLongMethod);
  java_call_functions[swarm_type_slong] = 
      FFI_FN ((*jniEnv)->CallLongMethod);
  java_call_functions[swarm_type_float] =
      FFI_FN ((*jniEnv)->CallFloatMethod);
  java_call_functions[swarm_type_double] = 
      FFI_FN ((*jniEnv)->CallDoubleMethod);
  java_call_functions[swarm_type_pointer] = NULL;
  java_call_functions[swarm_type_string] = 
      FFI_FN ((*jniEnv)->CallObjectMethod);
  java_call_functions[swarm_type_jobject] = 
      FFI_FN ((*jniEnv)->CallObjectMethod);
}

@implementation FCall

+ createBegin: aZone
{
  FCall *newCall;
  newCall = [aZone allocIVars: self];
  newCall->argNo = 0;
  newCall->assignedArguments = 0;
  newCall->hiddenArguments = 0;
  newCall->argTypes = NULL;
  newCall->argValues = NULL;
  newCall->returnType = swarm_type_void;
  newCall->result = NULL;
  newCall->signatureLength = 0;
  return newCall;
}

- setCallType: (unsigned int) cType
{
  callType = cType;
  if (callType == ccall)
    {
      hiddenArguments = 0;
      return self;
    }
  if (callType == objccall)
    {
      hiddenArguments = 2;
      return self;
    }
  if (callType == javacall || callType == javastaticcall)
    {
      hiddenArguments = 3;
      return self;
    }
  return self;
}

- setFunction: (void (*)()) fn
{
  if (!callType == ccall)
    raiseEvent (SourceMessage, "Call type and foreign function mismatch!\n");
  function = fn;
  return self;
}

- setMethod: (SEL) mtd inObject: obj
{
  if (!callType == objccall)
    raiseEvent (SourceMessage, "Call type and foreign function mismatch!\n");
  hiddenArguments = 2;
  (id) object = obj;
  (SEL) method = mtd;
  (Class) class = getClass(object);
  function = FFI_FN (get_imp (*(Class *) class, *(SEL *) method));
  return self;
}

- setJavaMethod: (const char *) methodName inClass: (const char *) className
       inObject: (jobject) obj
{
  if (!callType == javastaticcall && !callType == javacall)
    raiseEvent (SourceMessage, "Call type and foreign function mismatch!\n");
  if (!javaEnv)
    javaEnv = [JavaEnv create: globalZone];
  (jclass) class = (*jniEnv)->FindClass (jniEnv, className);
  if (!class)
    raiseEvent(SourceMessage, "Could not find Java class!\n");
  method = (char *) methodName;
  (jobject) object = obj;
  return self;
}    
   
- setJavaMethod: (const char *) methodName inClass: (const char *) className
{
  [self setJavaMethod: methodName inClass: className inObject: NULL];
  return self;
}

- setNumberOfArguments: (int) number
{
  argNo = number;
  if (argNo + hiddenArguments)
    {
      argTypes = [[self getZone] allocBlock: (sizeof (ffi_type *) * 
					      (argNo + hiddenArguments))];
      argValues = [[self getZone] allocBlock: (sizeof (void *) * 
					       (argNo + hiddenArguments))];
    }
  if (callType == objccall)
    {
      argTypes[0] = &ffi_type_pointer;
      argValues[0] = &object;
      argTypes[1] = &ffi_type_pointer;
      argValues[1] = &method;
      return self;
    }
  if (callType == javacall)
    {
      argTypes[0] = &ffi_type_pointer;
      argValues[0] = &jniEnv;
      argTypes[1] = &ffi_type_pointer;
      argValues[1] = &object;
      argTypes[2] = &ffi_type_pointer;
      argValues[2] = &method;
      return self;
    }
  if (callType == javastaticcall)
     {
      argTypes[0] = &ffi_type_pointer;
      argValues[0] = &jniEnv;
      argTypes[1] = &ffi_type_pointer;
      argValues[1] = &class;
      argTypes[2] = &ffi_type_pointer;
      argValues[2] = &method;
      return self;
    } 
  return self;
}

- addArgument: (void *) value ofType: (unsigned int) type
{
  if (assignedArguments == argNo)
    raiseEvent (SourceMessage, "Types already assigned to all arguments in the call!\n");
  if (callType == javacall || callType == javastaticcall) 
    {
      if (type <= swarm_type_double && type != swarm_type_float)
	{
	  argTypes[hiddenArguments + assignedArguments] = (void *) type;
	  argValues[hiddenArguments + assignedArguments] = 
	      [[self getZone] allocBlock: swarm_types[type]->size];
	  memcpy (argValues[hiddenArguments + assignedArguments], 
		  value, swarm_types[(int) argTypes[hiddenArguments + 
						   assignedArguments]]->size);
	  assignedArguments++;
	  signatureLength++;
	}
      else
	{
	  if (type == swarm_type_float)
	      [self addFloat: *(float *) value];
	  else
	      if (type == swarm_type_string)
		[self addString: *(char **) value];
	      else
		if (type == swarm_type_jobject)
		  [self addJObject: *(jobject *) value];
		else
		  raiseEvent (SourceMessage, "Passing pointers or structures to Java is not possible!\n");
	}
    }
  return self;
}

#define ADD_COMMON_TEST if (assignedArguments == argNo) raiseEvent (SourceMessage, "Types already assigned to all arguments in the call!\n"); if (!value) raiseEvent (SourceMessage, "NULL pointer passed as a pointer to argument!\n");



#define ADD_COMMON(type)  { ADD_COMMON_TEST if (callType == javacall || callType == javastaticcall) signatureLength++; argValues[hiddenArguments + assignedArguments] = [[self getZone] allocBlock: swarm_types[(type)]->size]; }



- addChar: (char) value
{
  ADD_COMMON (swarm_type_schar);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_schar; 
  *(char *) argValues[hiddenArguments + assignedArguments++] = value;
  return self;
}

- addShort: (short) value 
{
  ADD_COMMON (swarm_type_sshort);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_sshort;
  *(short *) argValues[hiddenArguments + assignedArguments++] = value; 
  return self;
}

- addInt: (int) value
{
  ADD_COMMON (swarm_type_sint);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_sint;
  *(int *) argValues[hiddenArguments + assignedArguments++] = value; 
  return self;
}

- addLong: (long) value
{
  ADD_COMMON (swarm_type_slong);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_slong;
  *(long *) argValues[hiddenArguments + assignedArguments++] = value; 
  return self;
}

- addFloat: (float) value
{
  /* in case the function to be called is compiled with compiler other
     than gcc, that does automatic casting of floats to doubles */
  ADD_COMMON (swarm_type_double); 
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_float;
  *(double *) argValues[hiddenArguments + assignedArguments++] = value; 
  return self;
}

- addDouble: (double) value
{
  ADD_COMMON (swarm_type_double);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_double;
  *(double *) argValues[hiddenArguments + assignedArguments++] = value; 
  return self;
}

- addString: (char *) value
{
  ADD_COMMON (swarm_type_string);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_string;
  if (callType == javacall || callType == javastaticcall) 
    {
      jstring jstr;
      jstr = (*jniEnv)->NewStringUTF(jniEnv, value);
      if (jstr == NULL)
      	  raiseEvent (SourceMessage, "Out of memory!\n");
      signatureLength += 18; // strlen("Ljava/lang/String;") 
      *(jstring *) argValues[hiddenArguments + assignedArguments] = jstr;
      assignedArguments++;
    }
  else
      *(char **) argValues[hiddenArguments + assignedArguments++] = value;
  return self;
}

- addJObject: (jobject) value
{
  ADD_COMMON (swarm_type_jobject);
  argTypes[hiddenArguments + assignedArguments] = (void *) swarm_type_jobject;
  if (callType == javacall || callType == javastaticcall) 
    {
      signatureLength += 18; // strlen("Ljava/lang/Object;") 
      *(jobject *) argValues[hiddenArguments + assignedArguments] = value; 
      assignedArguments++;
    }
  else
    *(jobject *) argValues[hiddenArguments + assignedArguments++] = value;
  return self;
}

- setReturnType: (unsigned int) type
{
  if (type > number_of_types)
      raiseEvent(SourceMessage, "Unkown return type for foerign function call!\n"); 

  if (callType == javacall || callType == javastaticcall)
    {
       if (type >= swarm_type_void && type <=swarm_type_double)
	  signatureLength++;
       else
         {
	    if (type == swarm_type_string)
	      [self setStringReturnType];
	    else
		if (type == swarm_type_jobject)
		    [self setJObjectReturnType];
		else
		    raiseEvent (SourceMessage, "Java methods can not return pointers or structures - specify strings and arrays directly!\n");
	 }
    }
  result = (void *) [[self getZone] allocBlock: swarm_types[type]->size];  
  returnType = (void *) type;
  return self;
}

- setStringReturnType
{
  returnType = (void *) swarm_type_string;
  result = (void *) [[self getZone] allocBlock: 
					swarm_types[(int) returnType]->size];
  signatureLength += 18; // == strlen("Ljava.lang.String;")
  return self;
}

- setJObjectReturnType
{
  returnType = (void *) swarm_type_jobject;
  result = (void *) [[self getZone] allocBlock: 
					swarm_types[(int) returnType]->size];
  signatureLength += 18; // == strlen("Ljava.lang.Object;")
  return self;
}
 
char * createSignature (FCall * self)
{
  char *str;
  unsigned int i;
  unsigned int offset = 0;

  str = [[self getZone] allocBlock: self->signatureLength + 3];
  str[offset++] = '(';
  for (i = self->hiddenArguments; 
       i < self->hiddenArguments + self->argNo; 
       i++)
    {
       strcpy (str + offset, 
	       java_type_signatures [*(int *)(self->argTypes + i)]);
       offset += 
	   strlen (java_type_signatures [*(int *) (self->argTypes + i)]);
       *(self->argTypes + i) = 
	   (void *) swarm_types [*(int *) (self->argTypes + i)];
    }

  str[offset++] = ')';

  strcpy (str + offset, java_type_signatures [(int) self->returnType]);
  offset += strlen (java_type_signatures [(int) self->returnType]);
  self->returnType = (void *) swarm_types [(int) self->returnType];
  
  str[offset++]='\0';
  return str;
}

void switch_to_ffi_types(FCall * self)
{
  unsigned int i;
  for (i = self->hiddenArguments; 
       i < self->hiddenArguments + self->argNo; 
       i++)
      *(self->argTypes + i) = 
	  (void *) swarm_types [*(int *) (self->argTypes + i)];
  self->returnType = (void *) swarm_types [(int) self->returnType];
}

- createEnd
{
  unsigned int res;
  if (_obj_debug && (callType == ccall || callType == objccall) && !function)
    raiseEvent (SourceMessage, "Function to be called not set!\n");
  if (_obj_debug && assignedArguments != argNo)
    raiseEvent (SourceMessage, "Arguments not initialized!\n");
  if (callType == javacall || callType == javastaticcall)
      {
	  char * mtdName;
	  char * signature;
	  mtdName = (char *) method;
	  function = (callType == javacall ? 
		      java_call_functions[(int) returnType] :
		      java_static_call_functions[(int) returnType]);
	  signature = createSignature ((FCall *) self);
	  printf("|%s|\n", signature);
	  
	  (jmethodID) method = (callType == javacall ?
				(*jniEnv)->GetMethodID (jniEnv, class, mtdName,
 							signature) :
				(*jniEnv)->GetStaticMethodID(jniEnv, class, 
							     mtdName,
							     signature)); 
	  if (!method)
	      raiseEvent (SourceMessage, "Could not find Java method!\n");
	  
      }
  else 
      switch_to_ffi_types ((FCall *) self);
  res = ffi_prep_cif (&cif, FFI_DEFAULT_ABI, hiddenArguments + argNo, 
		      (ffi_type *) returnType, (ffi_type **) argTypes);
  if (_obj_debug && res != FFI_OK)
      raiseEvent (SourceMessage, "Failed while preparing foreign function call closure!\n"); 
  return self;
}

- (void) _performAction_: anActvity
{
  ffi_call(&cif, function, result, argValues);  
}

- (char *) getStringResult
{
    if ((callType == javastaticcall || callType == javacall) && 
	returnType == &ffi_type_pointer)
           return (void *) 
	       (*jniEnv)->GetStringUTFChars (jniEnv, *(jobject *) result, 
					   0); 
    return *(char **)result;
}

- (jobject) getJObjectResult
{
    return *(jobject *)result;
}

- (void *) getResult
{
  return result;
}

@end



