/* Implementation for Objective-C Tcl interpreter functions
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   With NeXT runtime compatibility incorporated by:
   Robert Stabl <stabl@informatik.uni-muenchen.de>
   Comp. Sci. Inst., U. of Munich, Leopoldstr. 11B D-80802 Muenchen

   This file is part of the Tcl/Objective-C interface library.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.
   
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/ 

/*****************************************************************
  in Tcl type:
    set tclObjcDebug 1
  to see debugging information at each message send.
*******************************************************************/

/* Choose between:
   1. Each time an object is returned, it is defined as a tcl command.
   2. Messages to Objective-C objects are caught in 'unknown'.
   I think 2. is better. */
#define OBJECTS_AS_TCL_COMMANDS 0

#include <swarmconfig.h>

#ifdef BUGGY_BUILTIN_APPLY
#define USE_FFI
#endif

#ifdef USE_FFI
#ifdef USE_AVCALL
#include <avcall.h>
#else
#undef PACKAGE
#undef VERSION
#include <ffi.h>
#endif
#endif

#include "tclObjc.h"
#include <tcl.h>

#ifdef HAVE_OBJC_MALLOC
#include <objc/objc-api.h>
#else
#include "objc-malloc.h"
#endif
#include <stdlib.h>
#include <string.h>

#define ATDELIMCHAR '@'

static id getObjectReturn(void *);
static void *getPointerReturn(void *);
static int getIntegerReturn(void *);
static unsigned int getUIntegerReturn(void *);
static short getShortReturn(void *);
static unsigned short getUShortReturn(void *);
static long getLongReturn(void *);
static unsigned long getULongReturn(void *);
static char getCharReturn(void *);
static unsigned char getUCharReturn(void *);
static char *getStringReturn(void *);
static float getFloatReturn(void *);
static double getDoubleReturn(void *);

#include <objc/objc-api.h>
#include <objc/encoding.h>
#include "List.h"		/* for special case hack */
extern int method_get_sizeof_arguments (struct objc_method* mth);
#if (HAVE_LIBCOLL)
#include <coll/Array.h>		/* for special case hack */
#endif /* HAVE_LIBCOLL */

#define XSTR(s) STR(s)
#define STR(s) #s
const char coll_version[] = XSTR(TCLOBJC_VERSION);

int (*tclObjc_eventHook)();

Tcl_Interp *_TclObject_interp;

char *tclObjc_objectToName(id obj)
{
  /* Fix this messiness */
  static char name[512];
  if (obj)
    {
      sprintf(name, "%s%c0x%p", obj->class_pointer->name, ATDELIMCHAR, obj);
      return name;
    }
  return "nil";
}

extern char *strchr();

/* Return TCLOBJC_NO_OBJ if name is no good */
id tclObjc_nameToObject(const char *name)
{
  id object;
  unsigned long ul;
  const char *p = name;
  while (*p != ATDELIMCHAR && *p != '\0') p++;
  if ((*p) && (sscanf(p+3, "%lx", &ul) == 1))
    {
      return (id)ul;
    }
  else if ((!strcmp(name, "nil")) 
	   || (!strcmp(name, "Nil"))
	   || (!strcmp(name, "0x0")))
    {
      return nil;
    }
  else if ((object = (id)objc_lookup_class(name)))
    {
      return object;
    }
  return TCLOBJC_NO_OBJ;
}

int tclObjc_msgSendToClientData(ClientData clientData, Tcl_Interp *interp,
				int argc, char *argv[])
{
  char resultString[1024];
  char methodName[100];
  BOOL argvIsMethodArg[256];
  id self;
  SEL sel;
  Method_t method;
  int i;

  if (argc < 2)
    {
      interp->result = "no method specified.";
      return TCL_ERROR;
    }

  argvIsMethodArg[0] = NO;
  argvIsMethodArg[1] = NO;
  strcpy(methodName, argv[1]);
  for (i = 2; i < argc; i++)
    {
      if (argv[i][strlen(argv[i])-1] == ':')
	{
	  strcat(methodName, argv[i]);
	  argvIsMethodArg[i] = NO;
	}
      else
	{
	  argvIsMethodArg[i] = YES;
	}
    }

  self = (id)clientData;

#if (HAVE_LIBCOLL)
  /* special case hack for getting Arrays of id's to tcl.
     Send the message "contents" to an Array object.
   */
  if (!strcmp("contents", methodName) 
      && [self isKindOf:[Array class]]
      && !strcmp("@",[self contentEncoding]))
    {
      int i;

      Tcl_ResetResult(interp);
      for (i = 0; i < [self count]; i++)
	{
	  Tcl_AppendElement(interp, tclObjc_objectToName([self elementAtIndex:i].id_u));
	}
      return TCL_OK;
    }
#endif /* HAVE_LIBCOLL */

  /* special case hack for getting List contents to tcl.
     Send the message "contents" to a List object.
   */
  if (!strcmp("contents", methodName) 
      && [self isKindOf:[List class]])
    {
      int i;

      Tcl_ResetResult(interp);
      for (i = 0; i < [self count]; i++)
	{
	  Tcl_AppendElement(interp, tclObjc_objectToName([self objectAt:i]));
	}
      return TCL_OK;
    }

  /* special case hack for sending message to a TclObject */
  if (self->class_pointer == [TclObject class])
    {
      static Tcl_DString command;
      static char *cmd;
      int i;
      int code;

      Tcl_DStringInit(&command);
      Tcl_DStringAppend(&command, ((TclObject*)self)->_tclName, -1);
      Tcl_DStringAppend(&command, " ", -1);
      Tcl_DStringAppend(&command, methodName, -1);
      for (i = 2; i < argc; i++)
	{
	  if (argvIsMethodArg[i]) continue;
	  Tcl_DStringAppendElement(&command, argv[i]);
	  Tcl_DStringAppend(&command, " ", -1);
	}
      cmd = Tcl_DStringAppend(&command, "\n", -1);
      if (!(((TclObject*)self)->_interp))
	{
	  fprintf(stderr, "TclObject interp not yet set\n");
	  return TCL_ERROR;
	}
      code = Tcl_Eval(((TclObject*)self)->_interp, cmd);
      if (code != TCL_OK)
	{
	  char *msg;
	  msg = Tcl_GetVar(interp, "errorInfo", TCL_GLOBAL_ONLY);
	  if (msg == NULL) {
	    msg = interp->result;
	  }
	  fprintf(stderr, "(tclObjc: messaging a TclObject:) %s\n", msg);
	  fprintf(stderr, "while evaluating: %s\n", cmd);
	}
      Tcl_DStringFree(&command);
      return code;
    }

  sel = sel_get_uid(methodName);

  if (![self respondsTo:sel])
    {
      printf("%s does not respond to method %s\n", 
	     [self name], methodName);
      Tcl_SetResult(interp, "object does not respond to method", 
		    TCL_STATIC);    
      return TCL_ERROR;
    }

  if (object_is_instance(self))
    method = class_get_instance_method(self->class_pointer, sel);
  else
    method = class_get_class_method(self->class_pointer, sel);

  if (!method)
    {
      printf("class %s doesn't have method %s\n", 
	     self->class_pointer->name, methodName);
      Tcl_SetResult(interp, "method is NULL", TCL_STATIC); 
      return TCL_ERROR;
    }

  {

# define BUILDNEWARGFRAME
# ifdef BUILDNEWARGFRAME
    arglist_t argframe;
    int regArgSize;
    const char * typeForReg;
# else
    arglist_t argframe = __builtin_apply_args();
# endif
    char *datum;
#ifndef USE_FFI
    void *retframe = NULL ;
#else
    long long ret;
    void *retframe = &ret;
#endif

    int argsize = method_get_sizeof_arguments(method);
    char argptr_buffer[argsize];
    unsigned int bargframe = (unsigned int)argptr_buffer;
    int argnum;
    const char *type;
    int tmpint;
    int tmpuint;
    char *objcdebug;
    BOOL debug_printing;
#if OBJECTS_AS_TCL_COMMANDS
    Tcl_CmdInfo cmdInfo;
#endif

//
// We attempt to avoid the __builtin dynamic call entirely
// in a few common cases. For example, when all the arguments 
// and the return value is either an object or an int.
//

    int do_special_hack = 1 ;
    int hack_arg_count = 0 ;
    int hack_arguments[3] ;
    int returnValue = 0;

#ifdef BUILDNEWARGFRAME
    // create the pointer argframe to memory on the stack
    typeForReg = (const char *)
      strrchr(objc_skip_typespec(method->method_types), '+');
    if (typeForReg)
      regArgSize = atoi(++typeForReg) + sizeof(void *);
    else
      regArgSize = 0;
    argframe = (arglist_t) alloca(sizeof(char *) + regArgSize);
#endif
    
    argframe->arg_ptr = argptr_buffer;

    objcdebug = Tcl_GetVar(interp, "tclObjcDebug", TCL_GLOBAL_ONLY);
    if (objcdebug) 
      debug_printing = YES;
    else 
      debug_printing = NO;
    /* Perhaps later we could add different levels of debugging 
       depending on the contents of objcdebug */

    if(debug_printing)
      printf("The method: %s \n",methodName) ;

    if (debug_printing)
      printf("method %s, argc %d argsize %d (on stack), method types: '%s'\n", 
	     methodName, argc, argsize, method->method_types);
#ifdef BUILDNEWARGFRAME
    if (debug_printing)
      printf("Found %d bytes of register args.\n", regArgSize);
#endif
    
    datum = method_get_first_argument(method, argframe, &type);
    *(id*)datum = self;
    datum = method_get_next_argument(argframe, &type);
    *(SEL*)datum = sel;

    if (debug_printing) {
      printf("Filling in method arguments now. There should be %d.\n",
	     method_get_number_of_arguments(method));
    }

    switch( *(method->method_types) ){
      case _C_ID:
      case _C_PTR:
      case _C_INT:
      case _C_UINT:
      case _C_CHARPTR:
      case _C_VOID:
        if(debug_printing)
    printf("Return type is compatible with special_hack!!!\n") ;
        break ;
      case _C_SHT:
      case _C_USHT:
      case _C_LNG:
      case _C_ULNG:
      case _C_CHR:
      case _C_UCHR:
      case _C_FLT:
      case _C_DBL:
      default:
        do_special_hack = 0 ;
        if(debug_printing)
    printf("Abandoning special_hack option -> incompatible return type!\n") ;
        break ;
    }

    if(method_get_number_of_arguments(method) > 5){
      do_special_hack = 0 ;
      if(debug_printing)
        printf("Abandoning special_hack option -> too many arguments!\n") ;
    }

    for (argnum = 2,
	 datum = method_get_next_argument(argframe, &type);
	 datum;
	 datum = method_get_next_argument(argframe, &type),
	 ({argnum++; while (datum && !argvIsMethodArg[argnum]) argnum++;}))
      {

#define marg_getRef(margs, offset, type) ( (type *)offset )

	unsigned flags = objc_get_type_qualifiers(type);
	type = objc_skip_type_qualifiers(type);
	flags = flags;
	if (debug_printing)
	  {
	    printf("datum=%x type=%s\n", 
		   (unsigned int)datum - bargframe, type);
	    printf("argv[%d] = %s type=%s\n", argnum, argv[argnum], type);
	  }

	switch (*type)
	  {
	  case _C_ID:
	    *(marg_getRef(argframe, datum, id)) = 
	    	tclObjc_nameToObject(argv[argnum]);
	    if (*(marg_getRef(argframe, datum, id)) == TCLOBJC_NO_OBJ)
	      {
		sprintf(interp->result, 
			"Expected objc object, got %s instead.\n", 
			argv[argnum]);
		return TCL_ERROR;
	      }
            if(do_special_hack){
              hack_arguments[hack_arg_count] = (int)     	
                tclObjc_nameToObject(argv[argnum]) ;
              if(debug_printing)
                printf("Special Hack: Turned %s (ID) to %d.\n",
                        argv[argnum],hack_arguments[hack_arg_count]) ;  
              hack_arg_count++ ;
            }
	    break;
	  case _C_PTR:
	    sscanf(argv[argnum], "0x%x", 
			(marg_getRef(argframe, datum, unsigned int)));
            if(do_special_hack){
	      sscanf(argv[argnum], "0x%x", hack_arguments + hack_arg_count) ;
              if(debug_printing)
                printf("Special Hack: Turned %s (PTR) to %d.\n",
                        argv[argnum],hack_arguments[hack_arg_count]) ;  
              hack_arg_count++ ;
            }
	    break;
	  case _C_INT:
	    sscanf(argv[argnum], "%d", 
			(marg_getRef(argframe, datum, int)));
            if(do_special_hack){
  	      sscanf(argv[argnum], "%d", hack_arguments + hack_arg_count) ;
              if(debug_printing)
                printf("Special Hack: Turned %s (INT) to %d.\n",
                        argv[argnum],hack_arguments[hack_arg_count]) ;  
              hack_arg_count++ ;
            }
	    break;
	  case _C_UINT:
	    sscanf(argv[argnum], "%u",  
			(marg_getRef(argframe, datum, unsigned int)));
            if(do_special_hack){
	      sscanf(argv[argnum], "%u", hack_arguments + hack_arg_count) ;
              if(debug_printing)
                printf("Special Hack: Turned %s (UINT) to %d.\n",
                        argv[argnum],hack_arguments[hack_arg_count]) ;  
              hack_arg_count++ ;
            }
	    break;
	  case _C_LNG:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> unsigned short typed argument!\n") ;
	    sscanf(argv[argnum], "%ld", 
			(marg_getRef(argframe, datum, long)));
	    break;
	  case _C_ULNG:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> unsigned short typed argument!\n") ;
	    sscanf(argv[argnum], "%lu",
			(marg_getRef(argframe, datum, unsigned long)));
	    break;
	  case _C_SHT:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> short typed argument!\n") ;
	    sscanf(argv[argnum], "%d", &tmpint);
	    *(marg_getRef(argframe, datum, short)) = (short)tmpint;
	    break;
	  case _C_USHT:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> unsigned short typed argument!\n") ;
	    sscanf(argv[argnum], "%u", &tmpuint);
	    *(marg_getRef(argframe, datum, unsigned short)) = 
	    	(unsigned short)tmpuint;
	    break;
	  case _C_CHR:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> char typed argument!\n") ;
	    sscanf(argv[argnum], "%c",
	    		(marg_getRef(argframe, datum, char)));
	    break;
	  case _C_UCHR:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> unsigned char argument!\n") ;
	    sscanf(argv[argnum], "%d", &tmpuint);
	    *(marg_getRef(argframe, datum, unsigned char)) = 
	    	(unsigned char)tmpuint;
	    break;
	  case _C_CHARPTR:
	    *(marg_getRef(argframe, datum, char*)) = argv[argnum];
            if(do_special_hack){
  	      hack_arguments[hack_arg_count] = (int) argv[argnum] ;
              if(debug_printing)
                printf("Special Hack: Turned %s (CHARPTR) to %d.\n",
                        argv[argnum],hack_arguments[hack_arg_count]) ;  
              hack_arg_count++ ;	
            }    
	    break;
	  case _C_FLT:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> float typed argument!\n") ;
	    sscanf(argv[argnum], "%f",
	    		(marg_getRef(argframe, datum, float)));
	    break;
	  case  _C_DBL:
            do_special_hack = 0 ;
            if(debug_printing)
  printf("Abandoning special_hack option -> double typed argument!\n") ;
	    sscanf(argv[argnum], "%lf",
	    		(marg_getRef(argframe, datum, double)));
	    break;
	  default:
	    {
	      fprintf(stderr, "Tcl can't handle arg type %s", type);
	      sprintf(resultString, "Tcl can't handle arg type %s", type);
	      Tcl_SetResult(interp, resultString, TCL_VOLATILE);
	      return TCL_ERROR;
	    }
	  }
      }

    if(do_special_hack){
      typedef int (*IntImp)(id, SEL, ...) ;
      IntImp theCall ;

      theCall = (IntImp) [self methodFor: sel] ;

      switch(hack_arg_count) {
        case 0:           
          returnValue = theCall(self,sel) ;
          break ;
        case 1:
          returnValue = theCall(self,sel,hack_arguments[0]) ;
          break ;
        case 2:
          returnValue = theCall(self,sel,hack_arguments[0],hack_arguments[1]) ;
          break ;
        case 3:
          returnValue = theCall(self,sel,hack_arguments[0],
                                         hack_arguments[1],
                                         hack_arguments[2]) ;
          break ;
        default:
  fprintf(stderr,"Something went awfully wrong in the tclObjc special_hack\n") ;
          exit(-1) ;
          break ;
      }
    } else {
      IMP imp = method->method_imp;
#ifndef USE_FFI
      retframe = __builtin_apply ((apply_t)imp, (void*)argframe, argsize);
#else
#ifndef USE_AVCALL
      typedef struct alist *av_alist;
      struct alist alist_buf;
      av_alist alist = &alist_buf;   
      int acnt = method_get_number_of_arguments (method);
      ffi_type *types_buf[acnt];
      void *values_buf[acnt];
      ffi_cif cif;
      ffi_type *fret; 
      struct alist
        {
          ffi_type **type_pos;
          void **value_pos;
        };

      void push_argument (const char *typespec, void *obj)
        {
          char type = *typespec;

          switch (type)
            {
            case _C_ID:
              *alist->type_pos = &ffi_type_pointer;
              break;
              
            case _C_SEL:
              *alist->type_pos = &ffi_type_pointer;
              break;
              
            case _C_UCHR:
              *alist->type_pos = &ffi_type_uchar;
              break;
              
            case _C_INT:
              *alist->type_pos = &ffi_type_sint;
              break;
              
            case _C_FLT:
              *alist->type_pos = &ffi_type_float;
              break;
              
            case _C_DBL:
              *alist->type_pos = &ffi_type_double;
              break;

            case _C_LNG:
              *alist->type_pos = &ffi_type_slong;
              break;
              
            case _C_CHARPTR:
              *alist->type_pos = &ffi_type_pointer;
              break;
              
            default:
              abort ();
            }
          alist->type_pos++;
          *alist->value_pos = obj;
          alist->value_pos++; 
        }
      alist->type_pos = types_buf; 
      alist->value_pos = values_buf; 

      switch (*(method->method_types))
        {
        case _C_ID:
          fret = &ffi_type_pointer;
          break;
        case _C_SEL:
          fret = &ffi_type_pointer;
          break;
        case _C_UCHR:
          fret = &ffi_type_uchar;
          break;
        case _C_INT:
          fret = &ffi_type_sint;
          break;
        case _C_FLT:
          fret = &ffi_type_float;
          break;
        case _C_DBL:
          fret = &ffi_type_double;
          break;
        case _C_CHARPTR:
          fret = &ffi_type_pointer;
          break;
        default:
          abort ();
        }
      if (ffi_prep_cif (&cif, FFI_DEFAULT_ABI, acnt, fret, types_buf) != FFI_OK)
        abort ();  

#else
      av_alist alist;

      void push_argument (const char *typespec, void *obj)
        {
          char type = *typespec;

          switch (type)
            {
            case _C_ID:
              av_ptr (alist, id, *(id *)obj);
              break;

            case _C_SEL:
              av_ptr (alist, SEL, (SEL *)obj);
              break;

            case _C_UCHR:
              av_uchar (alist, *(unsigned char *)obj);
              break;
              
            case _C_INT:
              av_int (alist, *(int *)obj);
              break;
              
            case _C_FLT:
              av_float (alist, *(float *)obj);
              break;

            case _C_DBL:
              av_double (alist, *(double *)obj);
              break;
              
            case _C_LNG:
              av_long (alist, *(long *)obj);
              break;
              
            case _C_CHARPTR:
              av_ptr (alist, const char *, *(const char **)obj);
              break;
              
            default:
              abort ();
            }
        }
      switch (*(method->method_types))
        {
        case _C_ID:
          av_start_ptr (alist, imp, id, retframe);
          break;
        case _C_SEL:
          av_start_ptr (alist, imp, SEL, retframe);
          break;
        case _C_UCHR:
          av_start_uchar (alist, imp, retframe);
          break;
        case _C_INT:
          av_start_int (alist, imp, retframe);
          break;
        case _C_FLT:
          av_start_float (alist, imp, retframe);
          break;
        case _C_DBL:
          av_start_double (alist, imp, retframe);
          break;
        case _C_CHARPTR:
          av_start_ptr (alist, imp, const char *, retframe);
          break;
        default:
          abort ();
        }              
#endif
      for (argnum = 0,
	 datum = method_get_first_argument (method, argframe, &type);
	 datum;
	 datum = method_get_next_argument (argframe, &type),
	 ({argnum++; while (datum && !argvIsMethodArg[argnum]) argnum++;}))
      {
        push_argument (type, datum);
      }

#ifndef USE_AVCALL
      ffi_call (&cif, imp, retframe, values_buf);  
#else
      av_call (alist);
#endif
#endif
    }

    if (debug_printing)
      {
        if(do_special_hack)
          printf("Special_Hack: return value unsigned int 0x%x\n",returnValue) ;
        else
  	  printf("__Builtin_Apply: retframe unsigned int 0x%x\n", 
	         getIntegerReturn(retframe));
      }
    type = method->method_types;
    switch (*type)
      {
      case _C_ID:
	{
	  id returnedObject;
	  char * s;

          if(do_special_hack)
	    returnedObject = (id) returnValue ;
          else 
	    returnedObject = getObjectReturn(retframe);
         
          s = tclObjc_objectToName(returnedObject);
	  sprintf(resultString, s);
	}
#if OBJECTS_AS_TCL_COMMANDS
	if (!Tcl_GetCommandInfo(interp, resultString, &cmdInfo))
	  Tcl_CreateCommand(interp, resultString, tclObjc_msgSendToClientData,
			    *(id*)retframe, 0);
#else /* messages caught and forwarded by tcl proc "unknown" */
#endif
	break;
      case _C_PTR:
        if(do_special_hack)
  	  sprintf(resultString, "0x%x", returnValue);
        else
#ifdef POINTER_FMT_HEX_PREFIX
	  sprintf(resultString, "%p", getPointerReturn(retframe));
#else
	  sprintf(resultString, "0x%p", getPointerReturn(retframe));
#endif
	break;
      case _C_INT:
        if(do_special_hack)
  	  sprintf(resultString, "%d", returnValue);
        else
	  sprintf(resultString, "%d", getIntegerReturn(retframe));
	break;
      case _C_UINT:
        if(do_special_hack)
  	  sprintf(resultString, "%u", returnValue);
        else
	  sprintf(resultString, "%u", getUIntegerReturn(retframe));
	break;
      case _C_SHT:
	sprintf(resultString, "%d", getShortReturn(retframe));
	break;
      case _C_USHT:
	sprintf(resultString, "%u", getUShortReturn(retframe));
	break;
      case _C_LNG:
        sprintf(resultString, "%ld", getLongReturn(retframe));
	break;
      case _C_ULNG:
	sprintf(resultString, "%lx", getULongReturn(retframe));
	break;
      case _C_CHR:
	sprintf(resultString, "%d", getCharReturn(retframe));
	break;
      case _C_UCHR:
	sprintf(resultString, "%d", getUCharReturn(retframe));
	break;
      case _C_CHARPTR:
        /* Yuck.  Clean this up. */
        if(do_special_hack)
          Tcl_SetResult(interp, (char *) returnValue, TCL_VOLATILE);
        else
          Tcl_SetResult(interp, getStringReturn(retframe), TCL_VOLATILE);
        return TCL_OK;
      case _C_FLT:
	sprintf(resultString, "%g", getFloatReturn(retframe));
	break;
      case _C_DBL:
	sprintf(resultString, "%g", getDoubleReturn(retframe));
	break;
      case _C_VOID:
        resultString[0] = '\0';
        break;
      default:
	{
	  fprintf(stderr, "Tcl can't handle ret type %s", type);
	  sprintf(resultString, "Tcl can't handle ret type %s", type);
	  Tcl_SetResult(interp, resultString, TCL_VOLATILE);
	  return TCL_ERROR;
	}
      }

    Tcl_SetResult(interp, resultString, TCL_VOLATILE);

    if (*tclObjc_eventHook)
      (*tclObjc_eventHook)();
    return TCL_OK;
    
  }

}

//
// __builtin_return does not seem to work within nested procedures so 
// we must give up using the macro GET_RETVAL as defined in previous
// versions of libtclObjc
//

static id
getObjectReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(id *)p;
#endif
}

static void *
getPointerReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(void **)p;
#endif
}

static int
getIntegerReturn (void * p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(int *)p;
#endif
}

static unsigned
getUIntegerReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(unsigned *)p;
#endif
}

static short
getShortReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(short *)p;
#endif
}

static unsigned short
getUShortReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(unsigned short *)p;
#endif
}

static long
getLongReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(long *)p;
#endif
}

static unsigned long
getULongReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(unsigned long *)p;
#endif
}

static char
getCharReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(char *)p;
#endif
}

static unsigned char
getUCharReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return (p);
#else
  return *(unsigned char *)p;
#endif
}

static char *
getStringReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return(p);
#else
  return *(unsigned char **)p;
#endif
}

static float
getFloatReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return(p);
#else
  return *(float *)p;
#endif
}

static double
getDoubleReturn (void *p)
{
#ifndef USE_FFI
  __builtin_return(p);
#else
  return *(double *)p;
#endif
}
  
void tclObjc_registerObjectWithName(Tcl_Interp *interp, 
				    id object, const char *name)
{
  Tcl_CreateCommand(interp, (char *)name, tclObjc_msgSendToClientData,
		    object, 0);
}

void tclObjc_unregisterObjectNamed(Tcl_Interp *interp,
				   const char *name)
{
  Tcl_DeleteCommand(interp, (char *)name);
}

void tclObjc_registerClassnames(Tcl_Interp *interp)
{
  id class; 
  void *es = NULL;
  while ((class = objc_next_class(&es)))
    tclObjc_registerObjectWithName(interp, class, [class name]);
#if 0
  node_ptr node = NULL;

  /* register all class names with tcl */
  while ((node = hash_next(__objc_class_hash, node)))
    {
      //      printf("registering %s\n", (char *)node->key);
      tclObjc_registerObjectWithName(interp, node->value, node->key);
    }
#endif
}


int tclObjc_msgSendToArgv1(ClientData clientData, Tcl_Interp *interp,
			   int argc, char *argv[])
{
  id obj;

  if ((obj = tclObjc_nameToObject(argv[1])) != TCLOBJC_NO_OBJ)
    {
      return tclObjc_msgSendToClientData((ClientData)obj, interp, 
					argc-1, &(argv[1]));
    }
  else
    {
      sprintf(interp->result, 
	      "tclObjc: %s not recognized as an object", argv[1]);
      return TCL_ERROR;
    }
}


@implementation TclObject

+ newName: (char *)objectName
{
  TclObject *newTclObject = class_create_instance(self);
  newTclObject->_tclName = (char*) objc_malloc((unsigned)(strlen(objectName)+1)
                                             * sizeof(char));
  strcpy(newTclObject->_tclName, objectName);
  /* Fix this ugliness!!! */
  newTclObject->_interp = _TclObject_interp;
  return newTclObject;
}

- free
{
  objc_free(_tclName);
  return object_dispose(self);
}  

- (BOOL) respondsTo: (SEL)aSel
{
  Tcl_CmdInfo cmdInfo;
  char selString[128];
  sprintf(selString, "%s%s", _tclName, sel_get_name(aSel));
  return (((object_is_instance(self)
           ?class_get_instance_method(self->ISA, aSel)
	    :class_get_class_method(self->ISA, aSel))!=METHOD_NULL)
	  || Tcl_GetCommandInfo(_interp, selString, &cmdInfo));
}

- forward: (SEL)aSel : (arglist_t)argframe
{
  return [self performv: aSel :argframe];
}


- performv:(SEL)aSel :(arglist_t)argframe
{
  char *datum;
  const char *type;
  char *objcdebug;
  BOOL debug_printing;
  Method_t method = 0;
  char argString[256];
  Tcl_DString command;
  char *cmd;
  int tmpint;
  unsigned int tmpuint;

  if (_interp == NULL)
    {
      fprintf(stderr, "interp not set yet, %s\n", sel_get_name(aSel));
      return self;
    }

  objcdebug = Tcl_GetVar(_interp, "objcdebug", TCL_GLOBAL_ONLY);
  if (objcdebug) 
    debug_printing = YES;
  else 
    debug_printing = NO;

  Tcl_DStringInit(&command);
  Tcl_DStringAppend(&command, _tclName, -1);
  //  Tcl_DStringAppend(&command, " ", -1);
  Tcl_DStringAppend(&command, (char *)sel_get_name(aSel), -1);
  Tcl_DStringAppend(&command, " ", -1);

  if (debug_printing)
    printf("selector: %s\n", sel_get_name(aSel));

  /* search all classes for the method */
  {
    id class;
    void *es = NULL;

    while ((class = objc_next_class(&es))
	   && (!(method = class_get_instance_method(class, aSel)))
	   && (!(method = class_get_class_method(class, aSel))))
      ;
  }
#if 0
  {
    node_ptr node = NULL;
    while ((node = hash_next(__objc_class_hash, node))
           && (!(method = class_get_instance_method(node->value, aSel)))
           && (!(method = class_get_class_method(node->value, aSel))))
      ;
  }
#endif
  if (!method)
    {
      fprintf(stderr, "method not found, %s\n", sel_get_name(aSel));
      return self;
    }

  /* self */
  datum = method_get_first_argument(method, argframe, &type);
  /* SEL */
  datum = method_get_next_argument(argframe, &type);
  for (datum = method_get_next_argument(argframe, &type);
       datum;
       datum = method_get_next_argument(argframe, &type))
    {
      unsigned flags = objc_get_type_qualifiers(type);
      type = objc_skip_type_qualifiers(type);
      flags = flags;

      switch (*type)
	{
	case _C_PTR:
	  sprintf(argString, "0x%x", 
	  	*(unsigned int*)(marg_getRef(argframe, datum, unsigned int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_ID:
	  strcpy(argString, tclObjc_objectToName(
	  	*(id*)(marg_getRef(argframe, datum, id))));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_INT:
	  sprintf(argString, "%d", 
	  	*(int*)(marg_getRef(argframe, datum, int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_UINT:
	  sprintf(argString, "%u", 
	  	*(unsigned int*)(marg_getRef(argframe, datum, unsigned int)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_SHT:
	  tmpint = 
	  	*(short*)(marg_getRef(argframe, datum, short));
	  sprintf(argString, "%d", tmpint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_USHT:
	  tmpuint = 
	  	*(unsigned short*)(marg_getRef(argframe, datum, unsigned short));
	  sprintf(argString, "%u", tmpuint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_CHR:
	  sprintf(argString, "%c", 
	  	*(char*)(marg_getRef(argframe, datum, char)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_UCHR:
	  tmpuint = 
	  	*(unsigned char*)(marg_getRef(argframe, datum, unsigned char));
	  sprintf(argString, "%u", tmpuint);
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case _C_CHARPTR:
	  Tcl_DStringAppendElement(&command, 
	  	*(char**)(marg_getRef(argframe, datum, char *)));
	  break;
	case _C_FLT:
	  sprintf(argString, "%f", 
	  	*(float*)(marg_getRef(argframe, datum, float)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	case  _C_DBL:
	  sprintf(argString, "%f", 
	  	*(double*)(marg_getRef(argframe, datum, double)));
	  Tcl_DStringAppendElement(&command, argString);
	  break;
	default:
	  {
	    fprintf(stderr, "TclObject can't handle arg type %s", type);
	    return self;
	  }
	}
    }
  cmd = Tcl_DStringAppend(&command, "\n", -1);
  Tcl_GlobalEval(_interp, cmd);
  // I should interpret returned string and return it!;
  return self;
}


@end


/*****************************************************************/

static char tclObjcInitCmd[] =
"if {[llength [info procs unknown]]} { \n\
   rename unknown unknown_pre_tclObjc \n\
 } \n\
 proc unknown {name args} {\n\
   if {[string match *%c0x* $name]} {\n\
     return [uplevel tclObjc_msg_send $name $args]\n\
   } else {\n\
     if {[llength [info procs unknown_pre_tclObjc]]} {\n\
       unknown_pre_tclObjc $name $args\n\
     } else {\n\
       error \"in unknown: invalid command name: $name\"\n\
     }\n\
   }\n\
 }\n";


int TclObjc_Init(Tcl_Interp *interp)
{
#if ! OBJECTS_AS_TCL_COMMANDS
  int code;
#endif

  /* Fix this ugliness!!! */
  _TclObject_interp = interp;
  tclObjc_registerClassnames(interp);
  Tcl_CreateCommand(interp, "tclObjc_msg_send", 
		    tclObjc_msgSendToArgv1, 0, 0);
#if ! OBJECTS_AS_TCL_COMMANDS
  {
    char buf [strlen (tclObjcInitCmd) + 1];

    sprintf (buf, tclObjcInitCmd, ATDELIMCHAR);
    code = Tcl_Eval(interp, buf);
    if (code != TCL_OK)
      {
        fprintf(stderr, "tclObjc: Error during TclObjc_Init:\n");
        fprintf(stderr, interp->result);
      }
  }
#endif
  return TCL_OK;
}

