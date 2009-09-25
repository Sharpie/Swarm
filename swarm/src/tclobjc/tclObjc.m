/* Implementation for Objective-C Tcl interpreter functions
   Copyright (C) 1993,1994  R. Andrew McCallum

   Written by:  R. Andrew McCallum <mccallum@cs.rochester.edu>
   Dept. of Computer Science, U. of Rochester, Rochester, NY  14627

   Rewritten for Swarm FCall by Marcus G. Daniels <mgd@santafe.edu>. (C)1999

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
#include <swarmconfig.h>
#import <defobj.h> // FArguments, FCall
#import <defobj/DefObject.h> // mapalloc_t for defalloc
#import <defobj/defalloc.h> // getCZone

#include "tclObjc.h"
#include <tcl.h>
#include <misc.h>

#define ATDELIMCHAR '@'
  
#import <defobj/swarm-objc-api.h>
//#include <objc/encoding.h>

int (*tclObjc_eventHook) ();

Tcl_Interp *_TclObject_interp;

char *
tclObjc_objectToName(id obj)
{
  /* Fix this messiness */
  static char name[512];
  if (obj)
    {
      sprintf(name, "%s%c" PTRHEXFMT, swarm_class_getName(swarm_object_getClass(obj)),
	      ATDELIMCHAR, obj);
      return name;
    }
  return "nil";
}

/* Return TCLOBJC_NO_OBJ if name is no good */
id
tclObjc_nameToObject (const char *name)
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
  else if ((object = (id)swarm_objc_lookupClass(name)))
    {
      return object;
    }
  return TCLOBJC_NO_OBJ;
}

static id
getObjectReturn (void *p)
{
  return *(id *) p;
}

static void *
getPointerReturn (void *p)
{
  return *(void **) p;
}

static int
getIntegerReturn (void * p)
{
  return *(int *) p;
}

static unsigned
getUIntegerReturn (void *p)
{
  return *(unsigned *) p;
}

static short
getShortReturn (void *p)
{
  return *(short *) p;
}

static unsigned short
getUShortReturn (void *p)
{
  return *(unsigned short *) p;
}

static long
getLongReturn (void *p)
{
  return *(long *) p;
}

static unsigned long
getULongReturn (void *p)
{
  return *(unsigned long *) p;
}

static char
getCharReturn (void *p)
{
  return *(char *) p;
}

static unsigned char
getUCharReturn (void *p)
{
  return *(unsigned char *) p;
}

static char *
getStringReturn (void *p)
{
  return *(char **) p;
}

static float
getFloatReturn (void *p)
{
  return *(float *) p;
}

static double
getDoubleReturn (void *p)
{
  return *(double *) p;
}

int
tclObjc_msgSendToClientData(ClientData clientData, Tcl_Interp *interp,
			    int argc, char *argv[])
{
  char resultString[1024];
  char methodName[100];
  BOOL argvIsMethodArg[256];
  id target;
  SEL sel;
  unsigned i;

  if (argc < 2)
    {
      interp->result = "no method specified.";
      return TCL_ERROR;
    }

  argvIsMethodArg[0] = NO;
  argvIsMethodArg[1] = NO;
  strcpy (methodName, argv[1]);
  for (i = 2; i < (unsigned)argc; i++)
    {
      if (argv[i][strlen (argv[i]) - 1] == ':')
	{
	  strcat (methodName, argv[i]);
	  argvIsMethodArg[i] = NO;
	}
      else
        argvIsMethodArg[i] = YES;
    }

  target = (id) clientData;
  sel = sel_get_any_typed_uid (methodName);

  if (![target respondsTo: sel])
    {
      fprintf (stderr, "%s does not respond to method %s\n",
               [target name], methodName);
      Tcl_SetResult (interp, "object does not respond to method", TCL_STATIC); 
      return TCL_ERROR;
    }
  
  {
    const char *seltype = sel_get_type (sel), *type;
    id <FArguments> fa;
    id <FCall> fc = nil;
    void *ret = NULL;
    unsigned argnum;

    fa = [FArguments createBegin: getCZone (scratchZone)];
#if SWARM_OBJC_TODO
    [fa setObjCReturnType: *(objc_skip_type_qualifiers (seltype))];
#endif
    type = objc_skip_argspec (seltype);
    type = objc_skip_argspec (type);
    type = objc_skip_argspec (type);

    for (argnum = 2; *type; type = objc_skip_argspec (type), argnum++)
      {
        while (!argvIsMethodArg[argnum]) argnum++;
	{ 
          const char *unqualifiedtype = objc_skip_type_qualifiers (type);
          
          switch (*unqualifiedtype)
            {
            case _C_ID:
              {
                id obj = tclObjc_nameToObject (argv[argnum]);
                
                if (obj != TCLOBJC_NO_OBJ)
                  [fa addObject: obj];
                else
                  {
                    sprintf (interp->result, 
                             "Expected objc object, got %s instead.\n", 
                             argv[argnum]);
                    goto fail;
                  }
              }
              break;
            case _C_PTR:
              abort ();
              break;
            case _C_INT:
              {
                int value;
                
                sscanf (argv[argnum], "%d", &value);
                [fa addInt: value];
              }
              break;
            case _C_UINT:
              {
                unsigned value;
                
                sscanf (argv[argnum], "%u", &value);
                [fa addUnsigned: value];
              }
              break;
            case _C_LNG:
              {
                long value;
                
                sscanf (argv[argnum], "%ld", &value);
                [fa addLong: value];
              }
              break;
            case _C_ULNG:
              {
                unsigned long value;
                
                sscanf (argv[argnum], "%lu", &value);
                [fa addUnsignedLong: value];
              }
              break;
            case _C_SHT:
              {
                short value;
                
                sscanf (argv[argnum], "%hd", &value);
                [fa addShort: value];
              }
              break;
            case _C_USHT:
              {
                unsigned short value;
                
                sscanf (argv[argnum], "%hu", &value);
                [fa addUnsignedShort: value];
              }
              break;
            case _C_CHR:
              {
                char value;
                
                sscanf (argv[argnum], "%c", &value);
                [fa addChar: value];
              }
              break;
            case _C_UCHR:
              {
                unsigned value;
                
                sscanf (argv[argnum], "%u", &value);
                [fa addUnsignedChar: (unsigned char) value];
              }
              break;
            case _C_CHARPTR:
              [fa addString: argv[argnum]];
              break;
            case _C_FLT:
              {
                float value;
                
                sscanf (argv[argnum], "%f", &value);
                [fa addFloat: value];
              }
              break;
            case  _C_DBL:
              {
                double value;
                
                sscanf (argv[argnum], "%lf", &value);
                [fa addDouble: value];
              }
              break;
            default:
              {
                fprintf (stderr, "Tcl can't handle arg type `%s' in `%s'",
                         type, seltype);
                sprintf (resultString, "Tcl can't handle arg type %s", type);
                Tcl_SetResult (interp, resultString, TCL_VOLATILE);
                goto fail;
              }
            }
        }
      }
    fc = [[[[FCall createBegin: getCZone (scratchZone)]
             setArguments: [fa createEnd]]
            setMethodFromSelector: sel inObject: target]
           createEnd];
    
    [fc performCall];
    
    ret = [fc getResult];
    type = objc_skip_type_qualifiers (seltype);
    switch (*type)
      {
      case _C_ID:
        {
          id returnedObject;
          char *s;

          returnedObject = getObjectReturn (ret);
          
          s = tclObjc_objectToName (returnedObject);
          strcpy (resultString, s);
        }
        break;
      case _C_PTR:
        sprintf (resultString, PTRHEXFMT, getPointerReturn (ret));
        break;
      case _C_INT:
        sprintf (resultString, "%d", getIntegerReturn (ret));
        break;
      case _C_UINT:
        sprintf (resultString, "%u", getUIntegerReturn (ret));
        break;
      case _C_SHT:
        sprintf (resultString, "%d", (int) getShortReturn (ret));
        break;
      case _C_USHT:
        sprintf (resultString, "%u", (unsigned) getUShortReturn (ret));
        break;
      case _C_LNG:
        sprintf (resultString, "%ld", getLongReturn (ret));
        break;
      case _C_ULNG:
        sprintf (resultString, "%lu", getULongReturn (ret));
        break;
      case _C_CHR:
        sprintf (resultString, "%d", (int) getCharReturn (ret));
        break;
      case _C_UCHR:
        sprintf (resultString, "%u", (unsigned) getUCharReturn (ret));
        break;
      case _C_CHARPTR:
        strcpy (resultString, getStringReturn (ret));
        break;
      case _C_FLT:
        sprintf (resultString, "%g", getFloatReturn (ret));
        break;
      case _C_DBL:
        sprintf (resultString, "%g", getDoubleReturn (ret));
        break;
      case _C_VOID:
        resultString[0] = '\0';
        break;
      default:
        {
          fprintf (stderr, "Tcl can't handle ret type `%s' in `%s'",
                   type, seltype);
          sprintf (resultString, "Tcl can't handle ret type %s", type);
          Tcl_SetResult (interp, resultString, TCL_VOLATILE);
          goto fail;
        }
      }

    Tcl_SetResult (interp, resultString, TCL_VOLATILE);
    if (tclObjc_eventHook)
      (*tclObjc_eventHook) ();
    [fc drop];
    [fa drop];
    return TCL_OK;
  fail:
    if (fc)
      [fc drop];
    [fa drop];
    return TCL_ERROR;
  }
}

void
tclObjc_registerObjectWithName (Tcl_Interp *interp, 
				    id object, const char *name)
{
  Tcl_CreateCommand(interp, (char *) name, (Tcl_CmdProc *)tclObjc_msgSendToClientData,
		    object, 0);
}

void
tclObjc_unregisterObjectNamed (Tcl_Interp *interp,
                               const char *name)
{
  Tcl_DeleteCommand(interp, (char *)name);
}

void
tclObjc_registerClassnames (Tcl_Interp *interp)
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

int
tclObjc_msgSendToArgv1 (ClientData clientData,
                        Tcl_Interp *interp,
                        int argc,
                        char *argv[])
{
  id obj;

  if ((obj = tclObjc_nameToObject(argv[1])) != TCLOBJC_NO_OBJ)
    return tclObjc_msgSendToClientData ((ClientData) obj,
                                        interp, 
                                        argc-1,
                                        &(argv[1]));
  else
    {
      sprintf(interp->result, 
	      "tclObjc: %s not recognized as an object", argv[1]);
      return TCL_ERROR;
    }
}


@implementation TclObject

- (BOOL)respondsTo: (SEL)aSel
{
  Tcl_CmdInfo cmdInfo;
  char selString[128];

  sprintf(selString, "%s%s", _tclName, sel_get_name(aSel));
  return (((object_is_instance (self)
            ? class_get_instance_method(self->ISA, aSel)
	    : class_get_class_method(self->ISA, aSel)) != METHOD_NULL)
	  || Tcl_GetCommandInfo(_interp, selString, &cmdInfo));
}

+ newName: (char *)objectName
{
  TclObject *newTclObject = class_create_instance (self);
  newTclObject->_tclName =
    (char*) objc_malloc ((unsigned) (strlen(objectName) + 1) * sizeof(char));
  strcpy(newTclObject->_tclName, objectName);
  /* Fix this ugliness!!! */
  newTclObject->_interp = _TclObject_interp;
  return newTclObject;
}

- free
{
  objc_free (_tclName);
  return object_dispose (self);
}  


- forward: (SEL)aSel : (arglist_t)argframe
{
  return [self performv: aSel :argframe];
}


#define marg_getRef(margs, offset, type) ( (type *)offset )

- performv:(SEL)aSel :(arglist_t)argframe
{
  char *datum;
  const char *type;
  const char *objcdebug;
  BOOL debug_printing;
  ObjcMethod method = 0;
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
	  sprintf(argString, PTRHEXFMT, 
	        *(void **) (marg_getRef(argframe, datum, void *)));
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
   rename unknown _unknown_pre_tclObjc \n\
 } \n\
 proc unknown {name args} {\n\
   if {[string match *%c0x* $name]} {\n\
     return [uplevel tclObjc_msg_send $name $args]\n\
   } else {\n\
     if {[llength [info procs _unknown_pre_tclObjc]]} {\n\
       uplevel 1 _unknown_pre_tclObjc $name $args\n\
     } else {\n\
       error \"in unknown: invalid command in tclObjc: $name\"\n\
     }\n\
   }\n\
 }\n";

int
TclObjc_Init (Tcl_Interp *interp)
{
  /* Fix this ugliness!!! */
  _TclObject_interp = interp;
  tclObjc_registerClassnames(interp);
  Tcl_CreateCommand(interp, "tclObjc_msg_send", 
		    (Tcl_CmdProc *)tclObjc_msgSendToArgv1, 0, 0);
  {
    int code;
    char buf [strlen (tclObjcInitCmd) + 1];

    sprintf (buf, tclObjcInitCmd, ATDELIMCHAR);
    code = Tcl_Eval(interp, buf);
    if (code != TCL_OK)
      {
        fprintf(stderr, "tclObjc: Error during TclObjc_Init:\n");
        fprintf(stderr, interp->result);
      }
  }
  return TCL_OK;
}

