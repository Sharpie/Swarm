// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         defobj.m
Description:  global data and functions for Swarm kernel
Library:      defobj
*/

#import <defobj.h>

#include "defobj.xm"
#import <defobj/Archiver.h>

#include <objc/objc-api.h> // objc_lookup_class
#include <misc.h> // strcmp, sscanf

id  t_Object, t_ByteArray;
BOOL _warning_dropFrom = YES;
BOOL _obj_debug = YES;
FILE *_obj_xerror, *_obj_xdebug;

//
// _defobj_implement() -- generate implementations for defobj module
//
void
_defobj_implement (void)
{
  [id_Zone_c setTypeImplemented: Zone];
  [id_Symbol_c setTypeImplemented: Symbol];
  [id_Warning_c setTypeImplemented: Warning];
  [id_Error_c setTypeImplemented: Error];
  [id_Arguments_c setTypeImplemented: Arguments];
}

//
// _defobj_initialize() -- initialize global data for defobj module
//
void
_defobj_initialize (void)
{
  // initialize error messages

  [InvalidCombination setMessageString:
"> Customization messages sent to a type are incompatible with each other\n"
"> or with other requirements of the type.\n" ];

  [InvalidArgument setMessageString:
"> Invalid argument value passed in call.\n"];

  [OutOfMemory setMessageString:
"> No more memory available from the system.  Value of sbrk: %0#8x\n"];

  [InvalidAllocSize setMessageString:
"> Requested allocation size must be at least one byte.\n"
"> (Requested allocation size was zero.)\n"];

  [BlockedObjectAlloc setMessageString:
"> Requested operation is defined by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because its default\n"
"> implementation is incompatible with the model of zone-based allocation\n"
"> established by the defobj package.  To allocate, free, or copy objects\n"
"> that use the defined model of zone-based allocation, one of the messages\n"
"> create:, createBegin:/End, drop, or copy: must be used instead.\n"];

  [BlockedObjectUsage setMessageString:
"> Requested operation is implemented by the Object superclass of the GNU\n"
"> Objective C runtime system, but is blocked from usage because it is not\n"
"> part of the standard public view established by the defobj package.\n"
"> See documentation for an explanation of the supported public view,\n"
"> including functional equivalents for most messages defined by the Object\n"
"> superclass.  This error may be avoided by compiling the defobj library\n"
"> without the -DINHERIT_OBJECT_WITH_ERRORS compile-time flag set.\n"];

  [ProtocolViolation setMessageString:
"> This object does not comply with an expected protocol\n"];

}

void
initDefobj (int argc, const char **argv)
{
  arguments = [Arguments createArgc: argc
                         Argv: argv
                         version: NULL
                         bugAddress: NULL
                         options: NULL
                         parseFunc: NULL];
  archiver = [Archiver ensure: globalZone];
}

void
initDefobjApp (int argc, const char **argv,
               const char *version,
               const char *bugAddress)
{
  arguments = [Arguments createArgc: argc
                         Argv: argv
                         version: version
                         bugAddress: bugAddress
                         options: NULL
                         parseFunc: NULL];
  archiver = [Archiver ensure: globalZone];
}


void
initDefobjAppFunc (int argc, const char **argv,
                   const char *version,
                   const char *bugAddress,
                   struct argp_option *options,
                   int (*parseKeyFunc) (int key, const char *arg))
{
  arguments = [Arguments createArgc: argc
                         Argv: argv
                         version: version
                         bugAddress: bugAddress
                         options: options
                         parseFunc: parseKeyFunc];
  archiver = [Archiver ensure: globalZone];
}

void
initDefobjAppArguments (int argc, const char **argv,
                        const char *version,
                        const char *bugAddress,
                        Class arguments)
{
  arguments = [arguments createArgc: argc
                         Argv: argv
                         version: version
                         bugAddress: bugAddress
                         options: NULL
                         parseFunc: NULL];
  archiver = [Archiver ensure: globalZone];
}

id
nameToObject (const char *name)
{
  id object;
  void *val;
  const char *p = name;
  
  while (*p != '@' && *p != '\0')
    p++;
  if ((*p) && (sscanf (p + 3, "%p", &val) == 1))
    return (id) val;
  else if ((!strcmp (name, "nil"))
           || (!strcmp (name, "Nil"))
           || (!strcmp (name, "0x0")))
    return nil;
  else if ((object = (id) objc_lookup_class (name)))
    return object;
  abort ();
}

#if ((__GNUC__ == 2) && (__GNUC_MINOR__ == 8)) && (__GNUC__ > 2)
id
nil_method (id receiver, SEL op, ...)
{
  [NotImplemented raiseEvent:  "The message `%s' was sent to nil.\n",
                  sel_get_name (op)];

  return nil;
}
#endif
