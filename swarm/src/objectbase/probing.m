// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <collections.h>

#import "probing.h"
#import "local.h"
#include <misc.h> // abort

#include <swarmconfig.h>
#ifdef HAVE_JDK
#include "../defobj/java.h"
#endif

externvardef id <Symbol> DefaultString, CharString, IntString;
externvardef id probeLibrary;

void
initProbing ()
{
  static BOOL already_initialized = 0;

  if (already_initialized)
    return;
  already_initialized = 1;
  
  defsymbol (DefaultString);
  defsymbol (CharString);
  defsymbol (IntString);  

  probeLibrary = [ProbeLibrary create: globalZone];
}

int
p_compare (id a, id b)
{
  if (!([a compare: b]))
    return 0;
  else
    return -1;
}

void
string_convert (fcall_type_t type, const types_t *p,
                const char *floatFormat, unsigned precision,
                id <Symbol> stringReturnType,
                char *buf)
{
  switch (type)
    {
    case fcall_type_boolean:
      strcpy (buf, p->boolean ? "true" : "false");
      break;
      
    case fcall_type_object:
      if (!p->object)
        sprintf (buf, "nil");
      else 
        {
          const char *name = NULL;
          
          if ([p->object respondsTo: @selector (getDisplayName)])
            name = [p->object getDisplayName];

          if (!name)
            name = [p->object name];
          strcpy (buf, name);
        }
      break;
    case fcall_type_class:
      if (!p->_class)
        sprintf (buf, "nil");
      else
        sprintf (buf, "%s", swarm_class_getName(p->_class));
      break;
    case fcall_type_uchar:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%u '%c'", 
                 (unsigned) p->uchar,
                 p->uchar);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'", p->uchar);
      else if (stringReturnType == IntString)
        sprintf (buf, "%u", (unsigned) p->uchar);
      else
        raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case fcall_type_schar:
      if (stringReturnType == DefaultString)
        sprintf (buf, "%d '%c'",
                 (int) p->schar,
                 p->schar);
      else if (stringReturnType == CharString)
        sprintf (buf, "'%c'", p->schar);
      else if (stringReturnType == IntString)
        sprintf (buf, "%d",(int) p->schar);
      else
       raiseEvent (InvalidArgument, "stringReturnType set incorrectly!\n");
      break;
    case fcall_type_ushort:
      sprintf (buf, "%hu", p->ushort);
      break;
    case fcall_type_sshort:
      sprintf (buf, "%hd", p->sshort);
      break;
    case fcall_type_sint:
      sprintf (buf, "%d", p->sint);
      break;
    case fcall_type_uint:
      sprintf (buf, "%u", p->uint);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_ulonglong:
#endif
    case fcall_type_ulong:
      sprintf (buf, "%lu", p->ulong);
      break;
#if SIZEOF_LONG_LONG == SIZEOF_LONG
    case fcall_type_slonglong:
#endif
    case fcall_type_slong:
      sprintf (buf, "%ld", p->slong);
      break;
#if defined(LLFMT) && (SIZEOF_LONG_LONG > SIZEOF_LONG)
    case fcall_type_slonglong:
      sprintf (buf, "%" LLFMT "d", p->slonglong);
      break;
    case fcall_type_ulonglong:
      sprintf (buf, "%" LLFMT "u", p->ulonglong);
      break;
#endif
    case fcall_type_float:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision,
                 (double) p->_float);
      else
        sprintf (buf, floatFormat, p->_float);
      break;
    case fcall_type_double:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision, p->_double);
      else
        sprintf (buf, floatFormat, p->_double);
      break;
    case fcall_type_long_double:
      if (!floatFormat)
        sprintf (buf, "%.*g", (int) precision,
                 (double) p->_long_double);
      else
        sprintf (buf, floatFormat, (double) p->_long_double);
      break;
    case fcall_type_string:
      sprintf (buf, "%s", p->string ? p->string : "<NULL>");
      break;
    case fcall_type_selector:
      sprintf (buf, p->selector ? swarm_sel_getName (p->selector) : "M(NULL)");
      break;
    case fcall_type_void:
      sprintf (buf, "none");
      break;
    case fcall_type_jobject:
    case fcall_type_jselector:
#ifdef HAVE_JDK
      {
        const char *name = java_class_name ((jobject) p->object);

        strcpy (buf, name);
        SFREEBLOCK (name);
      }
      break;
#endif
    case fcall_type_jstring:
#ifdef HAVE_JDK
      {
        jboolean copyFlag;
        const char *utf =
          (*jniEnv)->GetStringUTFChars (jniEnv,
                                        (jstring) p->string,
                                        &copyFlag);
        strcpy (buf, utf);
        if (copyFlag)
          (*jniEnv)->ReleaseStringUTFChars (jniEnv, (jstring) p->string, utf);
        break;
      }
#endif
    case fcall_type_iid:
      abort ();
    }
}
