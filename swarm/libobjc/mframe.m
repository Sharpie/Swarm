/* Implementation of functions for dissecting/making method calls 
   Copyright (C) 1994, 1995, 1996, 1997, 1998 Free Software Foundation, Inc.
   
   Written by:  Andrew Kachites McCallum <mccallum@gnu.ai.mit.edu>
   Created: Oct 1994
   
   This file is part of the GNUstep Base Library.

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

/* These functions can be used for dissecting and making method calls
   for many different situations.  They are used for distributed
   objects; they could also be used to make interfaces between
   Objective C and Scheme, Perl, Tcl, or other languages.

*/

#include <objc/objc-api.h>
#if defined(__hpux__) && defined(HAVE_SYS_SIGEVENT_H)
#include <sys/sigevent.h>
#endif
BOOL sel_types_match (const char* t1, const char* t2);
#include <objc/mframe.h>
#include <stdlib.h>
#include <string.h>

/* Deal with strrchr: */
#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
/* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#define index strchr
#define rindex strrchr
#define bcopy(s, d, n) memcpy ((d), (s), (n))
#define bcmp(s1, s2, n) memcmp ((s1), (s2), (n))
#define bzero(s, n) memset ((s), 0, (n))
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#include <strings.h>
/* memory.h and strings.h conflict on some systems.  */
#endif /* not STDC_HEADERS and not HAVE_STRING_H */

void *alloca (size_t);

#define inline

/* For encoding and decoding the method arguments, we have to know where
   to find things in the "argframe" as returned by __builtin_apply_args.

   For some situations this is obvious just from the selector type 
   encoding, but structures passed by value cause a problem because some
   architectures actually pass these by reference, i.e. use the
   structure-value-address mentioned in the gcc/config/_/_.h files.

   These differences are not encoded in the selector types.

   Below is my current guess for which architectures do this.
   xxx I really should do this properly by looking at the gcc config values.

   I've also been told that some architectures may pass structures with
   sizef(structure) > sizeof(void*) by reference, but pass smaller ones by
   value.  The code doesn't currently handle that case.
   */


char*
mframe_build_signature(const char *typePtr, int *size, int *narg, char *buf)
{
  MFRAME_ARGS	cum;
  BOOL		doMalloc = NO;
  const		char	*types;
  char		*start;
  char		*dest;
  int		total = 0;
  int		count = 0;

  /*
   *	If we have not been given a buffer - allocate space on the stack for
   *	the largest concievable type encoding.
   */
  if (buf == 0)
    {
      doMalloc = YES;
      buf = alloca((strlen(typePtr)+1)*16);
    }

  /*
   *	Copy the return type info (including qualifiers) into the buffer.
   */
  types = objc_skip_typespec(typePtr);
  strncpy (buf, typePtr, types - typePtr);
  buf[types-typePtr] = '\0'; 

  /*
   *	Point to the return type, initialise size of stack args, and skip
   *	to the first argument.
   */
  types = objc_skip_type_qualifiers(typePtr);
  MFRAME_INIT_ARGS(cum, types);
  types = objc_skip_typespec(types);
  if (*types == '+')
    {
      types++;
    }
  while (isDigit (*types))
    types++;

  /*
   *	Where to start putting encoding information - leave enough room for
   *	the size of the stack args to be stored after the return type.
   */
  start = &buf[strlen(buf)+10];
  dest = start;

  /*
   *	Now step through all the arguments - copy any type qualifiers, but
   *	let the macro write all the other info into the buffer.
   */
  while (types && *types)
    {
      const char	*qual = types;

      /*
       *	If there are any type qualifiers - copy the through to the
       *	destination.
       */
      types = objc_skip_type_qualifiers(types);
      while (qual < types)
	{
	  *dest++ = *qual++;
	}
      MFRAME_ARG_ENCODING(cum, types, total, dest);
      count++;
    }
  *dest = '\0';

  /*
   *	Write the total size of the stack arguments after the return type,
   *	then copy the remaining type information to fill the gap.
   */
  sprintf(&buf[strlen(buf)], "%d", total);
  dest = &buf[strlen(buf)];
  while (*start)
    {
      *dest++ = *start++;
    }
  *dest = '\0';

  /*
   *	If we have written into a local buffer - we need to allocate memory
   *	in which to return our result.
   */
  if (doMalloc)
    {
      char	*tmp = objc_malloc(dest - buf + 1);

      strcpy(tmp, buf);
      buf = tmp;
    }

  /*
   *	If the caller wants to know the total size of the stack and/or the
   *	number of arguments, return them in the appropriate variables.
   */
  if (size)
    {
      *size = total;
    }
  if (narg)
    {
      *narg = count;
    }
  return buf;
}


/*
 *      Step through method encoding information extracting details.
 */
const char *
mframe_next_arg(const char *typePtr, NSArgumentInfo *info)
{
  NSArgumentInfo	local;
  BOOL			flag;

  if (info == 0)
    {
      info = &local;
    }
  /*
   *	Skip past any type qualifiers - if the caller wants them, return them.
   */
  flag = YES;
  info->qual = 0;
  while (flag)
    {
      switch (*typePtr)
	{
	  case _C_CONST:  info->qual |= _F_CONST; break;
	  case _C_IN:     info->qual |= _F_IN; break;
	  case _C_INOUT:  info->qual |= _F_INOUT; break;
	  case _C_OUT:    info->qual |= _F_OUT; break;
	  case _C_BYCOPY: info->qual |= _F_BYCOPY; break;
#ifdef	_C_BYREF
	  case _C_BYREF:  info->qual |= _F_BYREF; break;
#endif
	  case _C_ONEWAY: info->qual |= _F_ONEWAY; break;
	  default: flag = NO;
	}
      if (flag)
	{
	  typePtr++;
	}
    }

  info->type = typePtr;

  /*
   *	Scan for size and alignment information.
   */
  switch (*typePtr++)
    {
      case _C_ID:
	info->size = sizeof(id);
	info->align = __alignof__(id);
	break;

      case _C_CLASS:
	info->size = sizeof(Class);
	info->align = __alignof__(Class);
	break;

      case _C_SEL:
	info->size = sizeof(SEL);
	info->align = __alignof__(SEL);
	break;

      case _C_CHR:
	info->size = sizeof(char);
	info->align = __alignof__(char);
	break;

      case _C_UCHR:
	info->size = sizeof(unsigned char);
	info->align = __alignof__(unsigned char);
	break;

      case _C_SHT:
	info->size = sizeof(short);
	info->align = __alignof__(short);
	break;

      case _C_USHT:
	info->size = sizeof(unsigned short);
	info->align = __alignof__(unsigned short);
	break;

      case _C_INT:
	info->size = sizeof(int);
	info->align = __alignof__(int);
	break;

      case _C_UINT:
	info->size = sizeof(unsigned int);
	info->align = __alignof__(unsigned int);
	break;

      case _C_LNG:
	info->size = sizeof(long);
	info->align = __alignof__(long);
	break;

      case _C_ULNG:
	info->size = sizeof(unsigned long);
	info->align = __alignof__(unsigned long);
	break;

#ifdef	_C_LNG_LNG
      case _C_LNG_LNG:
	info->size = sizeof(long long);
	info->align = __alignof__(long long);
	break;

      case _C_ULNG_LNG:
	info->size = sizeof(unsigned long long);
	info->align = __alignof__(unsigned long long);
	break;

#endif
      case _C_FLT:
	info->size = sizeof(float);
	info->align = __alignof__(float);
	break;

      case _C_DBL:
	info->size = sizeof(double);
	info->align = __alignof__(double);
	break;

      case _C_PTR:
	info->size = sizeof(char*);
	info->align = __alignof__(char*);
	if (*typePtr == '?')
	  {
	    typePtr++;
	  }
	else
	  {
	    typePtr = mframe_next_arg(typePtr, &local);
	    info->isReg = local.isReg;
	    info->offset = local.offset;
	  }
	break;

      case _C_ATOM:
      case _C_CHARPTR:
	info->size = sizeof(char*);
	info->align = __alignof__(char*);
	break;

      case _C_ARY_B:
	{
	  int	length = atoi(typePtr);

	  while (isDigit (*typePtr))
            typePtr++;
	  typePtr = mframe_next_arg(typePtr, &local);
	  info->size = length * ROUND(local.size, local.align);
	  info->align = local.align;
	  typePtr++;	/* Skip end-of-array	*/
	}
	break; 

      case _C_STRUCT_B:
	{
	  struct { int x; double y; } fooalign;
	  int acc_size = 0;
	  int acc_align = __alignof__(fooalign);

	  /*
	   *	Skip "<name>=" stuff.
	   */
	  while (*typePtr != _C_STRUCT_E)
	    {
	      if (*typePtr++ == '=')
		{
		  break;
		}
	    }
	  /*
	   *	Base structure alignment on first element.
	   */
	  if (*typePtr != _C_STRUCT_E)
	    {
	      typePtr = mframe_next_arg(typePtr, &local);
	      if (typePtr == 0)
		{
		  return 0;		/* error	*/
		}
	      acc_size = ROUND(acc_size, local.align);
	      acc_size += local.size;
	      acc_align = MAX(local.align, __alignof__(fooalign));
	    }
	  /*
	   *	Continue accumulating structure size.
	   */
	  while (*typePtr != _C_STRUCT_E)
	    {
	      typePtr = mframe_next_arg(typePtr, &local);
	      if (typePtr == 0)
		{
		  return 0;		/* error	*/
		}
	      acc_size = ROUND(acc_size, local.align);
	      acc_size += local.size;
	    }
	  info->size = acc_size;
	  info->align = acc_align;
	  typePtr++;	/* Skip end-of-struct	*/
	}
	break;

      case _C_UNION_B:
	{
	  unsigned	max_size = 0;
	  unsigned	max_align = 0;

	  /*
	   *	Skip "<name>=" stuff.
	   */
	  while (*typePtr != _C_UNION_E)
	    {
	      if (*typePtr++ == '=')
		{
		  break;
		}
	    }
	  while (*typePtr != _C_UNION_E)
	    {
	      typePtr = mframe_next_arg(typePtr, &local);
	      if (typePtr == 0)
		{
		  return 0;		/* error	*/
		}
	      max_size = MAX(max_size, local.size);
	      max_align = MAX(max_align, local.align);
	    }
	  info->size = max_size;
	  info->align = max_align;
	  typePtr++;	/* Skip end-of-union	*/
	}
	break;

      case _C_VOID:
	info->size = 0;
	info->align = __alignof__(char*);
	break;

      default:
	return 0;
    }

  if (typePtr == 0)
    {		/* Error condition.	*/
      return 0;
    }

  /*
   *	If we had a pointer argument, we will already have gathered
   *	(and skipped past) the argframe offset information - so we
   *	don't need to (and can't) do it here.
   */
  if (info->type[0] != _C_PTR || info->type[1] == '?')
    {
      BOOL negFlag = NO;

      /*
       *	May tell the caller if the item is stored in a register.
       */
      if (*typePtr == '-')
        {
          typePtr++;
          negFlag = YES;
          info->isReg = NO;
        }
      else if (*typePtr == '+')
	{
	  typePtr++;
	  info->isReg = YES;
	}
      else
        info->isReg = NO;

      /*
       *	May tell the caller what the stack/register offset is for
       *	this argument.
       */
      info->offset = 0;
      while (isDigit (*typePtr))
        info->offset = info->offset * 10 + (*typePtr++ - '0');
      if (negFlag)
        info->offset *= -1;
    }
  return typePtr;
}



/* Return the number of arguments that the method MTH expects.  Note
   that all methods need two implicit arguments `self' and `_cmd'.  */

int
method_types_get_number_of_arguments (const char *type)
{
  int i = 0;

  while (*type)
    {
      type = objc_skip_argspec (type);
      i += 1;
    }
  return i - 1;
}


/* Return the size of the argument block needed on the stack to invoke
  the method MTH.  This may be zero, if all arguments are passed in
  registers.  */

int
method_types_get_size_of_stack_arguments (const char *type)
{
  type = objc_skip_typespec (type);
  return atoi (type);
}

int
method_types_get_size_of_register_arguments(const char *types)
{
  const char* type = strrchr(types, '+');

  if (type)
    {
      return atoi(++type) + sizeof(void*);
    }
  else
    {
      return 0;
    }
}


/* To fix temporary bug in method_get_next_argument() on NeXT boxes */
/* xxx Perhaps this isn't working with the NeXT runtime? */

char*
method_types_get_next_argument (arglist_t argf, const char **type)
{
  const char *t = objc_skip_argspec (*type);
  arglist_t	argframe;

  argframe = (void*)argf;

  if (*t == '\0')
    {
      return 0;
    }
  *type = t;
  t = objc_skip_typespec (t);

  if (*t == '+')
    {
      return argframe->arg_regs + atoi(++t);
    }
  else
    {
      /* xxx What's going on here?  This -8 needed on my 68k NeXT box. */
#if NeXT
      return argframe->arg_ptr + (atoi(t) - 8);
#else
      return argframe->arg_ptr + atoi(t);
#endif
    }
}


/* mframe_dissect_call()

   This function encodes the arguments of a method call.

   Call it with an ARGFRAME that was returned by __builtin_args(), and
   a TYPE string that describes the input and return locations,
   i.e. from sel_get_types() or Method->method_types.

   The function ENCODER will be called once with each input argument.

   Returns YES iff there are any outparameters---parameters that for
   which we will have to get new values after the method is run,
   e.g. an argument declared (out char*). */

BOOL
mframe_dissect_call_opts (arglist_t argframe, const char *type,
		     void (*encoder)(int,void*,const char*,int),
			BOOL pass_pointers)
{
  unsigned flags;
  char *datum;
  int argnum;
  BOOL out_parameters = NO;

  if (*type == _C_STRUCT_B || *type == _C_UNION_B || *type == _C_ARY_B) {
    datum = alloca((strlen(type)+1)*10);
    type = mframe_build_signature(type, 0, 0, datum);
  }
  /* Enumerate all the arguments in ARGFRAME, and call ENCODER for
     each one.  METHOD_TYPES_GET_NEXT_ARGUEMENT() returns 0 when
     there are no more arguments, otherwise it returns a pointer to the
     argument in the ARGFRAME. */

  for (datum = method_types_get_next_argument(argframe, &type), argnum=0;
       datum;
       datum = method_types_get_next_argument(argframe, &type), argnum++)
    {
      /* Get the type qualifiers, like IN, OUT, INOUT, ONEWAY. */
      flags = objc_get_type_qualifiers(type);

      /* Skip over the type qualifiers, so now TYPE is pointing directly
	 at the char corresponding to the argument's type, as defined
	 in <objc/objc-api.h> */
      type = objc_skip_type_qualifiers(type);

      /* Decide how, (or whether or not), to encode the argument
	 depending on its FLAGS and TYPE.  Only the first two cases
	 involve parameters that may potentially be passed by
	 reference, and thus only the first two may change the value
	 of OUT_PARAMETERS. */

      switch (*type)
	{

	case _C_CHARPTR:
	  /* Handle a (char*) argument. */
	  /* If the char* is qualified as an OUT parameter, or if it
	     not explicitly qualified as an IN parameter, then we will
	     have to get this char* again after the method is run,
	     because the method may have changed it.  Set
	     OUT_PARAMETERS accordingly. */
	  if ((flags & _F_OUT) || !(flags & _F_IN))
	    out_parameters = YES;
	  /* If the char* is qualified as an IN parameter, or not
             explicity qualified as an OUT parameter, then encode
             it. */
	  if ((flags & _F_IN) || !(flags & _F_OUT))
	    (*encoder) (argnum, datum, type, flags);
	  break;

	case _C_PTR:
	  /* If the pointer's value is qualified as an OUT parameter,
	     or if it not explicitly qualified as an IN parameter,
	     then we will have to get the value pointed to again after
	     the method is run, because the method may have changed
	     it.  Set OUT_PARAMETERS accordingly. */
	  if ((flags & _F_OUT) || !(flags & _F_IN))
	    out_parameters = YES;
	  if (pass_pointers) {
	    if ((flags & _F_IN) || !(flags & _F_OUT))
	      (*encoder) (argnum, datum, type, flags);
	  }
	  else {
	    /* Handle an argument that is a pointer to a non-char.  But
	       (void*) and (anything**) is not allowed. */
	    /* The argument is a pointer to something; increment TYPE
		 so we can see what it is a pointer to. */
	    type++;
	    /* If the pointer's value is qualified as an IN parameter,
	       or not explicity qualified as an OUT parameter, then
	       encode it. */
	    if ((flags & _F_IN) || !(flags & _F_OUT))
	      (*encoder) (argnum, *(void**)datum, type, flags);
	  }
	  break;

	case _C_STRUCT_B:
	case _C_UNION_B:
	case _C_ARY_B:
	  /* Handle struct and array arguments. */
	  /* Whether DATUM points to the data, or points to a pointer
	     that points to the data, depends on the value of
	     MFRAME_STRUCT_BYREF.  Do the right thing
	     so that ENCODER gets a pointer to directly to the data. */
#if MFRAME_STRUCT_BYREF
	  (*encoder) (argnum, *(void**)datum, type, flags);
#else
	  (*encoder) (argnum, datum, type, flags);
#endif
	  break;

	default:
	  /* Handle arguments of all other types. */
	  (*encoder) (argnum, datum, type, flags);
	}
    }

  /* Return a BOOL indicating whether or not there are parameters that
     were passed by reference; we will need to get those values again
     after the method has finished executing because the execution of
     the method may have changed them.*/
  return out_parameters;
}

BOOL
mframe_dissect_call (arglist_t argframe, const char *type,
		     void (*encoder)(int,void*,const char*,int))
{
  return mframe_dissect_call_opts(argframe, type, encoder, NO);
}

