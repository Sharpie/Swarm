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

// (nelson) these are necessary to patch a bug in the gcc 2.7.2 runtime,
// the file encoding.c. Manor wrote the patch.

#import <objectbase/swarm_rts_routines.h>
#include <misc.h> // abort, isDigit

#include "internal.h"

const char*
skip_type_qualifiers (const char *type)
{
  while (*type == _C_CONST
         || *type == _C_IN 
         || *type == _C_INOUT
         || *type == _C_OUT 
         || *type == _C_BYCOPY
         || *type == _C_ONEWAY)
    type++;
  return type;
}

const char* 
skip_typespec (const char *type)
{
  type = skip_type_qualifiers (type);
  
  switch (*type)
    {
    case _C_ID:
      /* An id may be annotated by the actual type if it is known
         with the @"ClassName" syntax */
      
      if (*++type != '"')
        return type;
      else
        {
          while (*++type != '"') /* do nothing */;
          return type + 1;
        }
      
      /* The following are one character type codes */
    case _C_CLASS:
    case _C_SEL:
    case _C_CHR:
    case _C_UCHR:
    case _C_CHARPTR:
    case _C_ATOM:
    case _C_SHT:
    case _C_USHT:
    case _C_INT:
    case _C_UINT:
    case _C_LNG:
    case _C_ULNG:
    case _C_LNG_LNG:
    case _C_ULNG_LNG:
    case _C_FLT:
    case _C_DBL:
    case _C_LNG_DBL:
    case _C_VOID:
    case _C_UNDEF:
      return ++type;
      break;
    case _C_BFLD:
      while (isDigit (*++type));
      type = skip_typespec (type);
      while (isDigit (*type)) type++;
      return type;
      
    case _C_ARY_B:
      /* skip digits, typespec and closing ']' */
      
      while (isDigit (*++type));
      type = skip_typespec (type);
      if (*type == _C_ARY_E)
        return ++type;
      else
        abort();
      
    case _C_STRUCT_B:
      /* skip name, and elements until closing '}'  */
      
      while (*type != _C_STRUCT_E && *type++ != '=');
      while (*type != _C_STRUCT_E) 
        type = skip_typespec (type);
      return ++type;
      
    case _C_UNION_B:
      /* skip name, and elements until closing ')'  */
      
      while (*type != _C_UNION_E && *type++ != '=');
      while (*type != _C_UNION_E)
        type = skip_typespec (type);
      return ++type;
      
    case _C_PTR:
      ++type;
      /* Just skip the following typespec */
      if (isDigit (*type))
        return type;
      else
        return skip_typespec (type);
      
    default:
      abort();
    }
}

const char* 
skip_offset (const char* type)
{
  if (*type == '+')
    type++;
  while (isDigit (*++type));
  return (*type == '\0') ? NULL : type;
}

const char*
skip_argspec (const char* type)
{
  type = skip_typespec (type);
  type = skip_offset (type);
  return type;
}

int
get_number_of_arguments (const char *type)
{
  int i;
 
  for (i = 0; type; type = skip_argspec (type))
    i++;
  return i - 1;
}

