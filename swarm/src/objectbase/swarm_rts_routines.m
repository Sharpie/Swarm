// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// (nelson) these are necessary to patch a bug in the gcc 2.7.2 runtime,
// the file encoding.c. Manor wrote the patch.

#import <objectbase/swarm_rts_routines.h>

// Avoid using chars as an index to ctype table.
#define isDigit(ch) isdigit((int)ch)

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
    case _C_FLT:
    case _C_DBL:
    case _C_VOID:
    case _C_UNDEF:
      return ++type;
      break;
      
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
      /* Just skip the following typespec */
      
      return skip_typespec (++type);
      
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

