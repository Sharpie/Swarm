// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/internal.h>
#import <defobj.h> // raiseEvent, DSIZE

#include <misc.h> // strtoul
#include <objc/objc-api.h>

size_t
alignto (size_t pos, size_t alignment)
{
  size_t mask = (alignment - 1);

  if ((pos & mask) == 0)
    return pos;
  else
    return (pos + alignment) & ~mask;
}

size_t
alignment_for_objc_type (const char *varType)
{
  size_t alignment = 0;

  switch (*varType)
    {
    case _C_SHT: case _C_USHT:
      alignment = __alignof__ (short);
      break;
    case _C_LNG: case _C_ULNG:
      alignment = __alignof__ (long);
      break;
    case _C_INT: case _C_UINT:
      alignment = __alignof__ (int);
      break;
    case _C_FLT:
      alignment = __alignof__ (float);
      break;
    case _C_DBL:
      alignment = __alignof__ (double);
      break;
    case _C_CHR: case _C_UCHR:
      alignment = __alignof__ (char);
      break;
    case _C_CHARPTR:
      alignment = __alignof__ (const char *);
      break;
    case _C_ID:
      alignment = __alignof__ (id);
      break;
    case _C_ARY_B:
      varType++;
      while (isdigit ((int) *varType))
        varType++;
      
      alignment = alignment_for_objc_type (varType);
      break;

    default:
      abort ();
    }
  return alignment;
}

size_t
size_for_objc_type (const char *varType)
{
  size_t size;

  switch (*varType)
    {
    case _C_SHT: case _C_USHT:
      size = sizeof (short);
      break;
    case _C_LNG: case _C_ULNG:
      size = sizeof (long);
      break;
    case _C_INT: case _C_UINT:
      size = sizeof (int);
      break;
    case _C_CHR: case _C_UCHR:
      size = sizeof (char);
      break;
    case _C_FLT:
      size = sizeof (float);
      break;
    case _C_DBL:
      size = sizeof (double);
      break;
    case _C_CHARPTR:
      size = sizeof (const char *);
      break;
    case _C_ID:
      size = sizeof (id);
      break;
    case _C_ARY_B:
      {
        char *tail;
        unsigned count = strtoul (varType + 1, &tail, 10);
       
        size = count * size_for_objc_type (tail);
      }
      break;
    default:
      abort ();
    }
  return size;
}

void
process_array (const char *type,
               void (*setup_array) (unsigned rank, unsigned *dims,
                                    const char *baseType),
               void (*start_dim) (unsigned dimnum),
               void (*end_dim) (void),
               void (*start_element) (void),
               void (*end_element) (void),
               void (*output_type) (const char *type,
                                    unsigned offset,
                                    void *data),
               const void *ptr,
               void *data)
{
  const char *baseType;
  char *tail;

  unsigned get_rank (const char *type)
    {
      unsigned rank = 0;
      
      do {
        
        errno = 0;
        strtoul (type + 1, &tail, 10);
        
        if (errno != 0)
          raiseEvent (InvalidArgument, "Value out of range [%s]", type + 1);
        
        rank++;
        type = tail;
      } while (*tail == _C_ARY_B);
      return rank;
    }
  {
    unsigned rank = get_rank (type);
    unsigned dims[rank];
    
    void fill_dims (const char *type)
      {
        unsigned dimnum = 0;

        do {
          
          errno = 0;
          dims[dimnum] = strtoul (type + 1, &tail, 10);
          if (errno != 0)
            raiseEvent (InvalidArgument, "Value out of range [%s]", type + 1);
          
          dimnum++;
          type = tail;
        } while (*tail == _C_ARY_B);
      }
    
    fill_dims (type);
    baseType = tail;

    if (setup_array)
      setup_array (rank, dims, baseType);

    if (output_type)
      {
        unsigned coord[rank];
        
        void permute (unsigned dim)
          {
            unsigned i;
            
            if (dim < rank)
              {
                if (start_dim)
                  start_dim (dim);
                for (i = 0; i < dims[dim]; i++)
                  {
                    coord[dim] = i;
                    permute (dim + 1);
                  }
                if (end_dim)
                  end_dim ();
              }
            else
              {
                unsigned offset = 0;
                unsigned mult = 1;
                
                offset = coord[rank - 1];
                for (i = rank - 1; i > 0; i--)
                  {
                    mult *= dims[i];
                    offset += coord[i - 1] * mult;
                  }
                if (start_element)
                  start_element ();
                output_type (baseType, offset, NULL);
                if (end_element)
                  end_element ();
              }
          }
        permute (0);
      }
  }
}

void
map_ivars (Class class,
           void (*process_object) (struct objc_ivar *ivar))
{
  struct objc_ivar_list *ivars = class->ivars;
  
  if (class->super_class)
    {
      if (strcmp (class->super_class->name, "Object_s") != 0)
        map_ivars (class->super_class, process_object);
    }
  
  if (ivars)
    {
      unsigned i, ivar_count = ivars->ivar_count;
      struct objc_ivar *ivar_list = ivars->ivar_list;
      
      for (i = 0; i < ivar_count; i++)
        {
          // Special case to allow member_t for setIndexFromMemberLoc: lists.
          if (strcmp (ivar_list[i].ivar_type, "{?=\"memberData\"[2^v]}") == 0)
            continue;
          process_object (&ivar_list[i]);
        }
    }
}

struct objc_ivar *
find_ivar (Class class, const char *name)
{
  struct objc_ivar_list *ivars = class->ivars;

  if (class->super_class)
    {
      if (strcmp (class->super_class->name, "Object_s") != 0)
        {
          struct objc_ivar *ret = find_ivar (class->super_class, name);
          
          if (ret)
            return ret;
        }
    }
  
  if (ivars)
    {
      unsigned i, ivar_count = ivars->ivar_count;
      struct objc_ivar *ivar_list = ivars->ivar_list;

      for (i = 0; i < ivar_count; i++)
        {
          if (strcmp (ivar_list[i].ivar_name, name) == 0)
            return &ivars->ivar_list[i];
        }
      return NULL;
    }
  return NULL;
}

void *
ivar_ptr (id obj, const char *name)
{
  struct objc_ivar *ivar = find_ivar ([obj class], name);

  if (ivar)
    return (void *) obj + ivar->ivar_offset;
  return NULL;
}

const char *
objc_type_for_array (const char *baseType, unsigned rank, unsigned *dims)
{
  unsigned i;
  char nbuf[DSIZE (unsigned) + 1];
  char buf[rank * (2 + DSIZE (unsigned)) + strlen (baseType) + 1], *p = buf;

  for (i = 0; i < rank ; i++)
    {
      *p++ = '[';
      sprintf (nbuf, "%u", dims[i]);
      p = stpcpy (p, nbuf);
    }
  p = stpcpy (p, baseType);
  for (i = 0; i < rank ; i++)
    *p++ = ']';
  *p = '\0';
  return strdup (buf);
}

void
lisp_process_array (const char *type,
                    const void *ptr, void *data,
                    id <OutputStream> stream,
                    BOOL deepFlag)
{
  const char *space;
  
  void lisp_setup_array (unsigned rank, unsigned *dims, const char *baseType)
    {
      char buf[1 + rank + 1]; // always big enough
      
      sprintf (buf, "#%u", rank);
      [stream catC: buf];
    }
  void lisp_start_dim (unsigned dim)
    {
      [stream catC: "("];
      space = "";
    }
  void lisp_end_dim (void)
    {
      [stream catC: ")"];
    }
  void lisp_start_element (void)
    {
      [stream catC: space];
    }
  void lisp_end_element (void)
    {
      space = " ";
    }
  void lisp_array_output_type (const char *type,
                               unsigned offset,
                               void *data)
    {
      lisp_output_type (type, ptr, offset, data, stream, deepFlag);
    }
    
  process_array (type,
                 lisp_setup_array,
                 lisp_start_dim,
                 lisp_end_dim,
                 lisp_start_element,
                 lisp_end_element,
                 lisp_array_output_type,
                 ptr,
                 data);
}

void
lisp_output_type (const char *type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag)
{
  char buf[50];

  switch (*type)
    {
    case _C_ID:
      {
        id obj = ((id *) ptr)[offset];

        if (obj == nil || !deepFlag)
          [stream catC: "#f"];
        else
          [obj lispOutDeep: stream];
        break;
      }
    case _C_CLASS:
      raiseEvent (NotImplemented, "Classes not supported [%s]", type);
      break;
    case _C_SEL:
      raiseEvent (NotImplemented, "Selectors not supported");
      break;
    case _C_CHR: 
    case _C_UCHR: 
      [stream catC: "#\\"];
      {
        unsigned char ch = ((unsigned char *) ptr)[offset];
        
        if (isprint (ch))
          {
            buf[0] = ch;
            buf[1] = '\0';
          }
        else
          sprintf (buf, "%03o", (unsigned) ch);
      }
      [stream catC: buf];
      break;
    case _C_SHT: 
      sprintf (buf, "%hd", ((short *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_USHT: 
      sprintf (buf, "%hu", ((unsigned short *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_INT:
      sprintf (buf, "%d", ((int *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_UINT:
      sprintf (buf, "%u", ((unsigned *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_LNG:
      sprintf (buf, "%ld", ((long *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_ULNG:
      sprintf (buf, "%lu", ((unsigned long *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_FLT:
      sprintf (buf, "%fF0", ((float *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_DBL:
      sprintf (buf, "%fD0", ((double *) ptr)[offset]);
      [stream catC: buf];
      break;
    case _C_BFLD:
      raiseEvent (NotImplemented, "Bit fields not supported [%s]", type);
      break;
    case _C_VOID:
      abort ();
      break;
    case _C_UNDEF: 
      abort ();
      break;
    case _C_PTR:
      raiseEvent (NotImplemented, "Pointers not supported [%s]", type);
      break;
    case _C_CHARPTR:
      [stream catC: "\""];
      [stream catC: ((const char **) ptr)[offset]];
      [stream catC: "\""];
      break;
    case _C_ATOM:
      raiseEvent (NotImplemented, "Atoms not supported");
      break;
    case _C_ARY_B:
      lisp_process_array (type, ptr, data, stream, deepFlag);
      break;
    case _C_ARY_E:
      abort ();
      break;
    case _C_UNION_B:
      raiseEvent (NotImplemented, "Unions not supported [%s]", type);
      break;
    case _C_UNION_E:
      abort ();
      break;
    case _C_STRUCT_B:
      raiseEvent (NotImplemented, "Structures not supported [%s]", type);
      break;
    case _C_STRUCT_E:
      abort ();
      break;
    default:
      abort ();
      break;
    }
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
