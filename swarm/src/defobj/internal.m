// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/internal.h>
#import <defobj.h> // raiseEvent

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
map_ivars (struct objc_ivar_list *ivars,
           void (*process_object) (struct objc_ivar *ivar))
{
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

#if ((__GNUC__ == 2) && (__GNUC_MINOR__ == 8)) && (__GNUC__ > 2)
id
nil_method (id receiver, SEL op, ...)
{
  [NotImplemented raiseEvent:  "The message `%s' was sent to nil.\n",
                  sel_get_name (op)];

  return nil;
}
#endif
