// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/internal.h>
#import <defobj.h> // raiseEvent

#include <misc.h> // strtoul
#include <objc/objc-api.h>

struct array_element {
  unsigned count;
  struct array_element *prev;
};

const char *
process_array (const char *type,
               void (*start_array) (unsigned rank, unsigned *dims),
               void (*end_array) (void),
               void (*start_dim) (unsigned dimnum),
               void (*end_dim) (void),
               void (*start_element) (void),
               void (*end_element) (void),
               const char * (*output_type) (const char *type,
                                            unsigned offset,
                                            void *data),
               const void *ptr,
               void *data)
{
  char *tail;
  struct array_element array_element;
  
  errno = 0;
  array_element.count = strtoul (type + 1, &tail, 10);
  if (errno != 0)
    raiseEvent (InvalidArgument, "Value out of range [%s]", type + 1);
  
  array_element.prev = data;
  
  if (*tail != _C_ARY_B)
    {
      struct array_element *aeptr;
      unsigned rank = 0;
      const char *ret = NULL;
      
      aeptr = &array_element;
      while (aeptr)
        {
          rank++;
          aeptr = aeptr->prev;
        }
      aeptr = &array_element;
      {
        unsigned dims[rank];
        unsigned i;
        
        i = rank;
        while (aeptr)
          {
            i--;
            dims[i] = aeptr->count;
            aeptr = aeptr->prev;
          }
        start_array (rank, dims);
        {
          unsigned coord[rank];
          
          void permute (unsigned dim)
            {
              unsigned i;
              
              if (dim < rank)
                {
                  start_dim (dim);
                  for (i = 0; i < dims[dim]; i++)
                    {
                      coord[dim] = i;
                      permute (dim + 1);
                    }
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
                  start_element ();
                  ret = output_type (tail, offset, NULL);
                  end_element ();
                }
            }
          permute (0);
        }
      }
      end_array ();
      return ret;
    }
  else
    return output_type (tail, 0, &array_element);
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
