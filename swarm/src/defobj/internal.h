// Swarm library. Copyright © 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <collections.h> // OutputStream
#include <misc.h> // size_t
#include <objc/objc-api.h>

extern size_t alignsizeto (size_t pos, size_t alignment);
extern void *alignptrto (void *ptr, size_t alignment);

extern size_t size_for_objc_type (const char *varType);
extern size_t alignment_for_objc_type (const char *varType);
extern void map_ivars (Class class,
                       void (*process_object) (struct objc_ivar *ivar));
extern struct objc_ivar *find_ivar (id obj, const char *name);
extern void *ivar_ptr (id obj, const char *name);
extern const char *objc_type_for_array (const char *baseType, unsigned rank, unsigned *dims);

extern void
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
               void *data);
  
extern void
lisp_output_type (const char *type,
                  const void *ptr,
                  unsigned offset,
                  void *data,
                  id <OutputStream> stream,
                  BOOL deepFlag);

extern void
lisp_process_array (const char *type,
                    const void *ptr, void *data,
                    id <OutputStream> stream,
                    BOOL deepFlag);

extern char *zstrdup (id aZone, const char *str);

