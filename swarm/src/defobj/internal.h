// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#include <misc.h> // size_t

extern size_t size_for_objc_type (const char *varType);
extern size_t alignment_for_objc_type (const char *varType);

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

