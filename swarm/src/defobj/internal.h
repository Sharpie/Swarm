// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

extern const char *
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
               void *data);
