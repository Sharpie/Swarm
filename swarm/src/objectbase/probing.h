// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/ProbeLibrary.h>

extern id probeLibrary;
extern void initProbing();

void string_convert (fcall_type_t type, const types_t *p,
                     const char *floatFormat, unsigned precision,
                     id <Symbol> stringReturnType,
                     char *buf);
