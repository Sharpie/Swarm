// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj.h>
#import <gui.h>

id <Error> WindowCreation, WindowUsage;

void
initAWTObjc (id arguments)
{
  deferror (WindowCreation, NULL);
  deferror (WindowUsage, NULL);
}

