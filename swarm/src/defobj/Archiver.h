// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>
#import <defobj.h>

extern id archiver;

@interface Archiver: CreateDrop_s
{
  id currentApplicationKey;
  id applicationMap;
  const char *lispPath;
@public
  id clients;
}

+ createBegin: aZone;
- setLispPath: (const char *)lispPath;
- save;
- getMap;

- lispOut: outputCharStream;

void archiverRegister (id client);
void archiverUnregister (id client);
void archiverPut (const char *key, id object);
id archiverGet (const char *key);
void archiverSave (void);

@end

