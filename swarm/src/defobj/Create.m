// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

/*
Name:         Create.m
Description:  superclass for implementation of create-phase customization
Library:      defobj
*/

#import <defobj/Create.h>


//
// CreateDrop_s -- superclass for create protocol with phase switching
//

@implementation CreateDrop_s

PHASE(Creating)

//
// create: -- allocate and initialize an object in a specified zone
//
+ create: aZone
{
  return [[self createBegin: aZone] createEnd];
}

//
// createBegin: -- begin object creation in a specified zone
//
+ createBegin: aZone
{
  CreateDrop_s  *newObject;

  newObject = [aZone allocIVars: self];
  newObject->zone = aZone;
  return newObject;
}

//
// createEnd -- finalize an instance being created and return result
//
- createEnd  
{
  createByCopy();
  setNextPhase( self );
  return self;
}

PHASE(Using)

//
// getZone -- return zone saved within object
//
- getZone
{
  return zone;
}

//
// drop -- drop object from zone in which allocated
//
- (void) drop
{
  [zone freeIVars: self];
}

@end


//
// CreateDrop -- superclass for create protocol without phase switching
//

@implementation CreateDrop

//
// createEnd -- finalize an instance being created and return result
//
- createEnd  
{
  createByCopy();
  return self;
}

@end


@implementation Drop_s

//
// dropFrom: -- release an object from the zone in which it was allocated
//
- (void) dropFrom: aZone
{
  extern BOOL _warning_dropFrom;
  if ( ! _warning_dropFrom ) {
    _warning_dropFrom = 1;
    raiseEvent( ObsoleteMessage,
      "dropFrom: aZone has been replaced by drop in all uses\n" );
  }

  if ( aZone != zone )
    raiseEvent( SourceMessage,
      "dropFrom: -- argument zone does not match zone returned by getZone\n" );
  [(id)self drop];
}

- getZone
{
  return zone;
}

- (void) drop  // override in subclass to free any additional allocations
{
  [zone freeIVars: self];
}

@end
