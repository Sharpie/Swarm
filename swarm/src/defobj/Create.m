// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
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
