// Swarm library. Copyright © 1996-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

#import <simtoolsgui/ProbeDisplay.h>
#import <simtoolsgui/SimpleProbeDisplay.h>
#import <simtoolsgui/CompleteProbeDisplay.h>
#import <gui.h>
#import <defobj/defalloc.h>

// SAFEPROBES enables lots of error checking here.
#define SAFEPROBES 1

static void
resetObjectError (void)
{
  raiseEvent (InvalidCombination,
              "It is an error to reset the object when building a ProbeDisplay\n");
}

//
//  notifyObjectDropped() -- function to notify the probe display
//                           when the object it's probing is dropped
//
static void
notifyObjectDropped (id anObject, id realloc, id pd)
{
  // Put a hook here so that the drop method knows whether the drop
  // was called from here.
  [pd setRemoveRef: 0];  // false => don't remove the reference in "drop"
  [pd markForDrop];
  // There might be an issue of recursivity if a user decided
  // to probe a probe display.  I ignored that. --gepr
}

@implementation ProbeDisplay

PHASE(Creating)

- setWindowGeometryRecordName: (const char *)theName
{
  windowGeometryRecordName = theName;

  return self;
}

- setProbedObject: anObject
{
  if (SAFEPROBES)
    {
      if (probedObject != 0)
        {
          resetObjectError ();
          return nil;
        }
    }
  probedObject = anObject;

  return self;
}

- setProbeMap: theProbeMap
{
  if (SAFEPROBES)
    {
      if (probeMap != 0)
        {
          resetObjectError ();
          return nil;
        }
    }
  probeMap = theProbeMap;

  return self;
}

- createEnd
{
  id probeDisplay;
	
  if (SAFEPROBES)
    {
      if (probedObject == 0)
        {
          raiseEvent (InvalidCombination,
                      "ProbeDisplay object was not properly initialized\n");
          return nil;
        }
    }
  
  GUI_UPDATE_IDLE_TASKS_AND_HOLD ();
  
  if (probeMap == nil)
    probeDisplay = [CompleteProbeDisplay createBegin: getZone (self)];
  else
    {
      probeDisplay = [SimpleProbeDisplay createBegin: getZone (self)];
      [probeDisplay setProbeMap: probeMap];
    }
  [probeDisplay setWindowGeometryRecordName: windowGeometryRecordName];
  [probeDisplay setProbedObject: probedObject];
  probeDisplay = [probeDisplay createEnd];

  // Probe notification mechanism added to handle automatic removal
  // of probe displays when an probed object is dropped.  --gepr
  [probeDisplay setObjectRef: [probedObject 
				addRef: (notify_t) notifyObjectDropped 
                                withArgument: (void *) probeDisplay ]];
  [probeDisplay setRemoveRef: 1];  // set this every time a reference is added

  GUI_RELEASE_AND_UPDATE ();
  
  [self drop];
  
  return probeDisplay;
}

PHASE(Using)

- getProbeMap
{
  return probeMap;
}

- getProbedObject
{
  return probedObject;
}

@end


