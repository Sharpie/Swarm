// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there.  One argument is passed on the message,
// the display widget.

#import <space/Int2dFiler.h>
#import <space/Discrete2d.h> // discrete2dSiteAt
#import <simtools.h>

@implementation Int2dFiler

PHASE(Creating)

+ createBegin: aZone
{
  Int2dFiler *obj;

  obj = [super createBegin: aZone];
  obj->valueMessage = NULL;
  obj->background = 0;
  return obj;
}

PHASE(Using)

- setDiscrete2dToFile: aSpace 
{
  discrete2d = aSpace;

  return self;
}

- setValueMessage: (SEL)aSelector
{
  valueMessage = aSelector;

  return self;
}

- setBackground: (int)aValue
{
  background = aValue;

  return self;
}

- fileTo: (const char *)aFileName
{
  int x, y;
  id *lattice;
  long *offsets;
  int xsize, ysize;
  id outFile;

  if (discrete2d == nil)
    raiseEvent (InvalidArgument,
                "Int2dFiler: attempted to file a (null) space object!\n");

  outFile = [OutFile create: [self getZone] setName: aFileName];

  if (outFile == nil)
    return nil;
  
  lattice = [discrete2d getLattice];
  offsets = [discrete2d getOffsets];
  xsize = [discrete2d getSizeX];
  ysize = [discrete2d getSizeY];

  if (valueMessage)
    {
      for (y = 0; y < ysize; y++)
        {
          for (x = 0; x < xsize; x++)
            {
              id potentialObject;
              
              if (x)
                [outFile putString: " "];
              
              potentialObject = *discrete2dSiteAt (lattice, offsets, x, y);
              if (potentialObject)
                [outFile putLong: (long) [potentialObject perform: valueMessage]];
              else 
                [outFile putInt: background];          
            }
          
          [outFile putNewLine];
        }
    }
  else
    {
      for (y = 0; y < ysize; y++)
        {
          for (x = 0; x < xsize; x++)
            {
              if (x)
                [outFile putString: " "];
              
              [outFile putLong: 
                         (long)*discrete2dSiteAt(lattice, offsets, x, y)];
            }      
          [outFile putNewLine];
        }
    }
  
  [outFile drop];

  return self;
}

@end

