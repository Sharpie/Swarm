// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Loop through a Discrete2d, sending the displayMessage message to
// all objects found in there.  One argument is passed on the message,
// the display widget.

#import <space/Int2dFiler.h>
#import <simtools.h>

//S: Saves the state of a Discrete2d object.
//D: The Int2dFiler class is used to save the state of any Discrete2d
//D: object (or a subclass thereof) to a specified file.
@implementation Int2dFiler

+ createBegin: aZone
{
  Int2dFiler *obj;

  obj = [super createBegin: aZone];
  obj->valueMessage = NULL;
  obj->background = 0;
  return obj;
}

//M: Set the target space to be filled.
//M: This message can be used more than once, but often it is useful
//M: to keep one Int2dFiler per space
//M: (e.g. when the space is saved multiple times). 
- setDiscrete2dToFile: (Discrete2d *)aSpace 
{
  discrete2d = aSpace;
  return self;
}

//M: This message is optional. It is used when the target Discrete2d
//M: contains objects.  By sending each object the message specified by
//M: the selector, the Int2dFiler is able to get from the object an
//M: integer representing its state, which it then writes to the file. 
- setValueMessage: (SEL)aSelector
{
  valueMessage = aSelector;
  return self;
}

//M: This message is optional.  It is used when the target Discrete2d
//M: contains objects.  If a particular location in the space has no
//M: resident object, the argument of this message is the value which
//M: gets writtent to the file. The default background value is 0. 
- setBackground: (int)aValue
{
  background = aValue;
  return self;
}

//M: When the Int2dFiler receives this message, it opens a file called
//M: fileName, stores the state of a pre-specified space into it, and
//M: then closes the file. 
- fileTo: (const char *)aFileName
{
  int x, y;
  id *lattice;
  long *offsets;
  int xsize, ysize;
  id outFile;

  if (discrete2d == nil)
    [InvalidArgument raiseEvent: 
      "Int2dFiler: attempted to file a (null) space object!\n"];

  outFile = [OutFile create: [self getZone] withName: aFileName];
  
  if (!outFile)
    {
      fprintf (stderr,"Warning (Int2DFiler): could not open %s!\n", aFileName);
      return self;
    }
  
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

