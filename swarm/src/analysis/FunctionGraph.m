// Swarm library. Copyright © 1998-2000 Swarm Development Group.
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

#import "FunctionGraph.h"
#import <gui.h>

@implementation FunctionGraph

PHASE(Creating)

+ createBegin: aZone
{
  FunctionGraph *newFunctionGraph = [super createBegin: aZone];
  
  newFunctionGraph->minX      = 0.0;
  newFunctionGraph->maxX      = 0.0;
  newFunctionGraph->stepSize  = 0.0;
  
  newFunctionGraph->element      = nil;
  newFunctionGraph->dataFeed     = nil;
  newFunctionGraph->functionSEL  = 0;
  
  newFunctionGraph->arithmeticWarn = NO;
  
  newFunctionGraph->resetFrequency  = 0;
  newFunctionGraph->resetCountDown  = 0;
  
  return newFunctionGraph;
}

- createEnd
{
  static const char *Init_Error = 
    "\t FunctionGraph was not initialized properly.\n";
  
  if (element == nil || dataFeed == nil)
    raiseEvent (InvalidCombination, Init_Error);
  
  if (!functionSEL)
    raiseEvent (InvalidCombination, Init_Error);
  
  return [super createEnd];
}


- setElement: (id <GraphElement>)graphElement
{
  element = graphElement;

  return self;
}

- setDataFeed: feed
{
  dataFeed = feed;

  return self;
}

- setFunctionSelector: (SEL)aSelector
{
  static const char *Set_Error =
    "\t Attempted to set function selector to nil.\n";

  if (!aSelector)
    raiseEvent (InvalidArgument, Set_Error);
  
  functionSEL = aSelector;

  return self;
}

PHASE(Setting)

- setArithmeticWarn: (BOOL)state
{
  arithmeticWarn = state;

  return self;
}

- setXMin: (double)minx Max: (double)maxx Resolution: (unsigned)steps
{
  static const char *Step0_Error =
    "\t Attempted to set number of steps to zero (0) when maxx != minx.\n";
  static const char *StepNeg_Error =
    "\t Attempted to set number of steps to a negative value.\n";

  if (steps == 0 && maxx != minx)
    raiseEvent (InvalidArgument, Step0_Error);
  
  if (steps < 0)
    raiseEvent (InvalidArgument, StepNeg_Error);
  
  if (steps == 0)
    return [self setXMin: minx Max: maxx StepSize: 0];
  
  return [self setXMin: minx Max: maxx
               StepSize: (maxx - minx) / steps];
}

- setXMin: (double)minx Max: (double)maxx StepSize: (double)size
{
  static const char *Max_EQ_Min_Warning =
    "\t maxX == minX.  maxX = %e,  minX = %e.\n";
  static const char *Max_LT_Min_Warning =
    "\t maxX < minX.  maxX = %e,  minX = %e.\n";
  static const char *Size0_Error =
    "\t Attempted to set step size to zero (0) when maxx != minx.\n";
  static const char *SizePos_Error =
    "\t Attempted to set step size to a positive when maxx < minx.\n";
  static const char *SizeNeg_Error =
    "\t Attempted to set step size to a negative when maxx > minx.\n";

  if (maxx == minx)
    raiseEvent (WarningMessage, Max_EQ_Min_Warning, maxx, minx);

  if (maxx < minx)
    raiseEvent (WarningMessage, Max_LT_Min_Warning, maxx, minx);
  
  if (size == 0 && maxx != minx)
    raiseEvent (InvalidArgument, Size0_Error);
  
  if (size < 0 && maxx > minx)
    raiseEvent (InvalidArgument, SizeNeg_Error);
  
  if (size > 0 && maxx < minx)
    raiseEvent (InvalidArgument, SizePos_Error);
  
  minX = minx;
  maxX = maxx;
  stepSize  = size;
  
  return self;
}

- setResetFrequency: (unsigned)freq
{
  resetFrequency = freq;
  resetCountDown = freq;
  
  return self;
}

PHASE(Using)

- (void)graph
{
  static const char *Arithmetic_Warning =
    "\t Function could not be evaluated for value %e.\n";
  double x, y;
  id result = (id)NO;
  
  x = minX;  y = 0.00;
  
  if (resetCountDown > 0)
    resetCountDown--;
  else
    {
      [element resetData];
      resetCountDown = resetFrequency;
    }
  
  x = minX;
  result = [dataFeed perform: functionSEL with: (id)&x with: (id)&y];
  
  if (result == (id)YES)
    [element addX: x Y: y];
  else
    if (arithmeticWarn == YES)
      raiseEvent (WarningMessage, Arithmetic_Warning, x);
  
  for (x = minX + stepSize; x < maxX; x += stepSize)
    {
      result = [dataFeed perform: functionSEL with: (id)&x with: (id)&y];
      
      if (result == (id)YES)
        [element addX: x Y: y];
      else
        if (arithmeticWarn == YES)
          raiseEvent (WarningMessage, Arithmetic_Warning, x);
    }
  
  x = maxX;
  result = [dataFeed perform: functionSEL with: (id)&x with: (id)&y];
  
  if (result == (id)YES)
    [element addX: x Y: y];
  else
    if (arithmeticWarn == YES)
      raiseEvent (WarningMessage, Arithmetic_Warning, x);
}

@end
