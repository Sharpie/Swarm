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

#import <analysis/ActiveOutFile.h>
#import <simtools/OutFile.h>

@implementation ActiveOutFile
PHASE(Creating)

- setFileObject: aFileObj
{
  file = aFileObj;
  return self;
}

- setHDF5Dataset: (id <HDF5>)hdf5Obj
{
  hdf5Dataset = hdf5Obj;
  return self;
}

- setDataFeed: d
{
  dataFeed = d;
  return self;
}

- createEnd
{
  if (!(file || hdf5Dataset) || !dataFeed)
    [InvalidCombination raiseEvent: "ActiveOutFile not initialized properly"];
  [self setProbedClass: getClass (dataFeed)];
  [super createEnd];
  return self;
}

PHASE(Setting)
PHASE(Using)

- (void)step
{
  currentValue = [self doubleDynamicCallOn: dataFeed];
  if (file)
    {
      [file putDouble: currentValue];
      [file putNewLine];
    }
  else
    [hdf5Dataset addDoubleToVector: currentValue];
}

- (double)getCurrentValue
{
  return currentValue;
}

- (void)drop
{
  if (file)
    [file drop];
  [super drop];
}

@end
