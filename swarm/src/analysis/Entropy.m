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

#import <analysis/Entropy.h>
#import <collections.h>
#import <defobj/defalloc.h> // getZone
#include <misc.h> // log

@implementation Entropy

PHASE(Creating)
- setCollection: aCollection
{
  collection = aCollection;
  return self;
}

- createEnd 
{
  if (collection == nil)
    [InvalidCombination raiseEvent: "Entropy created without a collection\n"];

  return [super createEnd];
}

PHASE(Setting)
PHASE(Using)

- (void)update
{
  id iter, obj;
  double maximum;
  int count;

  entropy = 0.0;

  count = [collection getCount];   

  if (!count)
    return;
  
  maximum = log (1.0 / ((double) count));

  obj = [collection getFirst];
  
  iter = [collection begin: getZone (self)];
  while ((obj = [iter next]) != nil)
    {
      double v = [self doubleDynamicCallOn: obj];
      
      if(v > 0.0)
        entropy += v * log(v);
    }
  [iter drop];
  
  entropy /= maximum;
}

- (double)getEntropy
{
  return entropy;
} 

@end
