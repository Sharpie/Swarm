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

#import <simtools.h>
#import <simtools/NSelect.h>
#import <collections.h> // getCount, addLast
#import <defobj.h> // InvalidArgument
#import <random.h>

@implementation NSelect
PHASE(Creating)

PHASE(Using)

+ (void)select: (int)n from: aCollection into: bCollection
{
  id a;
  int N; // total number of items in aCollection
  int t; // items seen
  int m; // items selected
  float r;

  if (!n)
    return;

  t = m = 0;

  N = [aCollection getCount];

  if (N < n)
    {
      raiseEvent (InvalidArgument,
                  "NSelect: attempted to select %d elements from a collection containing only %d elements.\n",
                  n, N);
    }
  
  a = [aCollection begin: scratchZone];
  
  while (m < n)
    {
      r = (float)[uniformDblRand getDoubleWithMin:0 withMax: 1.0];    
      
      if ((((float)(N - t)) * r) >= ((float)(n - m)))
        [a next];
      else
        {
          m++;
          [bCollection addLast: [a next]];
        }
      t++;
    }
  
  [a drop];
}

@end
