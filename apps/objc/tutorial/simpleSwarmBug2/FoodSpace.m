// FoodSpace.m

#import "FoodSpace.h"
#import <random.h>

@implementation FoodSpace

- seedFoodWithProb: (float)seedProb
{
  int x, y;
  
  for (y = 0; y < ysize; y++) 
    for (x = 0; x < xsize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < seedProb) 
	[self putValue: 1 atX: x Y: y];
  
  return self;
}

@end


