// FoodSpace.m					simpleExperBug

#import "FoodSpace.h"

@implementation FoodSpace

-(int)getFood {
  return food;
}


-seedFoodWithProb: (float) seedProb {
  int x,y;
  
  food = 0;

  for (y = 0; y < ysize; y++) 
    for (x = 0; x < xsize; x++) 
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] <= seedProb) {
	[self putValue: 1 atX: x Y: y];
	food++ ;
  }
  return self;
}


-bugAte {
  food-- ;
  return self;
}


@end


