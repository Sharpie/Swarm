// FoodSpace.h					simpleExperBug

#import <space.h>

@interface FoodSpace : Discrete2d 
{
  int food;
}

- (int)getFood;
- seedFoodWithProb: (float)s;
- bugAte;

@end

