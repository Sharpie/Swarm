// FoodSpace.h					simpleExperBug

#import <space/Discrete2d.h>

@interface FoodSpace: Discrete2d 
{
  int food;
}

- (int)getFood;
- seedFoodWithProb: (float)s;
- bugAte;

@end

