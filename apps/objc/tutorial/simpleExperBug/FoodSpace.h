// FoodSpace.h					simpleExperBug

#import <space.h>
#import <simtools.h>


@interface FoodSpace : Discrete2d {
  int food;
}

-(int)getFood;
-seedFoodWithProb: (float) s;
-bugAte;

@end

