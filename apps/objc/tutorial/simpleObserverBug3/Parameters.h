#import <defobj/Arguments.h>

id
makeProbe (id obj, const char *ivarName);

int
getInt (id obj, const char *ivarName);



@interface Parameters: Arguments_c
 {
   
     int worldXSize, worldYSize;
     int run, experimentDuration;
     int randomSeed;
     double seedProb, bugDensity;
 }

-init;


- (int)parseKey: (int) key arg: (const char*) arg;

- (double)getSeedProb;

- (double)getBugDensity;

@end




