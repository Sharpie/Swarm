#import <defobj/Arguments.h>

id
makeProbe (id obj, const char *ivarName);

int
getInt (id obj, const char *ivarName);



@interface Parameters: Arguments_c
 {
   
     int worldXSize, worldYSize;
     int run, experimentDuration;
     double seedProb, bugDensity;
     long currentTime;
 }

-init;


- (int)parseKey: (int) key arg: (const char*) arg;

- (double)getSeedProb;

- (double)getBugDensity;

- (void)setCurrentTime: (long)x;

- (timeval_t)getCurrentTime;

@end




