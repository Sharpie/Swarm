#import <defobj/Arguments.h>


id
makeProbe (id obj, const char *ivarName);

int
getInt (id obj, const char *ivarName);

double
getDbl (id obj, const char *ivarName);

@interface Parameters: Arguments_c
{
  char * inputFilename;
  int run, experimentDuration;
  int worldXSize, worldYSize;
  int radius;
  int numRaces;
  char * neighborhood_type;
  long currentTime;
  long randomSeed;
  
  BOOL edgeWrap, synchronous, randomize, writeGUIRaster;
  double fractionVacant;
  double fractionBlue, fractionRed;
  double blueToleranceUpper, blueToleranceLower;
  double redToleranceUpper, redToleranceLower;
  double otherToleranceUpper, otherToleranceLower;

}

- init;


- (int)parseKey: (int)key arg: (const char*)arg;


- (void)setCurrentTime: (long)x;
- (timeval_t)getTime;

- (BOOL)toggleRandomizeList;

- (BOOL)toggleEdgeWrap;
- (BOOL)toggleWriteGUIRaster;
- takeScreenshot;

- (char *)getInputFilename;

- (char*)getNeighborhoodType;

- saveParameters: (char*)fn;
  
- loadParameters: (char*)fn;


@end
