// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugModelSwarm.m"

// Import Swarm libraries:

#import <string.h>

#import <space.h>
#import <activity.h>
#import <collections.h>
#import <swarmobject.h>
#import <simtools.h>

#import "HumbugModelSwarm.h"

// HumbugModelSwarm.m.0: 
// simple test for correct functioning of all objects.
// Usage: './humbug -batchmode > runlog'

// NOTE: doubleDistTest0 even writes state data to disk
// and zeroes the buffer, just to test persistence!

@implementation HumbugModelSwarm

+createBegin: (id) aZone {
  HumbugModelSwarm * obj;

  obj = [super createBegin: aZone];

  obj->debugPrint = 1;

  obj->myStream = [ OutputStream create: aZone setFileStream: stdout ];

  return obj;
}

-createEnd {
  return [super createEnd];
}

-bitDistTest0: (id) myDistribution tVal: (BOOL) tVal detail: (BOOL) d {
  const unsigned startSeed = 43574357;
  int i;
  int stateSizeG, stateSizeD;
  id stateBufG, stateBufD;
  unsigned int uMax;
  id myGenerator;
  unsigned long long int genCount1, genCount2, distCount1, distCount2;
  BOOL bitbucket;
  BOOL testVal1, testVal2;
  unsigned int yesVal, noVal;
  double dgRatio, ynRatio;

//  struct {
//   unsigned magic;
//   unsigned size;
//  } stateDummy;

// 1. create it

  // Distribution has already been created
  // printf("Distribution state at start:\n\n");
  // [ myDistribution describe: myStream ];
  myGenerator = [myDistribution getGenerator];
  uMax = [myGenerator getUnsignedMax];
  yesVal = noVal = 0;

// 2. set state with new seed

  printf("%s / %s: Setting generator state from seed %u\n\n", 
	[myDistribution getName],  [myGenerator getName], startSeed);
  [ myGenerator setStateFromSeed: startSeed ];
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }
  genCount1 = [myGenerator getCurrentCount];
  distCount1 = [myDistribution getCurrentCount];

// 3. Run 10000 steps and print 10

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getBooleanSample];
    if (bitbucket) yesVal++; else noVal++;
  }
  printf("\n");

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getBooleanSample];
    if (bitbucket) yesVal++; else noVal++;
    printf(" variate no. %2i: %d\n",i,bitbucket);
  }
  printf("\n");
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

// 4. extract generator and distribution state

  stateSizeG = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSizeG);

  printf(" Allocating state buffer memory\n");
  stateBufG = [[self getZone] alloc: stateSizeG];
  printf(" State buffer is at %p\n", stateBufG);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBufG];

  stateSizeD = [myDistribution getStateSize];
  printf(" Distribution state size is %d\n", stateSizeD);

  printf(" Allocating state buffer memory\n");
  stateBufD = [[self getZone] alloc: stateSizeD];
  printf(" State buffer is at %p\n", stateBufD);

  printf(" Saving distribution state\n\n");
  [myDistribution putStateInto: (void *) stateBufD];

// 5. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getBooleanSample];
    if (bitbucket) yesVal++; else noVal++;
  }
  printf("\n");

// 6. print 10 steps

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getBooleanSample];
    if (bitbucket) yesVal++; else noVal++;
    printf(" variate no. %2i: %d\n",i,bitbucket);
  }
  printf("\n");

  testVal1 = [myDistribution getBooleanSample];
  printf(" TEST VALUE #1 = %d\n\n", testVal1);

  ynRatio = (double) yesVal / (yesVal+noVal);
  printf(" Yes Fraction = %24.16f (%u YES, %u NO)\n\n", ynRatio, yesVal, noVal);

  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

  genCount2 = [myGenerator getCurrentCount];
  distCount2 = [myDistribution getCurrentCount];
  printf("Number of generator calls made = %24Lu\n", 
    (genCount2-genCount1));
  printf("Number of distribution calls made = %21Lu\n", 
    (distCount2-distCount1));
  dgRatio = (double) (genCount2-genCount1) / (distCount2-distCount1);
  printf("Ratio = %24.16e\n\n", dgRatio); 

// 7. mess up state data

  // save part of the state:
  // memcpy(&stateDummy, stateBuf, sizeof(stateDummy));

  // mess up the state data to test error handling:
  // stateDummy.magic = 33; 	// 
  // stateDummy.size = 33; 	// 
  // memcpy(stateBuf, &stateDummy, sizeof(stateDummy));

// 8. reset state

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBufG];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBufG];

  printf(" Resetting distribution state\n");
  [myDistribution setStateFrom: (void *) stateBufD];

  printf(" Freeing state buffer memory\n\n");
  [[self getZone] free: stateBufD];


// 10. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) 
    bitbucket = [myDistribution getBooleanSample];
  printf("\n");

// 11. print 10 steps

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %d\n",i,[myDistribution getBooleanSample]);
  printf("\n");
  testVal2 = [myDistribution getBooleanSample];

  printf("TEST VALUE #2 = %d\n\n", testVal2);

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = test value #2\n",
	[myDistribution getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n", 
	[myDistribution getName]);

  if (testVal1 == tVal)
   printf("%s Run test: GOOD. Test value = expected final value.\n", 
	[myDistribution getName]);
  else
   printf("%s Run test: NOT GOOD. Test value != expected final value %d !\n", 
    [myDistribution getName], tVal);

  printf("\f\n");

   return self;
}

-integerDistTest0: (id) myDistribution tVal: (int) tVal detail: (BOOL) d {
  const unsigned startSeed = 43574357;
  int i;
  int stateSizeG, stateSizeD;
  id stateBufG, stateBufD;
  unsigned int uMax;
  id myGenerator;
  unsigned long long int genCount1, genCount2, distCount1, distCount2;
  int bitbucket;
  int testVal1, testVal2;
  double dgRatio;

//  struct {
//   unsigned magic;
//   unsigned size;
//  } stateDummy;

// 1. create it

  // Distribution has already been created
  // printf("Distribution state at start:\n\n");
  // [ myDistribution describe: myStream ];
  myGenerator = [myDistribution getGenerator];
  uMax = [myGenerator getUnsignedMax];

// 2. set state with new seed

  printf("%s / %s: Setting generator state from seed %u\n\n", 
	[myDistribution getName],  [myGenerator getName], startSeed);
  [ myGenerator setStateFromSeed: startSeed ];
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }
  genCount1 = [myGenerator getCurrentCount];
  distCount1 = [myDistribution getCurrentCount];


// 3. Run 10000 steps and print 10

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getIntegerSample];
  }
  printf("\n");

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getIntegerSample];
    printf(" variate no. %2i: %d\n",i,bitbucket);
  }
  printf("\n");
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

// 4. extract generator and distribution state

  stateSizeG = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSizeG);

  printf(" Allocating state buffer memory\n");
  stateBufG = [[self getZone] alloc: stateSizeG];
  printf(" State buffer is at %p\n", stateBufG);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBufG];

  stateSizeD = [myDistribution getStateSize];
  printf(" Distribution state size is %d\n", stateSizeD);

  printf(" Allocating state buffer memory\n");
  stateBufD = [[self getZone] alloc: stateSizeD];
  printf(" State buffer is at %p\n", stateBufD);

  printf(" Saving distribution state\n\n");
  [myDistribution putStateInto: (void *) stateBufD];

// 5. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getIntegerSample];
  }
  printf("\n");

// 6. print 10 steps

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getIntegerSample];
    printf(" variate no. %2i: %d\n",i,bitbucket);
  }
  printf("\n");

  testVal1 = [myDistribution getIntegerSample];
  printf(" TEST VALUE #1 = %d\n\n", testVal1);

  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

  genCount2 = [myGenerator getCurrentCount];
  distCount2 = [myDistribution getCurrentCount];
  printf("Number of generator calls made = %24Lu\n", 
    (genCount2-genCount1));
  printf("Number of distribution calls made = %21Lu\n", 
    (distCount2-distCount1));
  dgRatio = (double) (genCount2-genCount1) / (distCount2-distCount1);
  printf("Ratio = %24.16e\n\n", dgRatio); 

// 7. mess up state data

  // save part of the state:
  // memcpy(&stateDummy, stateBuf, sizeof(stateDummy));

  // mess up the state data to test error handling:
  // stateDummy.magic = 33; 	// 
  // stateDummy.size = 33; 	// 
  // memcpy(stateBuf, &stateDummy, sizeof(stateDummy));

// 8. reset state

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBufG];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBufG];

  printf(" Resetting distribution state\n");
  [myDistribution setStateFrom: (void *) stateBufD];

  printf(" Freeing state buffer memory\n\n");
  [[self getZone] free: stateBufD];


// 10. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) 
    bitbucket = [myDistribution getIntegerSample];
  printf("\n");

// 11. print 10 steps

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %d\n",i,[myDistribution getIntegerSample]);
  printf("\n");
  testVal2 = [myDistribution getIntegerSample];

  printf("TEST VALUE #2 = %d\n\n", testVal2);

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = test value #2\n",
	[myDistribution getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n", 
	[myDistribution getName]);

  if (testVal1 == tVal)
   printf("%s Run test: GOOD. Test value = expected final value.\n", 
	[myDistribution getName]);
  else
   printf("%s Run test: NOT GOOD. Test value != expected final value %d !\n", 
    [myDistribution getName], tVal);

  printf("\f\n");

   return self;
}

-unsignedDistTest0: (id) myDistribution tVal: (unsigned) tVal detail: (BOOL) d {
  const unsigned startSeed = 43574357;
  int i;
  int stateSizeG, stateSizeD;
  id stateBufG, stateBufD;
  unsigned int uMax;
  id myGenerator;
  unsigned long long int genCount1, genCount2, distCount1, distCount2;
  unsigned bitbucket;
  unsigned testVal1, testVal2;
  double dgRatio;

//  struct {
//   unsigned magic;
//   unsigned size;
//  } stateDummy;

// 1. create it

  // Distribution has already been created
  // printf("Distribution state at start:\n\n");
  // [ myDistribution describe: myStream ];
  myGenerator = [myDistribution getGenerator];
  uMax = [myGenerator getUnsignedMax];

// 2. set state with new seed

  printf("%s / %s: Setting generator state from seed %u\n\n", 
	[myDistribution getName],  [myGenerator getName], startSeed);
  [ myGenerator setStateFromSeed: startSeed ];
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }
  genCount1 = [myGenerator getCurrentCount];
  distCount1 = [myDistribution getCurrentCount];

// 3. Run 10000 steps and print 10

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getUnsignedSample];
  }
  printf("\n");

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getUnsignedSample];
    printf(" variate no. %2i: %u\n",i,bitbucket);
  }
  printf("\n");
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

// 4. extract generator and distribution state

  stateSizeG = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSizeG);

  printf(" Allocating state buffer memory\n");
  stateBufG = [[self getZone] alloc: stateSizeG];
  printf(" State buffer is at %p\n", stateBufG);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBufG];

  stateSizeD = [myDistribution getStateSize];
  printf(" Distribution state size is %d\n", stateSizeD);

  printf(" Allocating state buffer memory\n");
  stateBufD = [[self getZone] alloc: stateSizeD];
  printf(" State buffer is at %p\n", stateBufD);

  printf(" Saving distribution state\n\n");
  [myDistribution putStateInto: (void *) stateBufD];

// 5. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getUnsignedSample];
  }
  printf("\n");

// 6. print 10 steps

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getUnsignedSample];
    printf(" variate no. %2i: %u\n",i,bitbucket);
  }
  printf("\n");

  testVal1 = [myDistribution getUnsignedSample];
  printf(" TEST VALUE #1 = %u\n\n", testVal1);

  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

  genCount2 = [myGenerator getCurrentCount];
  distCount2 = [myDistribution getCurrentCount];
  printf("Number of generator calls made = %24Lu\n", 
    (genCount2-genCount1));
  printf("Number of distribution calls made = %21Lu\n", 
    (distCount2-distCount1));
  dgRatio = (double) (genCount2-genCount1) / (distCount2-distCount1);
  printf("Ratio = %24.16e\n\n", dgRatio); 

// 7. mess up state data

  // save part of the state:
  // memcpy(&stateDummy, stateBuf, sizeof(stateDummy));

  // mess up the state data to test error handling:
  // stateDummy.magic = 33; 	// 
  // stateDummy.size = 33; 	// 
  // memcpy(stateBuf, &stateDummy, sizeof(stateDummy));

// 8. reset state

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBufG];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBufG];

  printf(" Resetting distribution state\n");
  [myDistribution setStateFrom: (void *) stateBufD];

  printf(" Freeing state buffer memory\n\n");
  [[self getZone] free: stateBufD];


// 10. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) 
    bitbucket = [myDistribution getUnsignedSample];
  printf("\n");

// 11. print 10 steps

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myDistribution getUnsignedSample]);
  printf("\n");
  testVal2 = [myDistribution getUnsignedSample];

  printf("TEST VALUE #2 = %u\n\n", testVal2);

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = test value #2\n",
	[myDistribution getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n", 
	[myDistribution getName]);

  if (testVal1 == tVal)
   printf("%s Run test: GOOD. Test value = expected final value.\n", 
	[myDistribution getName]);
  else
   printf("%s Run test: NOT GOOD. Test value != expected final value %u !\n", 
    [myDistribution getName], tVal);

  printf("\f\n");

   return self;
}

-doubleDistTest0: (id) myDistribution tVal: (double) tVal detail: (BOOL) d {
  const unsigned startSeed = 43574357;
  int i;
  int stateSizeG, stateSizeD;
  id stateBufG, stateBufD;
  unsigned int uMax;
  id myGenerator;
  unsigned long long int genCount1, genCount2, distCount1, distCount2;
  double bitbucket;
  double testVal1, testVal2;
  double dgRatio;
  FILE *myFile;
  int status;

//  struct {
//   unsigned magic;
//   unsigned size;
//  } stateDummy;

// 1. create it

  // Distribution has already been created
  // printf("Distribution state at start:\n\n");
  // [ myDistribution describe: myStream ];
  myGenerator = [myDistribution getGenerator];
  uMax = [myGenerator getUnsignedMax];

// 2. set state with new seed

  printf("%s / %s: Setting generator state from seed %u\n\n", 
	[myDistribution getName],  [myGenerator getName], startSeed);
  [ myGenerator setStateFromSeed: startSeed ];
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }
  genCount1 = [myGenerator getCurrentCount];
  distCount1 = [myDistribution getCurrentCount];

// 3. Run 10000 steps and print 10

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getDoubleSample];
  }
  printf("\n");

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getDoubleSample];
    printf(" variate no. %2i: %f\n",i,bitbucket);
  }
  printf("\n");
  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

// 4. extract generator and distribution state

  stateSizeG = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSizeG);

  printf(" Allocating state buffer memory\n");
  stateBufG = [[self getZone] alloc: stateSizeG];
  printf(" State buffer is at %p\n", stateBufG);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBufG];

  myFile = fopen("MyGenData.bin","w");
  if (myFile == NULL) {}; // error on open, likely disk full or no permission
  status = fwrite(stateBufG, stateSizeG, 1, myFile);
  if (status < 1) {}; // error on write
  status = fclose(myFile);
  if (status) {}; // error on close ?

  memset(stateBufG, 0, stateSizeG);

  stateSizeD = [myDistribution getStateSize];
  printf(" Distribution state size is %d\n", stateSizeD);

  printf(" Allocating state buffer memory\n");
  stateBufD = [[self getZone] alloc: stateSizeD];
  printf(" State buffer is at %p\n", stateBufD);

  printf(" Saving distribution state\n\n");
  [myDistribution putStateInto: (void *) stateBufD];

  myFile = fopen("MyDistData.bin","w");
  if (myFile == NULL) {}; // error on open, likely disk full or no permission
  status = fwrite(stateBufD, stateSizeD, 1, myFile);
  if (status < 1) {}; // error on write
  status = fclose(myFile);
  if (status) {}; // error on close ?

  memset(stateBufD, 0, stateSizeD);

// 5. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myDistribution getDoubleSample];
  }
  printf("\n");

// 6. print 10 steps

  for (i = 0; i < 10; i++) {
    bitbucket = [myDistribution getDoubleSample];
    printf(" variate no. %2i: %f\n",i,bitbucket);
  }
  printf("\n");

  testVal1 = [myDistribution getDoubleSample];
  printf(" TEST VALUE #1 = %24.16e\n\n", testVal1);

  if (d) { 
    [ myDistribution describe: myStream ];
    [ myGenerator describe: myStream ];
  }

  genCount2 = [myGenerator getCurrentCount];
  distCount2 = [myDistribution getCurrentCount];
  printf("Number of generator calls made = %24Lu\n", 
    (genCount2-genCount1));
  printf("Number of distribution calls made = %21Lu\n", 
    (distCount2-distCount1));
  dgRatio = (double) (genCount2-genCount1) / (distCount2-distCount1);
  printf("Ratio = %24.16e\n\n", dgRatio); 

// 7. mess up state data

  // save part of the state:
  // memcpy(&stateDummy, stateBuf, sizeof(stateDummy));

  // mess up the state data to test error handling:
  // stateDummy.magic = 33; 	// 
  // stateDummy.size = 33; 	// 
  // memcpy(stateBuf, &stateDummy, sizeof(stateDummy));

// 8. reset state

  myFile = fopen("MyGenData.bin", "r");
  if (myFile == NULL) {}; // error; file not found
  status = fread(stateBufG, stateSizeG, 1, myFile);
  if (status < 1) {}; // error
  status = fclose(myFile);
  if (status) {}; // error on close ?

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBufG];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBufG];

  myFile = fopen("MyDistData.bin", "r");
  if (myFile == NULL) {}; // error; file not found
  status = fread(stateBufD, stateSizeD, 1, myFile);
  if (status < 1) {}; // error
  status = fclose(myFile);
  if (status) {}; // error on close ?

  printf(" Resetting distribution state\n");
  [myDistribution setStateFrom: (void *) stateBufD];

  printf(" Freeing state buffer memory\n\n");
  [[self getZone] free: stateBufD];


// 10. run for 10000 steps

  printf(" Run distribution for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) 
    bitbucket = [myDistribution getDoubleSample];
  printf("\n");

// 11. print 10 steps

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %f\n",i,[myDistribution getDoubleSample]);
  printf("\n");
  testVal2 = [myDistribution getDoubleSample];

  printf("TEST VALUE #2 = %24.16e\n\n", testVal2);

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = test value #2\n",
	[myDistribution getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n", 
	[myDistribution getName]);

  if (testVal1 == tVal)
   printf("%s Run test: GOOD. Test value = expected final value.\n", 
	[myDistribution getName]);
  else
   printf("%s Run test: NOT GOOD. Test value != expected final value %f !\n", 
    [myDistribution getName], tVal);

  printf("\f\n");

   return self;
}

-generatorTest0split: (id) myGenerator tVal: (unsigned) tVal detail: (BOOL) d {
  const unsigned int startSeed = 43574357;
  int i;
  int stateSize;
  id stateBuf;
  unsigned bitbucket;
  unsigned testVal1, testVal2;


// 1. Create it

  // Generator has already been created
  // printf("Generator state at start:\n\n");
  // [myGenerator describe: myStream];

// 2. Set state with new seed:

printf("%s Setting state from seed %u\n\n", [myGenerator getName], startSeed);
  [myGenerator setStateFromSeed: startSeed];
  if (d) [myGenerator describe: myStream];

// 3. Run 10000 steps and print 10

   printf("Run generator(s) for 10000 steps and print 10\n\n");

   printf("vGen 0 segment 0:\n\n");

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:0];

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:0]);
  printf("\n");

   printf("vGen 1 segment 1:\n\n");

  [myGenerator advanceGenerator: 1];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:1];

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:1]);
  printf("\n");

   printf("vGen 31 segment 2:\n\n");

  [myGenerator advanceGenerator: 31];
  [myGenerator advanceGenerator: 31];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:31];

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:31]);
  printf("\n");

  if (d) [ myGenerator describe: myStream ];

// 4. extract generator state

  stateSize = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSize);

  printf(" Allocating state buffer memory\n");
  stateBuf = [[self getZone] alloc: stateSize];
  printf(" State buffer is at %p\n", stateBuf);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBuf];

// 5. run generator for 10000 steps

  printf(" Run generator(s) for 10000 steps and print 10\n\n");

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:0];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:1];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:31];

  // [myGenerator describe: myStream];

// 6. print 10 steps

   printf("vGen 0 segment 0:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:0]);
  printf("\n");

   printf("vGen 1 segment 1:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:1]);
  printf("\n");

   printf("vGen 31 segment 2:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:31]);
  printf("\n");

  testVal1 = [myGenerator getUnsignedSample:31];
  printf("TEST VALUE #1 = %u\n\n", testVal1);

  if (d) [ myGenerator describe: myStream ];

// 8. reset generator state

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBuf];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBuf];
  // printf(" State buffer is now %p\n\n", stateBuf);

  // [myGenerator describe: myStream];

// 9. run generator for 10000 steps

  printf(" Run generator(s) for 10000 steps and print 10\n\n");

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:0];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:1];

  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample:31];

  // [myGenerator describe: myStream];

// 10. print 10 steps

   printf("vGen 0 segment 0:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:0]);
  printf("\n");

   printf("vGen 1 segment 1:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:1]);
  printf("\n");

   printf("vGen 31 segment 2:\n\n");

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample:31]);
  printf("\n");

  testVal2 = [myGenerator getUnsignedSample:31];
  printf("TEST VALUE #2 = %u\n\n", testVal2);

  // [ myGenerator describe: myStream ];

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = Test value #2\n\n",
	[myGenerator getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n\n",
	[myGenerator getName]);

  if (testVal1 == tVal)
    printf("%s Run test: GOOD! Test value = expected final value.\n\n",
	[myGenerator getName]);
  else
    printf("%s Run test: NOT GOOD. Test value != expected final value %u\n\n",
	[myGenerator getName], tVal);

  printf("\f\n");

  return self;
}


-generatorTest0simple: (id) myGenerator tVal: (unsigned) tVal detail: (BOOL) d {
  const unsigned startSeed = 43574357;
  int i;
  int stateSize;
  id stateBuf;
  unsigned bitbucket;
  unsigned testVal1, testVal2;
  unsigned int uMax;

//  struct {
//   unsigned magic;
//   unsigned size;
//  } stateDummy;

// 1. create it

  // Generator has already been created
  // printf("Generator state at start:\n\n");
  // [ myGenerator describe: myStream ];
  uMax = [myGenerator getUnsignedMax];

// 2. set state with new seed

  printf("%s Setting state from seed %u\n\n", [myGenerator getName], startSeed);
  [ myGenerator setStateFromSeed: startSeed ];
  if (d) [ myGenerator describe: myStream ];

// 3. Run 10000 steps and print 10

  printf(" Run generator for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myGenerator getUnsignedSample];
    if (bitbucket > uMax) 
      printf("WARNING! Value greater than uMax recorded! %u > %u\n\n",
      bitbucket, uMax);
  }
  printf("\n");
  // [myGenerator describe: myStream];

  for (i = 0; i < 10; i++) {
    bitbucket = [myGenerator getUnsignedSample];
    printf(" variate no. %2i: %u\n",i,bitbucket);
    if (bitbucket > uMax) 
      printf("WARNING! Value greater than uMax recorded! %u > %u\n\n",
      bitbucket, uMax);
  }
  printf("\n");
  if (d) [ myGenerator describe: myStream ];

// 4. extract generator state

  stateSize = [myGenerator getStateSize];
  printf(" Generator state size is %d\n", stateSize);

  printf(" Allocating state buffer memory\n");
  stateBuf = [[self getZone] alloc: stateSize];
  printf(" State buffer is at %p\n", stateBuf);

  printf(" Saving generator state\n\n");
  [myGenerator putStateInto: (void *) stateBuf];

// 5. run generator for 10000 steps

  printf(" Run generator for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) {
    bitbucket = [myGenerator getUnsignedSample];
    if (bitbucket > uMax) 
      printf("WARNING! Value greater than uMax recorded! %u > %u\n\n",
      bitbucket, uMax);
  }
  printf("\n");
  // [myGenerator describe: myStream];

// 6. print 10 steps

  for (i = 0; i < 10; i++) {
    bitbucket = [myGenerator getUnsignedSample];
    printf(" variate no. %2i: %u\n",i,bitbucket);
    if (bitbucket > uMax) 
      printf("WARNING! Value greater than uMax recorded! %u > %u\n\n",
      bitbucket, uMax);
  }
  printf("\n");

  testVal1 = [myGenerator getUnsignedSample];
  printf(" TEST VALUE #1 = %u\n\n", testVal1);

  if (d) [ myGenerator describe: myStream ];

// 7. mess up state data

  // save part of the state:
  // memcpy(&stateDummy, stateBuf, sizeof(stateDummy));

  // mess up the state data to test error handling:
  // stateDummy.magic = 33; 	// 
  // stateDummy.size = 33; 	// 
  // memcpy(stateBuf, &stateDummy, sizeof(stateDummy));

// 8. reset generator state

  printf(" Resetting generator state\n");
  [myGenerator setStateFrom: (void *) stateBuf];

  printf(" Freeing state buffer memory\n");
  [[self getZone] free: stateBuf];
  // printf(" State buffer is now %p\n\n", stateBuf);

  // [myGenerator describe: myStream];

// 10. run generator for 10000 steps

  printf(" Run generator for 10000 steps and print 10\n");
  for (i = 0; i < 10000; i++) 
    bitbucket = [myGenerator getUnsignedSample];
  printf("\n");
  // [myGenerator describe: myStream];

// 11. print 10 steps

  for (i = 0; i < 10; i++) 
    printf(" variate no. %2i: %u\n",i,[myGenerator getUnsignedSample]);
  printf("\n");
  testVal2 = [myGenerator getUnsignedSample];
  // [ myGenerator describe: myStream ];

  printf("TEST VALUE #2 = %u\n\n", testVal2);

  if (testVal2 == testVal1)
    printf("%s getState/setState test: GOOD! Test value #1 = test value #2\n\n",
	[myGenerator getName]);
  else
    printf("%s getState/setState test: NOT GOOD. Test values not equal!\n\n", 
	[myGenerator getName]);

  if (testVal1 == tVal)
   printf("%s Run test: GOOD. Test value = expected final value.\n\n", 
	[myGenerator getName]);
  else
   printf("%s Run test: NOT GOOD. Test value != expected final value %u !\n\n", 
    [myGenerator getName], tVal);

  printf("\f\n");

  return self;
}


-buildObjects {
  unsigned int mySeeds[1024];
 
  [super buildObjects];

//  printf(" Create generators: \n");

  mySeeds[0] = DEFAULTSEED1;
  mySeeds[1] = DEFAULTSEED2;
  mySeeds[2] = DEFAULTSEED3;
  mySeeds[3] = DEFAULTSEED4;

// SIMPLE generators:

/*
*/
// -----
  myGen = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG1gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG1gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG1gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 286811723u detail: NO];
// -----
  myGen = [PMMLCG2gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG2gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG2gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG2gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 2029920521u detail: NO];
// -----
  myGen = [PMMLCG3gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG3gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG3gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG3gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 248601296u detail: NO];
// -----
  myGen = [PMMLCG4gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG4gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG4gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG4gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1831578876u detail: NO];
// -----
  myGen = [PMMLCG5gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG5gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG5gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG5gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1979055038u detail: NO];
// -----
  myGen = [PMMLCG6gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG6gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG6gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG6gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1081682882u detail: NO];
// -----
  myGen = [PMMLCG7gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG7gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG7gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG7gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1526562528u detail: NO];
// -----
  myGen = [PMMLCG8gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG8gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG8gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG8gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1317529039u detail: NO];
// -----
  myGen = [PMMLCG9gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PMMLCG9gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PMMLCG9gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PMMLCG9gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1940194316u detail: NO];
// -----
  myGen = [LCG1gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [LCG1gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [LCG1gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [LCG1gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 170400212u detail: NO];
// -----
  myGen = [LCG2gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [LCG2gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [LCG2gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [LCG2gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 3119667220u detail: NO];
// -----
  myGen = [LCG3gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [LCG3gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [LCG3gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [LCG3gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1870216202u detail: NO];
// -----
  myGen = [ACGgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [ACGgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [ACGgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [ACGgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 3435964474u detail: NO];
// -----
  myGen = [SCGgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [SCGgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [SCGgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [SCGgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 20934893u detail: NO];
// -----
  myGen = [SWB1gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [SWB1gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [SWB1gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [SWB1gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1975162462u detail: NO];
// -----
  myGen = [SWB2gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [SWB2gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [SWB2gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [SWB2gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 2765495939u detail: NO];
// -----
  myGen = [SWB3gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [SWB3gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [SWB3gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [SWB3gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1908002660u detail: NO];
// -----
  myGen = [PSWBgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [PSWBgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [PSWBgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [PSWBgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 4092569303u detail: NO];
// -----
  myGen = [MRG5gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MRG5gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MRG5gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MRG5gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 140295182u detail: NO];
// -----
  myGen = [MRG6gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MRG6gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MRG6gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MRG6gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 727178510u detail: NO];
// -----
  myGen = [MRG7gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MRG7gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MRG7gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MRG7gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1619848208u detail: NO];
// -----
  myGen = [C2MRG3gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [C2MRG3gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [C2MRG3gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [C2MRG3gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1949646119u detail: NO];
// -----
  myGen = [C2TAUS1gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [C2TAUS1gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [C2TAUS1gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [C2TAUS1gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1050071094u detail: NO];
// -----
  myGen = [C2TAUS2gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [C2TAUS2gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [C2TAUS2gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [C2TAUS2gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 963535163u detail: NO];
// -----
  myGen = [C2TAUS3gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [C2TAUS3gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [C2TAUS3gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [C2TAUS3gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 2126750990u detail: NO];
// -----
  myGen = [TT403gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [TT403gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [TT403gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [TT403gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 76792795u detail: NO];
// -----
  myGen = [TT775gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [TT775gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [TT775gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [TT775gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1506106342u detail: NO];
// -----
  myGen = [TT800gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [TT800gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [TT800gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [TT800gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 4036156023u detail: NO];
// -----
  myGen = [MT19937gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MT19937gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MT19937gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MT19937gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 3282989578u detail: NO];
// -----
  myGen = [MWCAgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MWCAgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MWCAgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MWCAgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 2870705913u detail: NO];
// -----
  myGen = [MWCBgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [MWCBgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [MWCBgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [MWCBgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 2169143033u detail: NO];
// -----
  myGen = [C3MWCgen create: [self getZone] setStateFromSeed: 1];
  // myGen = [C3MWCgen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [C3MWCgen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [C3MWCgen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 1332294946u detail: NO];
// -----
  myGen = [RWC2gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [RWC2gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [RWC2gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [RWC2gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 4265961988u detail: NO];
// -----
  myGen = [RWC8gen create: [self getZone] setStateFromSeed: 1];
  // myGen = [RWC8gen create: [self getZone] setStateFromSeed: RANDOMSEED];
  // myGen = [RWC8gen create: [self getZone] setStateFromSeeds: mySeeds];
  // myGen = [RWC8gen createWithDefaults: [self getZone]];
  [self generatorTest0simple: myGen tVal: 498591737u detail: NO];
// -----
/*
*/

// SPLIT generators:

/*
*/
// -----
  myGen = [C2LCGXgen create: [self getZone] 
		setA: 32 setv: 20 setw: 30
		setStateFromSeed: 1];
  // myGen = [C2LCGXgen create: [self getZone] 
  // 		setA: 32 setv: 20 setw: 30
  // 		setStateFromSeed: RANDOMSEED];
  // myGen = [C2LCGXgen create: [self getZone] 
  // 		setA: 32 setv: 20 setw: 30
  // 		setStateFromSeeds: mySeeds];
  // myGen = [C2LCGXgen createWithDefaults: [self getZone]];

  [self generatorTest0split: myGen tVal: 386822811u detail: NO];
// -----
  myGen = [C4LCGXgen create: [self getZone] 
		setA: 128 setv: 31 setw: 41
		setStateFromSeed: 1];
  // myGen = [C4LCGXgen create: [self getZone] 
  // 		setA: 128 setv: 31 setw: 41
  // 		setStateFromSeed: RANDOMSEED];
  // myGen = [C4LCGXgen create: [self getZone] 
  // 		setA: 128 setv: 31 setw: 41
  // 		setStateFromSeeds: mySeeds];
  // myGen = [C4LCGXgen createWithDefaults: [self getZone]];

  [self generatorTest0split: myGen tVal: 147683006u detail: NO];
// -----

   mySimpleGen = [PMMLCG1gen createWithDefaults: [self getZone]];
   mySplitGen  = [C4LCGXgen  createWithDefaults: [self getZone]];

// DISTRIBUTIONS:

/*
*/
// -----
  // myDist = [RandomBitdist createWithDefaults: [self getZone]];
  myDist = [RandomBitDist create: [self getZone] setGenerator: mySimpleGen];
  // myDist = [RandomBitDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];

  [self bitDistTest0: myDist tVal: 1 detail: NO];
// -----
  // myDist = [BernoulliDist createWithDefaults: [self getZone]];
  // myDist = [BernoulliDist create: [self getZone] setGenerator: mySimpleGen];
  myDist = [BernoulliDist create: [self getZone] setGenerator: mySimpleGen
		setProbability: 0.67];
  // myDist = [BernoulliDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [BernoulliDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setProbability: 0.67];

  [self bitDistTest0: myDist tVal: 1 detail: NO];
// -----
  // myDist = [UniformIntegerDist createWithDefaults: [self getZone]];
  // myDist = [UniformIntegerDist create: [self getZone] 
  // 		setGenerator: mySimpleGen];
  myDist = [UniformIntegerDist create: [self getZone] 
		setGenerator: mySimpleGen
		setIntegerMin: -33 setMax: 78 ];
  // myDist = [UniformIntegerDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [UniformIntegerDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setIntegerMin: -33 setMax: 78 ];

  [self integerDistTest0: myDist tVal: 74 detail: NO];
// -----
  // myDist = [UniformUnsignedDist createWithDefaults: [self getZone]];
  // myDist = [UniformUnsignedDist create: [self getZone] 
  // 		setGenerator: mySimpleGen];
  myDist = [UniformUnsignedDist create: [self getZone] 
		setGenerator: mySimpleGen
		setUnsignedMin: 28 setMax: 61 ];
  // myDist = [UniformUnsignedDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [UniformUnsignedDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setUnsignedMin: 28 setMax: 61 ];

  [self unsignedDistTest0: myDist tVal: 59 detail: NO];
// -----
  // myDist = [UniformDoubleDist createWithDefaults: [self getZone]];
  // myDist = [UniformDoubleDist create: [self getZone] 
  // 		setGenerator: mySimpleGen];
  myDist = [UniformDoubleDist create: [self getZone] 
		setGenerator: mySimpleGen
		setDoubleMin: 3.14159 setMax: 2.81218 ];
  // myDist = [UniformDoubleDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [UniformDoubleDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setDoubleMin: 3.14159 setMax: 2.81218 ];

#ifdef USETHINDOUBLES
  [self doubleDistTest0: myDist tVal: 2.8561750496710001 detail: NO];
#else
  [self doubleDistTest0: myDist tVal: 3.0697950221718782 detail: NO];
#endif
// -----
  // myDist = [NormalDist createWithDefaults: [self getZone]];
  // myDist = [NormalDist create: [self getZone] setGenerator: mySimpleGen];
  myDist = [NormalDist create: [self getZone] setGenerator: mySimpleGen
		setMean: 3.0 setVariance: 1.5 ];
  // myDist = [NormalDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [NormalDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setMean: 3.0 setVariance: 1.5];

#ifdef USETHINDOUBLES
  [self doubleDistTest0: myDist tVal: 0.43567276456533938 detail: NO];
#else
  [self doubleDistTest0: myDist tVal: 2.7164700054052444 detail: NO];
#endif
// -----
  // myDist = [LogNormalDist createWithDefaults: [self getZone]];
  // myDist = [LogNormalDist create: [self getZone] setGenerator: mySimpleGen];
  myDist = [LogNormalDist create: [self getZone] setGenerator: mySimpleGen
		setMean: 3.0 setVariance: 1.5 ];
  // myDist = [LogNormalDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [LogNormalDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setMean: 3.0 setVariance: 1.5];

#ifdef USETHINDOUBLES
  [self doubleDistTest0: myDist tVal: 1.5460028050651136 detail: NO];
#else
  [self doubleDistTest0: myDist tVal: 15.126830258240883 detail: NO];
#endif
// -----
  // myDist = [ExponentialDist createWithDefaults: [self getZone]];
// myDist = [ExponentialDist create: [self getZone] setGenerator: mySimpleGen];
  myDist = [ExponentialDist create: [self getZone] setGenerator: mySimpleGen
		setMean: 3.0 ];
  // myDist = [ExponentialDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [ExponentialDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setMean: 3.0 ];

#ifdef USETHINDOUBLES
  [self doubleDistTest0: myDist tVal: 6.0396781661948173 detail: NO];
#else
  [self doubleDistTest0: myDist tVal: 0.73751060128591206 detail: NO];
#endif
// -----
  // myDist = [GammaDist createWithDefaults: [self getZone]];
  // myDist = [GammaDist create: [self getZone] setGenerator: mySimpleGen];
  myDist = [GammaDist create: [self getZone] setGenerator: mySimpleGen
		setAlpha: 3.0 setBeta: 1.5 ];
  // myDist = [GammaDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8];
  // myDist = [GammaDist create: [self getZone]
  // 		setGenerator: mySplitGen setVirtualGenerator: 8
  // 		setAlpha: 3.0 setBeta: 1.5 ];

#ifdef USETHINDOUBLES
  [self doubleDistTest0: myDist tVal: 1.6318177957941833 detail: NO];
#else
  [self doubleDistTest0: myDist tVal: 1.3252529596279012 detail: NO];
#endif
// -----
/*
*/

  return self;
}

-buildActions {

  [super buildActions];

  return self;
}

-activateIn: (id) swarmContext {

  [super activateIn: swarmContext];

  return [self getSwarmActivity];
}

@end
