// By Sven Thommesen <sthomme@humsci.auburn.edu>
// "HumbugModelSwarm.m"

// Import Swarm libraries:

#import <sys/time.h>
#import <unistd.h>

#import <space.h>
#import <activity.h>
#import <collections.h>
#import <objectbase.h>
#import <simtools.h>

#import "HumbugModelSwarm.h"

// HumbugModelSwarm.m.4
// -----------------------------
// Hit generator or distribution
// 10 million times
// for timing purposes:
// -----------------------------

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

-bitDistTest4: (id) myDistribution {
  int i,j;
  BOOL bitbucket;
  struct timeval before, after;
  double myTime;
  long long int earlyCount, lateCount;
  double relTime, relSpeed;
  unsigned genCalls;
  double gensPerDist;
  double netDist;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

  [ [myDistribution getGenerator ] setStateFromSeed: 1 ];

  earlyCount = [ [myDistribution getGenerator] getCurrentCount ];
  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      bitbucket      = [ myDistribution getBooleanSample  ];
  } }

  gettimeofday(&after, NULL);
  lateCount = [ [myDistribution getGenerator] getCurrentCount ];

  if (after.tv_usec >= before.tv_usec) {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
   } else {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;
   }

  relTime = myTime / referenceTimeU;
  relSpeed = 1.0 / relTime;

  genCalls = (unsigned) (lateCount - earlyCount);
  gensPerDist = (double) genCalls / 10000000;
  netDist = myTime - gensPerDist * referenceTimeU;

  printf("%20s%10s %12.6f  %8.3f  %6.3f  %6.3f  %8.3f\n",
	[ myDistribution getName ], 
	[ [myDistribution getGenerator] getName ],
	myTime, relTime, relSpeed, gensPerDist, netDist);

  return self;
}


-intDistTest4: (id) myDistribution {
  int i,j;
  int bitbucket;
  struct timeval before, after;
  double myTime;
  long long int earlyCount, lateCount;
  double relTime, relSpeed;
  unsigned genCalls;
  double gensPerDist;
  double netDist;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

  [ [myDistribution getGenerator ] setStateFromSeed: 1 ];

  earlyCount = [ [myDistribution getGenerator] getCurrentCount ];
  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      bitbucket      = [ myDistribution getIntegerSample  ];
  } }

  gettimeofday(&after, NULL);
  lateCount = [ [myDistribution getGenerator] getCurrentCount ];

  if (after.tv_usec >= before.tv_usec) {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
   } else {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;
   }

  relTime = myTime / referenceTimeU;
  relSpeed = 1.0 / relTime;

  genCalls = (unsigned) (lateCount - earlyCount);
  gensPerDist = (double) genCalls / 10000000;
  netDist = myTime - gensPerDist * referenceTimeU;

  printf("%20s%10s %12.6f  %8.3f  %6.3f  %6.3f  %8.3f\n",
	[ myDistribution getName ], 
	[ [myDistribution getGenerator] getName ],
	myTime, relTime, relSpeed, gensPerDist, netDist);

  return self;
}


-unsDistTest4: (id) myDistribution {
  int i,j;
  unsigned bitbucket;
  struct timeval before, after;
  double myTime;
  long long int earlyCount, lateCount;
  double relTime, relSpeed;
  unsigned genCalls;
  double gensPerDist;
  double netDist;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

  [ [myDistribution getGenerator ] setStateFromSeed: 1 ];

  earlyCount = [ [myDistribution getGenerator] getCurrentCount ];
  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      bitbucket      = [ myDistribution getUnsignedSample  ];
  } }

  gettimeofday(&after, NULL);
  lateCount = [ [myDistribution getGenerator] getCurrentCount ];

  if (after.tv_usec >= before.tv_usec) {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
   } else {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;
   }

  relTime = myTime / referenceTimeU;
  relSpeed = 1.0 / relTime;

  genCalls = (unsigned) (lateCount - earlyCount);
  gensPerDist = (double) genCalls / 10000000;
  netDist = myTime - gensPerDist * referenceTimeU;

  printf("%20s%10s %12.6f  %8.3f  %6.3f  %6.3f  %8.3f\n",
	[ myDistribution getName ], 
	[ [myDistribution getGenerator] getName ],
	myTime, relTime, relSpeed, gensPerDist, netDist);

  return self;
}


-dblDistTest4: (id) myDistribution {
  int i,j;
  double bitbucket;
  struct timeval before, after;
  double myTime;
  long long int earlyCount, lateCount;
  double relTime, relSpeed;
  unsigned genCalls;
  double gensPerDist;
  double netDist;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

  [ [myDistribution getGenerator ] setStateFromSeed: 1 ];

  earlyCount = [ [myDistribution getGenerator] getCurrentCount ];
  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      bitbucket      = [ myDistribution getDoubleSample  ];
  } }

  gettimeofday(&after, NULL);
  lateCount = [ [myDistribution getGenerator] getCurrentCount ];

  if (after.tv_usec >= before.tv_usec) {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
   } else {
     myTime =  (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;
   }

  relTime = myTime / referenceTimeU;
  relSpeed = 1.0 / relTime;

  genCalls = (unsigned) (lateCount - earlyCount);
  gensPerDist = (double) genCalls / 10000000;
  netDist = myTime - gensPerDist * referenceTimeU;

  printf("%20s%10s %12.6f  %8.3f  %6.3f  %6.3f  %8.3f\n",
	[ myDistribution getName ], 
	[ [myDistribution getGenerator] getName ],
	myTime, relTime, relSpeed, gensPerDist, netDist);

  return self;
}



-splitGenTest4: (id) myGenerator {
  int i,j;
  unsigned unsignedbucket;
  double thindoublebucket, doublebucket;
  long double ldoublebucket;
  struct timeval before, after;
  double unsT, thinDoubleT, doubleT, longDoubleT;
  double relTime, relSpeed;

  // const int j7 = 10000000;
  const int j6 =  1000000;
  // const int j5 =   100000;

// ----- step 1: unsigned ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;

// ----- step 3: thin doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      thindoublebucket =  [ myGenerator getThinDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 4: fat doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      doublebucket =  [ myGenerator getDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 5: long doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      ldoublebucket    =  [ myGenerator getLongDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


   relTime = unsT / referenceTimeU;
   relSpeed = 1.0 / relTime;

// ----- print the data ----------

  printf("%12s %12.6f %12.6f %12.6f %12.6f %7.3f %6.3f\n",
	[myGenerator getName],
	unsT, thinDoubleT, doubleT, longDoubleT, relTime, relSpeed);

  return self;
}


-splitGenReferenceTest4: (id) myGenerator {
  int i,j;
  unsigned unsignedbucket;
  double thindoublebucket, doublebucket;
  long double ldoublebucket;
  struct timeval before, after;
  double unsT, thinDoubleT, doubleT, longDoubleT;

  // const int j7 = 10000000;
  const int j6 =  1000000;
  // const int j5 =   100000;

// ----- step 1: unsigned ----------

  // First run the generator awhile to quiet the computer

  [ myGenerator setStateFromSeed: 1 ];

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample : j ];
    }
  }

  // Then sleep 5 seconds:
  j = sleep(5);

  // Then take the real measurement:

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;

// ----- step 3: thin doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      thindoublebucket =  [ myGenerator getThinDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 4: fat doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      doublebucket =  [ myGenerator getDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 5: long doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<10; j++) {
    for (i = 0; i < j6; i++) {
      ldoublebucket    =  [ myGenerator getLongDoubleSample : j ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- print the data ----------

  printf("Random 0.75 timing tests\n\n");
  printf("Reference generator for relative times and speed is:\n");
  printf("\t\t %s (run first)\n\n", [myGenerator getName]);
  printf("Generators (times in microseconds):\n\n");
  printf("                 Unsigned   thinDouble    fatDouble   longDouble   rtime  speed\n\n");

// ----- save the data ----------

  referenceTimeU  = unsT;
  referenceTimeTD = thinDoubleT;
  referenceTimeD  = doubleT;
  referenceTimeLD = longDoubleT;
  referenceGenerator = myGenerator;	// for use with Distributions

  return self;
}


-simpleGenTest4: (id) myGenerator {
  int i,j;
  unsigned unsignedbucket;
  double thindoublebucket, doublebucket;
  long double ldoublebucket;
  struct timeval before, after;
  double unsT, thinDoubleT, doubleT, longDoubleT;
  double relTime, relSpeed;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

// ----- step 1: unsigned ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;

// ----- step 3: thin doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      thindoublebucket =  [ myGenerator getThinDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 4: fat doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      doublebucket =  [ myGenerator getDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 5: long doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      ldoublebucket    =  [ myGenerator getLongDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;

   relTime = unsT / referenceTimeU;
   relSpeed = 1.0 / relTime;

// ----- print the data ----------

  printf("%12s %12.6f %12.6f %12.6f %12.6f %7.3f %6.3f\n",
	[myGenerator getName],
	unsT, thinDoubleT, doubleT, longDoubleT, relTime, relSpeed);

  return self;
}


-simpleGenReferenceTest4: (id) myGenerator {
  int i,j;
  unsigned unsignedbucket;
  double thindoublebucket, doublebucket;
  long double ldoublebucket;
  struct timeval before, after;
  double unsT, thinDoubleT, doubleT, longDoubleT;

  // const int j7 = 10000000;
  // const int j6 =  1000000;
  const int j5 =   100000;

// ----- step 1: unsigned ----------

  // First run the generator awhile to quiet the computer

  [ myGenerator setStateFromSeed: 1 ];

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample ];
    }
  }

  // then sleep 5 seconds:
  j = sleep(5);

  // Now do the real timing test:

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      unsignedbucket   =  [ myGenerator getUnsignedSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     unsT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;

// ----- step 3: thin doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      thindoublebucket =  [ myGenerator getThinDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     thinDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 4: fat doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      doublebucket =  [ myGenerator getDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     doubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- step 5: long doubles ----------

  [ myGenerator setStateFromSeed: 1 ];

  gettimeofday(&before, NULL);

  for (j=0; j<100; j++) {
    for (i = 0; i < j5; i++) {
      ldoublebucket    =  [ myGenerator getLongDoubleSample ];
    }
  }

  gettimeofday(&after, NULL);

  if (after.tv_usec >= before.tv_usec)
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec)
		+ (after.tv_usec - before.tv_usec) ) / 10000000.0;
  else
     longDoubleT = (1000000.0 * (after.tv_sec-before.tv_sec-1)
		+ (1000000 + after.tv_usec - before.tv_usec) ) / 10000000.0;


// ----- print the data ----------

  printf("Random 0.75 timing tests\n\n");
  printf("Reference generator for relative times and speed is:\n");
  printf("\t\t %s (run first)\n\n", [myGenerator getName]);
  printf("Generators (times in microseconds):\n\n");
  printf("                 Unsigned   thinDouble    fatDouble   longDouble   rtime  speed\n\n");

// ----- save the data ----------

  referenceTimeU  = unsT;
  referenceTimeTD = thinDoubleT;
  referenceTimeD  = doubleT;
  referenceTimeLD = longDoubleT;
  referenceGenerator = myGenerator;	// for use with Distributions

  return self;
}


-buildObjects {
  int j;
 
  [super buildObjects];

  pmmlcg1Generator = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg2Generator = [PMMLCG2gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg3Generator = [PMMLCG3gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg4Generator = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg5Generator = [PMMLCG2gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg6Generator = [PMMLCG3gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg7Generator = [PMMLCG1gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg8Generator = [PMMLCG2gen create: [self getZone] setStateFromSeed: 1];  
  pmmlcg9Generator = [PMMLCG3gen create: [self getZone] setStateFromSeed: 1];  
  lcg1Generator    = [LCG1gen    create: [self getZone] setStateFromSeed: 1];  
  lcg2Generator    = [LCG2gen    create: [self getZone] setStateFromSeed: 1];  
  lcg3Generator    = [LCG3gen    create: [self getZone] setStateFromSeed: 1];  
  acgGenerator     = [ACGgen     create: [self getZone] setStateFromSeed: 1];  
  scgGenerator     = [SCGgen     create: [self getZone] setStateFromSeed: 1];  
  swb1Generator    = [SWB1gen    create: [self getZone] setStateFromSeed: 1];  
  swb2Generator    = [SWB2gen    create: [self getZone] setStateFromSeed: 1];  
  swb3Generator    = [SWB3gen    create: [self getZone] setStateFromSeed: 1];  
  pswbGenerator    = [PSWBgen    create: [self getZone] setStateFromSeed: 1];
  TT403Generator   = [TT403gen   create: [self getZone] setStateFromSeed: 1];
  TT775Generator   = [TT775gen   create: [self getZone] setStateFromSeed: 1];
  TT800Generator   = [TT800gen   create: [self getZone] setStateFromSeed: 1];
  MT19937Generator = [MT19937gen create: [self getZone] setStateFromSeed: 1];
  c2taus1Generator = [C2TAUS1gen create: [self getZone] setStateFromSeed: 1];
  c2taus2Generator = [C2TAUS2gen create: [self getZone] setStateFromSeed: 1];
  c2taus3Generator = [C2TAUS3gen create: [self getZone] setStateFromSeed: 1];
  mrg5Generator    = [MRG5gen    create: [self getZone] setStateFromSeed: 1];
  mrg6Generator    = [MRG6gen    create: [self getZone] setStateFromSeed: 1];
  mrg7Generator    = [MRG7gen    create: [self getZone] setStateFromSeed: 1];
  c2mrg3Generator  = [C2MRG3gen  create: [self getZone] setStateFromSeed: 1];
  mwcaGenerator    = [MWCAgen    create: [self getZone] setStateFromSeed: 1];
  mwcbGenerator    = [MWCBgen    create: [self getZone] setStateFromSeed: 1];
  c3mwcGenerator   = [C3MWCgen   create: [self getZone] setStateFromSeed: 1];
  rwc2Generator    = [RWC2gen    create: [self getZone] setStateFromSeed: 1];
  rwc8Generator    = [RWC8gen    create: [self getZone] setStateFromSeed: 1];
//  rwc8uGenerator   = [RWC8Ugen   create: [self getZone] setStateFromSeed: 1];

  c2lcgxGenerator  = [C2LCGXgen  create: [self getZone] 
			setA: 32 setv: 20 setw: 30
			setStateFromSeed: 1];
  c4lcgxGenerator  = [C4LCGXgen  create: [self getZone] 
			setA: 128 setv: 31 setw: 41 
			setStateFromSeed: 1];

randomBitDistribution = [RandomBitDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone] 
			setStateFromSeed: 1] ];
bernoulliDistribution = [BernoulliDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1] 
			setProbability: 0.67 ];
uniformIntegerDistribution  = 
   [UniformIntegerDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setIntegerMin: -49 setMax: 50 ];
uniformUnsignedDistribution = 
   [UniformUnsignedDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setUnsignedMin: 200 setMax: 299 ];
uniformDoubleDistribution = 
   [UniformDoubleDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setDoubleMin: 3.000 setMax: 4.000 ];
normalDistribution = [NormalDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 setVariance: 1.500 ];
logNormalDistribution = [LogNormalDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 setVariance: 1.500 ];
exponentialDistribution = [ExponentialDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
			setStateFromSeed: 1]
			setMean: 3.000 ];
gammaDistribution = [GammaDist create: [self getZone]
			setGenerator: [MT19937gen create: [self getZone]
		setStateFromSeed: 1]
			setAlpha: 3.000 setBeta: 1.500 ];

  j = sleep(5);	// let system settle down

// First the reference generator:

  myGen = MT19937Generator;
  [ self simpleGenReferenceTest4: myGen ];

  // myGen = c4lcgxGenerator;
  // [ self splitGenReferenceTest4: myGen ];

  j = sleep(5); // more settling down ;-)

// Then the rest:

  myGen = acgGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = lcg1Generator;
  [ self simpleGenTest4: myGen ];

  myGen = swb1Generator;
  [ self simpleGenTest4: myGen ];

  myGen = mwcaGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = pswbGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = TT775Generator;
  [ self simpleGenTest4: myGen ];

  myGen = TT800Generator;
  [ self simpleGenTest4: myGen ];

  myGen = TT403Generator;
  [ self simpleGenTest4: myGen ];

  myGen = mwcbGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = c2taus1Generator;
  [ self simpleGenTest4: myGen ];

  myGen = scgGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = MT19937Generator;
  [ self simpleGenTest4: myGen ];

  myGen = pmmlcg1Generator;
  [ self simpleGenTest4: myGen ];

  myGen = rwc2Generator;
  [ self simpleGenTest4: myGen ];

  myGen = c3mwcGenerator;
  [ self simpleGenTest4: myGen ];

  myGen = c2lcgxGenerator;
  [ self splitGenTest4: myGen ];

  myGen = mrg5Generator;
  [ self simpleGenTest4: myGen ];

  myGen = mrg6Generator;
  [ self simpleGenTest4: myGen ];

  myGen = mrg7Generator;
  [ self simpleGenTest4: myGen ];

  myGen = c4lcgxGenerator;
  [ self splitGenTest4: myGen ];

  myGen = c2mrg3Generator;
  [ self simpleGenTest4: myGen ];

  myGen = rwc8Generator;
  [ self simpleGenTest4: myGen ];

  // myGen = rwc8uGenerator;
  // [ self simpleGenTest4: myGen ];

// Distributions:
// (they all use the reference generator)

  printf("\n\nDistributions (timing in microseconds):\n\n");
  printf("                                     timing     rtime   speed    #g/d       net\n\n");


  myDist = randomBitDistribution;
  [ self bitDistTest4: myDist ];

  myDist = uniformUnsignedDistribution;
  [ self unsDistTest4: myDist ];

  myDist = uniformIntegerDistribution;
  [ self intDistTest4: myDist ];

  myDist = bernoulliDistribution;
  [ self bitDistTest4: myDist ];

  myDist = uniformDoubleDistribution;
  [ self dblDistTest4: myDist ];

  myDist = exponentialDistribution;
  [ self dblDistTest4: myDist ];

  myDist = normalDistribution;
  [ self dblDistTest4: myDist ];

  myDist = logNormalDistribution;
  [ self dblDistTest4: myDist ];

  myDist = gammaDistribution;
  [ self dblDistTest4: myDist ];

  printf("\n");

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
