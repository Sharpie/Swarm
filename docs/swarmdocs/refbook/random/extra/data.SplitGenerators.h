#define COMPONENTS 4
#define SEEDS      4

struct vGenStruct {
   unsigned long long int currentCount;
   unsigned long long int currentSegment;
   unsigned int Ig [SEEDS];
   unsigned int Lg [SEEDS];
   unsigned int Cg [SEEDS];
} ;


@interface xxxXgen: SwarmObject

{

// 1. Data for the whole backbone generator:

// Generator personality:

   unsigned int stateSize;
   unsigned int genMagic;
   char genName[12];	

// Characteristic constants:

   unsigned int unsignedMax;		// largest integer value returned
   double invModMult;			// (1.0/unsignedMax+1)
   double invModMult2;			// invModMult^2

   unsigned int lengthOfSeedVector;	// if multiple seeds
   unsigned int maxSeedValues[SEEDS];	// if multiple seeds

// Parameters defining the configuration:

   unsigned int numGenerators;		// A
   unsigned int numSegments;		// v
   unsigned long long int segmentMax;	// V = 2^v
   unsigned int segmentLength;		// w
   unsigned long long int countMax;	// W = 2^w

// Other parameters:

   BOOL antiThetic;			// see -getUnsignedSample

// Working variables:

   BOOL singleInitialSeed;		// created with 1 seed ?
   unsigned int initialSeed;		// starting seed used (if 1)
   unsigned int initialSeeds[SEEDS];	// starting seeds used (if > 1)

// 2. Data for each of the virtual generators:

   struct vGenStruct * vGenArr;		// vGenArr[A], allocated dynamically

// --

// 3. Data for each of the component generators of the backbone:

// Generator parameters:

   unsigned int a   [COMPONENTS];	// multiplier
   unsigned int m   [COMPONENTS];	// modulus

// Fixed value working variables (all functions of m):

   unsigned int q   [COMPONENTS];	// quotient m/a
   unsigned int r   [COMPONENTS];	// remainder m%a

   unsigned int aw  [COMPONENTS];	// segment multiplier
   unsigned int avw [COMPONENTS];	// vGen multiplier

}
