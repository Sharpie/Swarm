@interface NormalDist: SwarmObject 

{

// Distribution personality:

   unsigned 	stateSize;	
   unsigned 	distMagic;
   char     	distName [DISTNAMESIZE];

// Data objects and fixed variables:

   id       	randomGenerator;
   unsigned	generatorMax;

   BOOL     	useSplitGenerator;
   unsigned 	virtualGenerator;
   BOOL     	optionsInitialized;

// Count of variates generated:

   unsigned long long int currentCount;

// --

// Parameters:

   double theMean;
   double theVariance;

// Working variables:

   double theStdDev;

// State variables:

   BOOL stored;
   double stored_double;

}
