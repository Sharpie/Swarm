// Common.gens.genSeeds.m

// 
// Common code for all generators
// Random version 0.7
// 

// Use an inline generator to create the full number of seeds required,
// using the single seed initialSeed to seed this inline generator.
// The specific generator used is a PMMLCG with a=69069 and m=2^32
// (see Knuth vol. 2, table 1 on p. 102, line 25). Cf. MT19937 source.
// This generator creates random nonzero 32-bit values, as long as
// initialSeed is not zero.

-generateSeeds {
   int ki;
   unsigned z;

   z = initialSeed;

   // Mix the bits a bit:
   for (ki=0; ki < 16; ki++)
      z = (69069 * z);		// modulo 2^32

   // Generate seeds, as many as needed:

   for (ki=0; ki < lengthOfSeedVector; ki++) {

     z = (69069 * z);		// modulo 2^32
     initialSeeds[ki] = z;
      
   }


   // Make sure we don't supply invalid seed values for this generator:

   for (ki=0; ki < lengthOfSeedVector; ki++) {

      if (maxSeedValues[ki] < 0xffffffff) {	// avoid seeds > maxSeedValue

        initialSeeds[ki] = initialSeeds[ki] % (maxSeedValues[ki] + 1);
        if (initialSeeds[ki] == 0) 		// avoid seed = 0
          initialSeeds[ki] = 0xecec;		// arbitrary value < 2^16
      } 
   }

   return self;
}


