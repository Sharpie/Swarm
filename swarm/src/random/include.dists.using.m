// dists.include.using.m

// Code common to all distributions
// Random version 0.8
// 


-(id) getGenerator {
   return randomGenerator;
}

-(unsigned) getVirtualGenerator {
   return virtualGenerator;
}

-(BOOL) getOptionsInitialized {
   return optionsInitialized;
}

-(unsigned long long int) getCurrentCount {
   return currentCount;
}

-(unsigned) getStateSize {
   return stateSize;
}

-(const char *) getName {
   return distName;
}

-(unsigned) getMagic {
   return distMagic;
}

// dists.include.using.m

