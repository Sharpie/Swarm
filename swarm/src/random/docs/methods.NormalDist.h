//
// NormalDistribution -- a well-known continuous probability distribution
//
@deftype NormalDist
CREATING
+	create: aZone setGenerator: (id) generator;

+	create: aZone setGenerator: (id) generator
	setVirtualGenerator: (unsigned) vGen;

+	create: aZone setGenerator: generator
        setMean: (double) mean setVariance: (double) variance;

+	create: aZone setGenerator: generator
	setVirtualGenerator: (unsigned) vGen
        setMean: (double) mean setVariance: (double) variance;

+	createWithDefaults: aZone;

+	createBegin: (id) aZone;
-	setGenerator: (id) generator;
-	setGenerator: (id) generator setVirtualGenerator: (unsigned) vGen;
-	createEnd;

USING
-		reset;
-               getGenerator;
- (unsigned)	getVirtualGenerator;
- (BOOL)	getOptionsInitialized;

- (unsigned long long int) getCurrentCount;

- (double)	getMean;
- (double)	getVariance;
- (double)	getStdDev;

- (double)	getDoubleSample;

- (double)	getSampleWithMean: (double) mean 
			withVariance: (double) variance;

- (unsigned)	getStateSize;		
- (void)	putStateInto: (void *) buffer;
- (void)	setStateFrom: (void *) buffer;
- (void)	describe: (id) outStream;
- (const char *)getName;		
- (unsigned)	getMagic;	

@end
