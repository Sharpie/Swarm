//
// <random/distributions.h>
//
//     1997-09-01 (v. 0.7)
//

// 
// See the file docs/README.Distributions.v07 for guide to usage
// 

// ProbabilityDistribution --
// 
// A process for generating a sequence of random numbers matching the
//   frequencies defined by a specific distribution function.  The
//   process is driven by input from a supplied uniform random generator.
//

#undef USETHINDOUBLES
// 
// All objects implementing the DoubleDistribution protocol presently call
//   the -getDoubleSample method of the generators, which uses 2 random
//   integer samples to fill the mantissa of the returned double value.
//
// If you do not need this degree of precision, or prefer faster execution,
//   uncomment the following line and recompile:
// #define USETHINDOUBLES 1
// 

@deftype ProbabilityDistribution <Create, Drop, InternalState> 
CREATING
+		createWithDefaults: aZone;

+		create: aZone setGenerator: (id) generator;

+		create: aZone setGenerator: (id) generator
		setVirtualGenerator: (unsigned) vGen;
+		createBegin;
USING
-		setGenerator: (id) generator;
-		setGenerator: (id) generator 
			setVirtualGenerator: (unsigned) vGen;
-		createEnd;

-		reset;

-               getGenerator;
- (unsigned)	getVirtualGenerator;
- (BOOL)	getOptionsInitialized;

- (unsigned long long int) getCurrentCount;
@end

// 
// Subtype protocols:
// 

//
// BooleanDistribution --
//   A probability distribution that returns YES/NO sample values.
//
@deftype BooleanDistribution <ProbabilityDistribution> 
- (BOOL)	getBooleanSample;
- (int)		getIntegerSample;	// for convenience
@end

//
// IntegerDistribution --
//   A probability distribution that returns integer sample values.
//
@deftype IntegerDistribution <ProbabilityDistribution> 
- (int)		getIntegerSample;
@end

//
// UnsignedDistribution --
//   A probability distribution that returns non-negative integer sample values.
//
@deftype UnsignedDistribution <ProbabilityDistribution> 
- (unsigned)	getUnsignedSample;
@end

//
// DoubleDistribution --
//   A probability distribution that returns an approximation of continuous
//   values as represented by double-precision floating point values.
//
@deftype DoubleDistribution <ProbabilityDistribution>
- (double)	getDoubleSample;
@end

// 
// Protocol definitions for specific distributions:
// 

//
// RandomBitDistribution --
//   A generator that returns uniformly distributed single bit values
//   (i.e. fair coin tosses).
//
@deftype RandomBitDist <BooleanDistribution, CREATABLE>
USING
- (BOOL)	getCoinToss;
@end

// 
// BernoulliDistribution -- distribution returning YES with given probability p
// 
@deftype BernoulliDist <BooleanDistribution, CREATABLE>
CREATING
+		create: aZone setGenerator: generator
                setProbability: (double) p;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setProbability: (double) p;
USING
-		setProbability: (double) p;

- (double)	getProbability;

- (BOOL)        getSampleWithProbability: (double) p;
@end


//
// UniformInteger --
//   A generator of integral values uniformly distributed across a closed
//   interval [min,max]. (The interval includes both its endpoints.)
//   Setting minValue == maxValue is allowed (and returns minValue).
// 
@deftype UniformIntegerDist <IntegerDistribution, CREATABLE>
CREATING
+		create: aZone setGenerator: generator
                setIntegerMin: (int) minValue setMax: (int) maxValue;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setIntegerMin: (int) minValue setMax: (int) maxValue;
USING
-		setIntegerMin: (int) minValue setMax: (int) maxValue;

- (int)		getIntegerMin;
- (int)		getIntegerMax;

- (int)		getIntegerWithMin: (int) minValue withMax: (int) maxValue;
@end

//
// UniformUnsigned --
//   A generator of non-negative integral values uniformly distributed across
//   a closed interval [min,max].  (The interval includes both its endpoints.)
//   Setting minValue == maxValue is allowed (and returns minValue).
//
@deftype UniformUnsignedDist <UnsignedDistribution, CREATABLE> 
CREATING
+		create: aZone setGenerator: generator
                setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;
USING
-		setUnsignedMin: (unsigned) minValue setMax: (unsigned) maxValue;

- (unsigned)	getUnsignedMin;
- (unsigned)	getUnsignedMax;

- (unsigned)	getUnsignedWithMin: (unsigned) minVal 
		           withMax: (unsigned) maxVal;
@end

//
// UniformDouble --
//   A generator of floating point values uniformly distributed
//   across a half-open interval [min,max). (The interval includes
//   the lower endpoint but excludes the upper endpoint.)
//   NOTE: Setting minValue == maxValue is allowed (and returns minValue).
//
@deftype UniformDoubleDist <DoubleDistribution, CREATABLE> 
CREATING
+		create: aZone setGenerator: generator
                setDoubleMin: (double) minValue setMax: (double) maxValue;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setDoubleMin: (double) minValue setMax: (double) maxValue;
USING
-		setDoubleMin: (double) minValue setMax: (double) maxValue;

- (double)	getDoubleMin;
- (double)	getDoubleMax;

- (double)	getDoubleWithMin: (double) minValue withMax: (double) maxValue;
@end

//
// NormalDistribution -- a well-known continuous probability distribution
// LogNormalDistribution -- also well-known ...
//
@deftype Normal <DoubleDistribution> 
CREATING
+		create: aZone setGenerator: generator
                setMean: (double) mean setVariance: (double) variance;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setMean: (double) mean setVariance: (double) variance;
USING
-		setMean: (double) mean setVariance: (double) variance;

- (double)	getMean;
- (double)	getVariance;
- (double)	getStdDev;

- (double)	getSampleWithMean: (double) mean 
			withVariance: (double) variance;
@end

@deftype NormalDist    <Normal, CREATABLE> 
@end

@deftype LogNormalDist <Normal, CREATABLE> 
@end

//
// ExponentialDistribution -- a well-known continuous probability distribution
//
@deftype ExponentialDist <DoubleDistribution, CREATABLE> 
CREATING
+		create: aZone setGenerator: generator
                setMean: (double) mean;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
                setMean: (double) mean;
USING
-		setMean: (double) mean;

- (double)	getMean;

- (double)	getSampleWithMean: (double) mean ;
@end

//
// GammaDistribution -- a well-known continuous probability distribution
//
@deftype GammaDist <DoubleDistribution, CREATABLE> 
CREATING
+		create: aZone setGenerator: generator
		setAlpha: (double) alpha setBeta: (double) beta;

+		create: aZone setGenerator: generator
		setVirtualGenerator: (unsigned) vGen
		setAlpha: (double) alpha setBeta: (double) beta;
USING
-		setAlpha: (double) alpha setBeta: (double) beta;

- (double)	getAlpha;
- (double)	getBeta;

- (double)	getSampleWithAlpha: (double) alpha 
			withBeta: (double) beta;
@end

//
// Include declarations of type factories for each deftype marked CREATABLE
// (type factories can be defined either as class names or external id's)
//

// Boolean:
@class RandomBitDist;
@class BernoulliDist;

// Integer:
@class UniformIntegerDist;

// Unsigned:
@class UniformUnsignedDist;

// Double:
@class UniformDoubleDist;
@class NormalDist;
@class LogNormalDist;
@class ExponentialDist;
@class GammaDist;

