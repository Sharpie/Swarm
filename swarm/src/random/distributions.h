//
// <random/distributions.h>
//
//     1997-09-01 (v. 0.7)
//     1998-10-08 (v. 0.8)
//

// 
// See the file docs/README.Distributions.v07 for guide to usage
// 

#undef USETHINDOUBLES
// 
// All objects implementing the DoubleDistribution protocol presently call
//   the -getDoubleSample method of the generators, which uses 2 random
//   integer samples to fill the mantissa of the returned double value.
//
// If you do not need this degree of precision, or prefer faster execution,
//   uncomment the following line and recompile ('make install'):
// #define USETHINDOUBLES 1
// 

@protocol ProbabilityDistribution <SwarmObject, InternalState> 
//S: Probability Distribution

//D: A process for generating a sequence of random numbers matching the
//D: frequencies defined by a specific distribution function.  The
//D: process is driven by input from a supplied uniform random generator.

CREATING
//M: The createWithDefaults method creates a distribution object with a 
//M: default set of seeds and parameters, and its own private generator.
+ createWithDefaults: aZone;

//M: Use this create message if the generator to be attached is a Simple one:
+ create: aZone setGenerator: generator;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone 
         setGenerator: generator
  setVirtualGenerator: (unsigned) vGen;

+ createBegin: aZone;

- createEnd;

SETTING
//M: Use this message if the generator to be attached is a Simple one:
- setGenerator: generator;

//M: Use this message if the generator to be attached is a Split one:
- setGenerator       : generator 
  setVirtualGenerator: (unsigned)vGen;

//M: The reset method resets the currentCount and other state data.
- reset;

USING
//M: The getGenerator method returns the id of the generator.
- getGenerator;

//M: The getVirtualGenerator returns the number of the virtual generator used.
- (unsigned)getVirtualGenerator;

//M: The getOptionsInitialized returns the value of the parameter.
- (BOOL)getOptionsInitialized;

//M: The getCurrentCount method returns the count of variates generated.
- (unsigned long long int)getCurrentCount;
@end

// 
// Subtype protocols:
// 

@protocol BooleanDistribution <ProbabilityDistribution> 
//S: Boolean Distribution

//D: A probability distribution that returns YES/NO sample values.

USING
//M: The getBooleanSample method returns a YES or NO sample value.
- (BOOL)getBooleanSample;
- (int)getIntegerSample;	// for convenience
@end

@protocol IntegerDistribution <ProbabilityDistribution> 
//S: Integer Distribution 

//D: A probability distribution that returns integer sample values.

USING
//M: The getIntegerSample method returns an integer sample value.
- (int)getIntegerSample;
@end

@protocol UnsignedDistribution <ProbabilityDistribution> 
//S: Unsigned Distribution 

//D: A probability distribution that returns non-negative integer sample 
//D: values.

USING
//M: The getUnsignedSample method returns a non-negative integer sample value.
- (unsigned)getUnsignedSample;
@end

@protocol DoubleDistribution <ProbabilityDistribution>
//S: Double Distribution 

//D: A probability distribution that returns an approximation of continuous
//D: values as represented by double-precision floating point values.

USING
//M: The getDoubleSample method returns a double-precision floating point 
//M: value.
- (double)getDoubleSample;
@end

// 
// Protocol definitions for specific distributions:
// 
// 

@protocol RandomBitDist <BooleanDistribution, CREATABLE>
//S: Random Bit Distribution 

//D: A generator that returns uniformly distributed single bit values
//D: (i.e. fair coin tosses).

USING
//M: The getCoinToss method returns a YES or NO value.
- (BOOL)getCoinToss;
@end

@protocol BernoulliDist <BooleanDistribution, CREATABLE>
//S: Bernoulli Distribution 

//D: A distribution returning YES with a given probability.
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create        : aZone
    setGenerator: generator
  setProbability: (double)p;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
       setProbability: (double)p;

SETTING
//M: The setProbability: method sets the probability of returning YES.
- setProbability: (double)p;

USING
//M: The getProbability method returns the probability of returning YES.
- (double)getProbability;

//M: The getSampleWithProbability: returns a sample YES or NO value.
- (BOOL)getSampleWithProbability: (double)p;
@end


@protocol UniformIntegerDist <IntegerDistribution, CREATABLE>
//S: Uniform Integer Distribution

//D: A generator of integral values uniformly distributed across a closed
//D: interval [min,max]. (The interval includes both its endpoints.)
//D: Setting minValue == maxValue is allowed (and returns minValue).
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create      : aZone
  setGenerator: generator
 setIntegerMin: (int)minValue
        setMax: (int)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create           : aZone
       setGenerator: generator
setVirtualGenerator: (unsigned) vGen
      setIntegerMin: (int)minValue
             setMax: (int)maxValue;

SETTING
//M: The setIntegerMin:setMax: method sets the minimum and maximum integer
//M: values to be returned
- setIntegerMin: (int)minValue setMax: (int)maxValue;

USING
//M: The getIntegerMin method returns the minimum integer value.
- (int)getIntegerMin;

//M: The getIntegerMax method returns the maximum integer value.
- (int)getIntegerMax;

//M: The getIntegerWithMin:withMax: returns an integer within the interval
//M: [min, max].
- (int)getIntegerWithMin: (int)minValue withMax: (int)maxValue;
@end

@protocol UniformUnsignedDist <UnsignedDistribution, CREATABLE> 
//S: Uniform Unsigned Distribution

//D: A generator of non-negative integral values uniformly distributed across
//D: a closed interval [min,max].  (The interval includes both its endpoints.)
//D: Setting minValue == maxValue is allowed (and returns minValue).
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create        : aZone
    setGenerator: generator
  setUnsignedMin: (unsigned)minValue
          setMax: (unsigned)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
       setUnsignedMin: (unsigned)minValue
               setMax: (unsigned)maxValue;

SETTING
//M: The setUnsignedMin:setMax: method sets the minimum and maximum unsigned
//M: values to be returned
- setUnsignedMin: (unsigned)minValue
          setMax: (unsigned)maxValue;

USING
//M: The getUnsignedMin method returns the minimum unsigned value.
- (unsigned)getUnsignedMin;

//M: The getUnsignedMax method returns the maximum unsigned value.
- (unsigned)getUnsignedMax;

//M: The getUnsignedWithMin:withMax: returns an unsigned integer within the 
//M: interval [min, max].
- (unsigned)getUnsignedWithMin: (unsigned)minVal 
                       withMax: (unsigned)maxVal;
@end

@protocol UniformDoubleDist <DoubleDistribution, CREATABLE> 
//S: Uniform Double Distribution

//D: A generator of floating point values uniformly distributed
//D: across a half-open interval [min,max). (The interval includes
//D: the lower endpoint but excludes the upper endpoint.)
//D: NOTE: Setting minValue == maxValue is allowed (and returns minValue).

CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create        : aZone
    setGenerator: generator
    setDoubleMin: (double)minValue
          setMax: (double)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
         setDoubleMin: (double)minValue
               setMax: (double)maxValue;

SETTING
//M: The setDoubleMin:setMax method sets the minimum and maximum floating
//M: point values of the distribution.
- setDoubleMin: (double)minValue setMax: (double)maxValue;

USING
//M: The getDoubleMin method returns the minimum floating point value in the
//M: specified range.
- (double)getDoubleMin;

//M: The getDoubleMax method returns the maximum floating point value in the 
//M: specified range.
- (double)getDoubleMax;

//M: The getDoubleWithMin:withMax: method returns a floating point value 
//M: within the range [min, max).
- (double)getDoubleWithMin: (double)minValue withMax: (double)maxValue;
@end

@protocol Normal <DoubleDistribution> 
//S: Internal
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create        : aZone
    setGenerator: generator
         setMean: (double)mean
     setVariance: (double)variance;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
          setVariance: (double)variance;

SETTING
//M: The setMean:setVariance: method sets the mean and the variance of the 
//M: distribution.
- setMean: (double)mean setVariance: (double)variance;

USING
//M: The getMean method returns the mean of the distribution.
- (double)getMean;

//M: The getVariance method returns the variance of the distribution.
- (double)getVariance;

//M: The getStdDev method returns the standard deviation of the distribution.
- (double)getStdDev;

//M: The getSampleWithMean:withVariance: method returns a sample value drawn
//M: from a distribution with the specified mean and variance.
- (double)getSampleWithMean: (double)mean 
               withVariance: (double)variance;
@end

@protocol NormalDist <Normal, CREATABLE> 
//S:  Normal (Gaussian) distribution

//D:  A well-known continuous probability distribution returning doubles.
@end

@protocol LogNormalDist <Normal, CREATABLE> 
//S: Log-Normal distribution

//D:  A well-known continuous probability distribution returning doubles.
@end

@protocol ExponentialDist <DoubleDistribution, CREATABLE> 
//S: Exponential distribuiton 

//D: A well-known continuous probability distribution returning doubles.
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create      : aZone
  setGenerator: generator
       setMean: (double)mean;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean;


SETTING
//M: The setMean: method sets the mean of the distribution.
- setMean: (double)mean;

USING
//M: The getMean method returns the mean of the distribution.
- (double)getMean;

//M: The getSampleWithMean: method returns a sample value from a distribution
//M: with the specified mean.
- (double)getSampleWithMean: (double)mean;
@end

@protocol GammaDist <DoubleDistribution, CREATABLE>
//S: Gamma distribution

//D: A well-known continuous probability distribution returning doubles
CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create     : aZone
 setGenerator: generator
     setAlpha: (double)alpha
      setBeta: (double)beta;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : aZone
         setGenerator: generator
  setVirtualGenerator: (unsigned)vGen
             setAlpha: (double)alpha
              setBeta: (double)beta;

SETTING
//M: The setAlpha:setBeta: method sets the alpha and beta values for the
//M: gamma distribution.
- setAlpha: (double)alpha setBeta: (double)beta;

USING
//M: The getAlpha method returns the alpha value.
- (double)getAlpha;

//M: The getBeta method returns the beta value.
- (double)getBeta;

//M: The getSampleWithAlpha:withBeta: method returns a sample value from a
//M: Gamma distribution with the specified alpha and beta values.
- (double)getSampleWithAlpha: (double)alpha 
                    withBeta: (double)beta;
@end

// Include declarations of type factories for each protocol marked CREATABLE
// (type factories can be defined either as class names or external id's)

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

