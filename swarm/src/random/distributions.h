//
// <random/distributions.h>
//
//     1997-09-01 (v. 0.7)
//     1998-10-08 (v. 0.8)
//     2000-02-19 (v. 0.81)
//     2001-07-17 
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
+ createWithDefaults: (id <Zone>)aZone;

//M: Use this create message if the generator to be attached is a Simple one:
+ create      : (id <Zone>)aZone
  setGenerator: (id <SimpleRandomGenerator>)simpleGenerator;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone 
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
  setVirtualGenerator: (unsigned) vGen;

SETTING
//M: Use this message if the generator to be attached is a Simple one:
- setGenerator: (id <SimpleRandomGenerator>)simpleGenerator;

//M: Use this message if the generator to be attached is a Split one:
- setGenerator       : (id <SplitRandomGenerator>)splitGenerator 
  setVirtualGenerator: (unsigned)vGen;

//M: The reset method resets the currentCount and other state data.
- reset;

USING
//M: The getGenerator method returns the id of the generator.
- (id <BasicRandomGenerator>) getGenerator;

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
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
  setProbability: (double)p;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
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
+ create      : (id <Zone>)aZone
  setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
 setIntegerMin: (int)minValue
        setMax: (int)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create           : (id <Zone>)aZone
       setGenerator: (id <SplitRandomGenerator>)splitGenerator
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
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
  setUnsignedMin: (unsigned)minValue
          setMax: (unsigned)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
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
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
    setDoubleMin: (double)minValue
          setMax: (double)maxValue;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
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
//M: Use this create message if the generator to be attached is a Simple one
//M: and you wish to specify the variance:
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
         setMean: (double)mean
     setVariance: (double)variance;

//M: Use this create message if the generator to be attached is a Simple one
//M: and you wish to specify the standard deviation:
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
         setMean: (double)mean
       setStdDev: (double)sdev;

//M: Use this create message if the generator to be attached is a Split one
//M: and you wish to specify the variance:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
          setVariance: (double)variance;

//M: Use this create message if the generator to be attached is a Split one
//M: and you wish to specify the standard deviation:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
  setVirtualGenerator: (unsigned)vGen
              setMean: (double)mean
            setStdDev: (double)sdev;

SETTING
//M: The setMean:setVariance: method 
//M: sets the mean and the variance of the distribution.
- setMean: (double)mean setVariance: (double)variance;

//M: The setMean:setStdDev: method 
//M: sets the mean and the standard deviation of the distribution.
- setMean: (double)mean setStdDev: (double)sdev;

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

//M: The getSampleWithMean:withStdDev: method returns a sample value drawn
//M: from a distribution with the specified mean and standard deviation.
- (double)getSampleWithMean: (double)mean 
                 withStdDev: (double)sdev;
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
+ create      : (id <Zone>)aZone
  setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
       setMean: (double)mean;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
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
+ create     : (id <Zone>)aZone
 setGenerator: (id <SimpleRandomGenerator>)simpleGenerator
     setAlpha: (double)alpha
      setBeta: (double)beta;

//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)splitGenerator
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

@protocol PoissonDist <UnsignedDistribution, CREATABLE>
//S: Poisson distribution
//D: A distribution used to model the integer number of occurrences 
//D: of some event over an interval of time or space. 

CREATING
//M: Use this create message if the generator to be attached is a Simple one
//M: and both the occurrence rate and the interval are set at create time:
+ create     : (id <Zone>)aZone
 setGenerator: (id <SimpleRandomGenerator>)generator
 setOccurRate: (double) anOccurRate
  setInterval: (double) anInterval;

//M: Use this create message if the generator to be attached is a Split one and
//M: both the occurrence rate and the interval are to be set at create time:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator
  setVirtualGenerator: (unsigned)vGen
         setOccurRate: (double) anOccurRate
          setInterval: (double) anInterval;

SETTING

//M: The setInterval method only sets the interval parameter; the occurRate 
//M: parameter is left unchanged from its previous or initialized value 
- setInterval: (double) anInterval;

//M: The setOccurRate method only sets the occurRate parameter; the interval 
//M: parameter is left unchanged from its previous or initialized value 
- setOccurRate: (double) anOccurRate;

//M: The setOccurRate:setInterval method sets both the occurrence rate 
//M: and the interval parameters.
- setOccurRate: (double) anOccurRate
   setInterval: (double) anInterval;

USING

//M: The getOccurRate method returns the occurrence rate parameter.
- (double) getOccurRate;

//M: The getInterval method returns the interval parameter.
- (double) getInterval;

//M: The getUnsignedSampleWithInterval method returns a sample value using 
//M: the distribution's current occurrence rate and new interval value. 
//M: Causes an error if the occurrence rate has not been previously set.
- (unsigned) getUnsignedSampleWithInterval: (double) anInterval;

//M: The getUnsignedSampleWithOccurRate:andInterval method returns 
//M: a sample value for the specified occurrence rate and interval. 
//M: Does not change the the distribution's parameter values set by 
//M: the setter methods.
- (unsigned) getUnsignedSampleWithOccurRate: (double) anOccurRate
                         withInterval: (double) anInterval;

@end

@protocol BinomialDist <UnsignedDistribution, CREATABLE>
//S: Binomial distribution
//D: The binomial distribution gives the discrete probability 
//D: of obtaining exactly n successes out of N Bernoulli trials

CREATING
//M: Use this create message if the generator to be attached is a Simple one:
+ create     : (id <Zone>)aZone
 setGenerator: (id <SimpleRandomGenerator>)generator;

//M: Use this create message if the generator to be attached is a Simple one:
//M: and both the number of trials and the probability are to be set at create time:
+ create        : (id <Zone>)aZone
    setGenerator: (id <SimpleRandomGenerator>)generator
    setNumTrials: (unsigned) aNumTrials
  setProbability: (double) aProbability;


//M: Use this create message if the generator to be attached is a Split one:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator
  setVirtualGenerator: (unsigned)vGen;

//M: Use this create message if the generator to be attached is a Split one
//M: and both the number of trials and the probability are to be set at create time:
+ create             : (id <Zone>)aZone
         setGenerator: (id <SplitRandomGenerator>)generator
  setVirtualGenerator: (unsigned)vGen
         setNumTrials: (unsigned) aNumTrials
       setProbability: (double) aProbability;

SETTING

//M: The setNumTrials only sets the numTrials parameter; the probability parameter
//M: is left unchanged from its previous or initialized value 
- setNumTrials: (unsigned) aNumTrials;

//M: The setNumTrials:setProbability sets both the number of trials rate 
//M: and the probability parameters.
-    setNumTrials: (unsigned) aNumTrials
   setProbability: (double) aProbability;

USING

//M: The getNumTrials returns number of trials parameter.
- (unsigned) getNumTrials;

//M: The getProbability returns probability parameter.
- (double) getProbability;

#if 0
//M: The getIntegerSample returns a sample value using the distribution's current 
//M: number of trials and probability parameters; causes an error if these parameters 
//M: have not been previously set.
- (unsigned) getUnsignedSample;
#endif

//M: The getIntegerSampleWithInterval returns a sample value using the distribution's 
//M: current number of trials and new probability value. Causes an error if the number 
//M: of trials has not been previously set.
- (unsigned) getUnsignedSampleWithProbability: (double) aProbability;

//M: The getUnsignedSampleWithOccurRate:andInterval return a sample value for the
//M: specified number of trials and probability. Does not change the the distribution's
//M: parameter values.
- (unsigned) getUnsignedSampleWithNumTrials: (unsigned) aNumTrials
                            withProbability: (double) aProbability;


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
@class PoissonDist;
@class BinomialDist;
