// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#define __USE_FIXED_PROTOTYPES__  // for gcc headers
#import <math.h>
#import <stdlib.h>
#import <collections.h>
#import <tkobjc.h>
#import <simtools.h>
#import <analysis.h>
@implementation EZDistribution

-createEnd {

  [super createEnd] ;

  probabilities = (double *) malloc(binNum * sizeof(double)) ;
  maximumEntropy = log( 1.0 / ((double) binNum)) ;
   
  return self ;
}

-update {
  int i ;

  [super update] ;

  for(i = 0 ; i < binNum ; i++){
    probabilities[i] = ((double)distribution[i]) / ((double)count) ;
    if(probabilities[i] > 0.0)
      entropy += probabilities[i] * log(probabilities[i]) ;
  }

  entropy /= maximumEntropy ;

  return self ;
}

-output {
  int i ;

  if(graphics){
    [globalTkInterp eval: 
      "%s marker configure active_outlier_marker \
          -text \"outliers: %d (%lg)\" ",
      [aHisto getWidgetName], 
      outliers, 
      ((double)outliers) / (((double)outliers) + ((double)count))] ;
  
    [aHisto drawHistoWithDouble: probabilities atLocations: locations] ;
  }

  if(fileOutput){
    [anOutFile putInt: probabilities[0]] ;
    for(i = 1 ; i < binNum ; i++){
      [anOutFile putTab] ;
      [anOutFile putInt: probabilities[i]] ;
    }
    [anOutFile putNewLine] ;
  }

  return self ;
}

-(double *)getProbabilities {
  if(clean){
    fprintf(stderr,"Attempted to getProbabilities from a reset EZDistribution (no data available).\n") ;
    exit(-1) ;
  }

  return probabilities ;
}

-(double) getEntropy {
  if(clean){
    fprintf(stderr,
 "Attempted to getEntropy from a reset EZDistribution (no data available).\n") ;
    exit(-1) ;
  }
  
  return entropy ;
}


-(void) drop {
  free(probabilities) ;
  [super drop] ;
}

@end

