// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import "Probe.h"

// two special types of IMP: take no arguments, pass back either int or double
typedef int (*IntImp)(id, SEL, ...);
typedef float (*FloatImp)(id, SEL, ...);
typedef double (*DoubleImp)(id, SEL, ...);

@interface MessageProbe: Probe {
  SEL probedSelector ;
  int returnCategory ;

  IntImp intImp ; 
  FloatImp floatImp ;
  DoubleImp doubleImp ; 
  int caching ;

  char * probedMessage;
  int argNum ;
  int hr ;
  char **argLabels ;
  char **arguments ;
}

-setProbedSelector: (SEL) aSel;
-setProbedMessage: (char *) aMessage;
-createEnd;

-(const char *) getProbedMessage;
-(int) getArgNum;
-setArg: (int) which To: (char *) what;
-(char *) getArg: (int) which ;
-(char *) getArgName: (int) which ;

-setHideResult: (int) val ;
-(int) getHideResult ;

-_setImp_: anObject ;
-updateMethodCache: anObject;


-_trueDynamicCallOn_: target resultStorage: (char **) result ;
-dynamicCallOn: target resultStorage: (char **) result;
-dynamicCallOn: target ;
-(int)intDynamicCallOn: target ;
-(float)floatDynamicCallOn: target ;
-(double)doubleDynamicCallOn: target ;

-(int) isResultId ;
-(int) isArgumentId: (int) which ;

@end

