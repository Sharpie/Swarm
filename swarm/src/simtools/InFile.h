// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>

@interface InFile : SwarmObject {
  FILE *theFile ;
}

+create: aZone withName: (char *) theName ;

-_setFile_: (FILE *) aFile ;

-(int) getWord: (char *) aWord ;
-(int) getInt: (int *) anInt ;
-(int) getDouble: (double *) aDouble ;
-(int) getFloat: (float *) aFloat ;
-(int) getChar: (char *) aChar ;
-(int) unGetChar: (char) aChar ;
-(int) skipLine ;

@end
