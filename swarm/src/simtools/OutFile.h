// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject.h>

@interface OutFile : SwarmObject {
  FILE *theFile ;
}

+create: aZone withName: (char *) theName ;

-_setFile_: (FILE *) aFile ;

-putString: (char *) aString ;
-putInt: (int) anInt ;
-putDouble: (double) aDouble ;
-putFloat: (float) aFloat ;
-putChar: (char) aChar ;
-putTab ;
-putNewLine ;

@end
