// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>

@interface ObjectLoader : SwarmObject {
  id probeMapCache ;  
  id theFileObject ;    
}

+load: anObject from: aFileObject ;
+load: anObject fromFileNamed: (char *) aFileName ;

-setFileObject: aFileObject ;
-loadObject: anObject ;

-updateCache: exampleTarget ;

-(void) _crash_: anObject ;

@end




