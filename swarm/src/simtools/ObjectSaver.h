// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <swarmobject/SwarmObject.h>
#import <swarmobject/VarProbe.h>

@interface ObjectSaver : SwarmObject {
  id templateProbeMap ;  
  id theFileObject ;    
}

+save: anObject to: aFileObject ;
+save: anObject to: aFileObject withTemplate: aProbeMap ;
+save: anObject toFileNamed: (char *) aFileName ;
+save: anObject toFileNamed: (char *) aFileName withTemplate: aProbeMap ;

-setFileObject: aFileObject ;
-setTemplateProbeMap: aProbeMap ;
-saveObject: anObject ;

-(void) _crash_: anObject ;

@end
