// Swarm library. Copyright (C) 1996-1999 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>
#import <objectbase/VarProbe.h>
#import <simtools.h>

@interface ObjectSaver: SwarmObject
{
  id <ProbeMap> templateProbeMap;  
  id theFileObject;    
}

+ save: anObject to: aFileObject;
+ save: anObject to: aFileObject withTemplate: (id <ProbeMap>)aProbeMap;
+ save: anObject toFileNamed: (const char *)aFileName;
+ save: anObject toFileNamed: (const char *)aFileName withTemplate: (id <ProbeMap>)aProbeMap;

- setFileObject: aFileObject;
- setTemplateProbeMap: (id <ProbeMap>)aProbeMap;
- saveObject: anObject;

- (void)_crash_: anObject;

@end

