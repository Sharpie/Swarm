// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <objectbase/SwarmObject.h>

@interface ObjectLoader: SwarmObject
{
  id probeMapCache;
  id theFileObject; 
}

+ load: anObject from: aFileObject;
+ load: anObject fromFileNamed: (const char *)aFileName;

- setFileObject: aFileObject;
- loadObject: anObject;

- setTemplateProbeMap : probeMap;
- updateCache: exampleTarget;

- (void)_crash_: anObject;

@end

