// Swarm library. Copyright � 1996-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools.h> // ObjectLoader
#import <objectbase/SwarmObject.h>

@interface ObjectLoader: SwarmObject <ObjectLoader>
{
  id <ProbeMap> probeMapCache;
  id theFileObject; 
}

+ load: anObject from: aFileObject;
+ load: anObject fromFileNamed: (const char *)aFileName;
+ load: anObject fromAppConfigFileNamed: (const char *)aFileName;
+ load: anObject fromAppDataFileNamed: (const char *)aFileName;

- setFileObject: aFileObject;
- loadObject: anObject;

- setTemplateProbeMap: (id <ProbeMap>)probeMap;
- updateCache: exampleTarget;
@end
