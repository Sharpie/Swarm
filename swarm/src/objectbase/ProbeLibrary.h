// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <defobj/Create.h>

@interface ProbeLibrary : CreateDrop {
	id myZone ;
	id classMap ;
}

-createEnd ;

-getProbeMapFor: (Class) aClass ;
-getCompleteProbeMapFor: (Class) aClass ;
-getCompleteVarMapFor: (Class) aClass ;
-getProbeForVariable: (char *) aVariable inClass: (Class) aClass ;
-getProbeForMessage: (char *) aVariable inClass: (Class) aClass ;
-setProbeMap: aMap For: (Class) aClass ;
@end
