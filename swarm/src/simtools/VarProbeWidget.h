// Swarm library. Copyright (C) 1996 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <tkobjc.h>
#import <swarmobject.h>

@interface VarProbeWidget : Widget {
	id     myObject ;
	VarProbe *myProbe ;
	Frame  *myLeft  ;
	Frame  *myRight ;
	Label  *myLabel ;
	int maxLabelWidth ;
        int interactive ;
	Entry  *myEntry ;
}

+createBegin: aZone ;
-setObject: obj ;
-setProbe: (Probe *) the_probe;
-setMyLeft: aFrame ;
//-setInteractive: (int) val ;
-setMyRight: aFrame ;
-setMaxLabelWidth: (int) width ;
-createEnd ;
-pack ;
-setValue;
-update ;
-Spawn ;
-idReceive ;
-(char *)package ;
-(const char *)getId ;
@end
