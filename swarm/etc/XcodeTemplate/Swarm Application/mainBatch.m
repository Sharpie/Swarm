//
//  mainBatch.m
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/simtools.h>     // initSwarm () and swarmGUIMode

int main(int argc, const char *argv[])
{
	NSAutoreleasePool *pool = [NSAutoreleasePool new];

	// Swarm initialization: all Swarm apps must call this first.
	initSwarm (argc, argv);

	[pool release];
	return 0;
}
