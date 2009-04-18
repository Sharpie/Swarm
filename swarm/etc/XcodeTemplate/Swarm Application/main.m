//
//  main.m
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/simtools.h>     // initSwarm () and swarmGUIMode

int main(int argc, const char *argv[])
{
	// Swarm initialization: all Swarm apps must call this first.
	initSwarm (argc, argv);

    return NSApplicationMain(argc,  (const char **) argv);
}
