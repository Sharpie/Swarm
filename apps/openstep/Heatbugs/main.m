//
//  main.m
//  Heatbugs
//
//  Created by Scott Christley on 4/17/09.
//  Copyright __MyCompanyName__ 2009. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/simtools.h>     // initSwarm () and swarmGUIMode

int main(int argc, const char *argv[])
{
	// Swarm initialization: all Swarm apps must call this first.
	initSwarm (argc, argv);

    return NSApplicationMain(argc,  (const char **) argv);
}
