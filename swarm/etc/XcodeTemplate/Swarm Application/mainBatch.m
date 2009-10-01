//
//  mainBatch.m
//  ÇPROJECTNAMEÈ
//
//  Created by ÇFULLUSERNAMEÈ on ÇDATEÈ.
//  Copyright ÇORGANIZATIONNAMEÈ ÇYEARÈ. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <Swarm/simtools.h>
#import "ÇPROJECTNAMEÈBatch.h"

int main(int argc, const char *argv[])
{
	NSAutoreleasePool *pool = [NSAutoreleasePool new];

	// Swarm initialization: all Swarm apps must call this first.
	initSwarm (argc, argv);

    // Process any command line arguments
    
    // We assume just a single optional argument which specifies a parameter file
    NSArray *args = [[NSProcessInfo processInfo] arguments];
    NSString *paramFile;
    if ([args count] == 2)
        paramFile = [args objectAtIndex: 1];
    else
        paramFile = [NSString stringWithFormat: @"SwarmModel.%@", @"ÇPROJECTNAMEÈ"];
    
    NSDictionary *simParameters = [NSMutableDictionary dictionaryWithContentsOfFile: paramFile];
    if (!simParameters) {
        NSLog(@"Could not load parameter file: %@\n", paramFile);
        exit(1);
    }
    
    // Create the top-level batch Swarm and run the simulation
    ÇPROJECTNAMEÈBatch *theTopLevelSwarm = [ÇPROJECTNAMEÈBatch create: globalZone withParameters: simParameters];
    [theTopLevelSwarm createEnd];
    
    [theTopLevelSwarm buildObjects];
    [theTopLevelSwarm buildActions];
    [theTopLevelSwarm activateIn: nil];
    [theTopLevelSwarm go];
    
	[pool release];
	return 0;
}
