//
//  �PROJECTNAME�GUI.h
//  �PROJECTNAME� Swarm Application
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Swarm/OpenStepSwarm.h>
#import "�PROJECTNAME�Model.h"

@interface �PROJECTNAME�GUI : OpenStepGUISwarm {
	�PROJECTNAME�Model *mainModel;
	
	int displayFrequency;
	id displayActions;
	id displaySchedule;
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- (void)updateGraphicalDisplays;

@end
