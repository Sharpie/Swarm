//
//  �PROJECTNAME�GUI.h
//  �PROJECTNAME� Swarm Application
//
//  Created by �FULLUSERNAME� on �DATE�.
//  Copyright �ORGANIZATIONNAME� �YEAR�. All rights reserved.
//

#import "�PROJECTNAME�Model.h"
#import <Swarm/OpenStepSwarm.h>
#import <Swarm/Value2dDisplay.h>
#import <Swarm/Object2dDisplay.h>

@interface �PROJECTNAME�GUI : OpenStepGUISwarm {
  �PROJECTNAME�Model *mainModel;
	
  int displayFrequency;
  id displayActions;
  id displaySchedule;

  // displays
  // id <Object2dDisplay> someDisplay;
}

+ createBegin: aZone;
- buildObjects;
- buildActions;
- activateIn: swarmContext;

- mainModel;

// - someDisplay;

- (void)updateGraphicalDisplays;

@end
