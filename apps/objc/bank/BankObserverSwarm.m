v// Copyright (C) 1996-1998 Santa Fe Institute.
#import "BankObserverSwarm.h"
#import "BankModelSwarm.h"
#import <simtoolsgui.h> // ActiveGraph
#import <analysis.h> // Entropy

@implementation BankObserverSwarm

// createBegin: here we set up the default observation parameters.
+ createBegin: aZone
{
  BankObserverSwarm *obj;
  id <ProbeMap> probeMap;
  
  obj = [super createBegin: aZone];

  obj->displayFrequency = 1;
  obj->interactiveGraph = 1;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "displayFrequency"
				    inClass: [self class]]];
  
  [probeMap addProbe: [probeLibrary getProbeForVariable: "interactiveGraph"
				    inClass: [self class]]];
  
  [probeMap addProbe: [[probeLibrary getProbeForMessage: "redistribute"
                                     inClass: [self class]]
			setHideResult: 1]];
  
  [probeLibrary setProbeMap: probeMap For: [self class]];

  return obj;
}

- createEnd
{
  return [super createEnd];
}

- _graphCanvasDeath_: caller
{
  [graphCanvas drop];
  graphCanvas = nil;
  [bankModelSwarm setGraphCanvas: nil];

  return self;
}

- buildObjects
{
  id modelZone;	
  
  [super buildObjects];
  
  modelZone = [Zone create: [self getZone]];
  bankModelSwarm = [BankModelSwarm create: modelZone];
  
  // Now create probe objects on the model and ourselves. This gives a
  // simple user interface to let the user change parameters.
  CREATE_ARCHIVED_PROBE_DISPLAY (bankModelSwarm);
  CREATE_ARCHIVED_PROBE_DISPLAY (self);
  
  [actionCache waitForControlEvent];
  if ([controlPanel getState] == ControlStateQuit)
    return self;

  if (interactiveGraph)
    {
      graphCanvas = [Canvas createBegin: [self getZone]];
      SET_WINDOW_GEOMETRY_RECORD_NAME (graphCanvas);
      graphCanvas = [graphCanvas createEnd];

      [graphCanvas enableDestroyNotification: self
                   notificationMethod: @selector(_graphCanvasDeath_:)];

      [[graphCanvas setHeight: 500] setWidth: 500];
      [graphCanvas setWindowTitle: "The Emergence Of Banking"];
      [graphCanvas packFillLeft: 1];
      GUI_UPDATE_IDLE_TASKS();
    }
  
  [bankModelSwarm setGraphCanvas: graphCanvas];
  [bankModelSwarm buildObjects];

  activeBanks = [EZGraph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (activeBanks);
  [activeBanks setTitle: "Active Banks"];
  [activeBanks setAxisLabelsX: "Time" Y: "Banks"];
  activeBanks = [activeBanks createEnd];

  [activeBanks createTotalSequence: "Survivors" 
                      withFeedFrom: [bankModelSwarm getEntityList]
                       andSelector: M(incident)];

  investorGraph = [Graph createBegin: [self getZone]];
  SET_WINDOW_GEOMETRY_RECORD_NAME (investorGraph);
  investorGraph = [investorGraph createEnd];

  [investorGraph setTitle: "Entropy of investor link distribution"];
  [investorGraph setAxisLabelsX: "time" Y: "prop. of Max. Entropy"];
  [investorGraph pack];

  investorData = [investorGraph createElement];
  [investorData setLabel: "H(investors)"];

  investorEntropy = [Entropy createBegin: [self getZone]];
  [investorEntropy setCollection: [bankModelSwarm getEntityList]];
  [investorEntropy setProbedSelector: M(getInvestorLinkProbability)];
  investorEntropy = [investorEntropy createEnd];

  investorGrapher = [ActiveGraph createBegin: [self getZone]];
  [investorGrapher setElement: investorData];
  [investorGrapher setDataFeed: investorEntropy]; // chain them up
  [investorGrapher setProbedSelector: M(getEntropy)];
  investorGrapher = [investorGrapher createEnd];

  borrowerData = [investorGraph createElement];
  [borrowerData setLabel: "H(borrowers)"];

  borrowerEntropy = [Entropy createBegin: [self getZone]];
  [borrowerEntropy setCollection: [bankModelSwarm getEntityList]];
  [borrowerEntropy setProbedSelector: M(getBorrowLinkProbability)];
  borrowerEntropy = [borrowerEntropy createEnd];

  borrowerGrapher = [ActiveGraph createBegin: [self getZone]];
  [borrowerGrapher setElement: borrowerData];
  [borrowerGrapher setDataFeed: borrowerEntropy]; // chain them up
  [borrowerGrapher setProbedSelector: M(getEntropy)];
  borrowerGrapher = [borrowerGrapher createEnd];

  // All done - we're ready to build a schedule and go.

  return self;
}  

- redistribute
{
  [[bankModelSwarm getTheFNet] redistribute];
  return self;
}

- buildActions
{
  [super buildActions];
  
  [bankModelSwarm buildActions];
  
  displayActions = [ActionGroup create: [self getZone]];

  [displayActions createActionTo: activeBanks         message: M(step)];
  [displayActions createActionTo: investorEntropy     message: M(update)];
  [displayActions createActionTo: investorGrapher     message: M(step)];
  [displayActions createActionTo: borrowerEntropy     message: M(update)];
  [displayActions createActionTo: borrowerGrapher     message: M(step)];
  [displayActions createActionTo: probeDisplayManager message: M(update)];
  [displayActions createActionTo: actionCache         message: M(doTkEvents)];

  displaySchedule = [Schedule createBegin: [self getZone]];
  [displaySchedule setRepeatInterval: displayFrequency];
  displaySchedule = [displaySchedule createEnd];
  [displaySchedule at: 0 createAction: displayActions];

  return self;
}  

- activateIn: swarmContext
{

  [super activateIn: swarmContext];

  [bankModelSwarm activateIn: self];

  [displaySchedule activateIn: self];

  return [self getSwarmActivity];
}

// You could override the "go" method here if you want something special
// to happen when the model and observer actually start running. But
// the default GUISwarm go is probably good enough.

@end
