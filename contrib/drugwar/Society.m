// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import <simtools.h> // swarmGUIMode, ObjectLoader, UName
#import <random.h>
#import <activity.h>
#import "graph/graph.h"
#import <gui.h>
#import "Society.h"
#import "MemeFactory.h"
#import "Agent.h"
@implementation Society

-setNodeSize: (unsigned) anInt {
  nodeSize = anInt;
  return self;
}

- build
{
  modelZone = [Zone create: [self getZone]];
  if (swarmGUIMode != 1)
    [ObjectLoader load: self fromAppDataFileNamed: "Society-Data.001"];
  [self buildAgents];
  [self buildMemeFactories];
  [self buildActivity];
  return self;
}

- buildAgents
{
  id <RandomBitDist> flip;
  id <SWB1gen> flipGen;
  int incr;

  flipGen = [SWB1gen create: modelZone setStateFromSeed: (getpid()*time(0))];
  flip = [RandomBitDist create: modelZone setGenerator: flipGen];

  members = [Set createBegin: modelZone];
  //[members setDupOption: DupIsError];
  members = [members createEnd];

  if (randomNet) {
    Agent * me;
    id <Index> index1;
    Agent * tempMember;
    id <UName> nameMaker;
    unsigned newX, newY;

    nameMaker = [UName create: modelZone setBaseName: "A"];
    
    newX = newY = 0;
    // randomly draw population
    for (incr=0; incr<numberOfMembers; incr++) {
      // create a member
      tempMember = [Agent createBegin: modelZone];
      // give her a name
      [tempMember setName: [nameMaker getNewName]];

      // initialize her state

      [tempMember setInitialStateToFact: [flip getCoinToss]
		  coercion: [flip getCoinToss]
		  socialPressure: [flip getCoinToss]];
      // initialize ratios
      [tempMember setFactRatioTo: [uniformDblRand 
				    getDoubleWithMin: minFactRatio
				    withMax: maxFactRatio]];
      [tempMember setSocialPressureRatioTo: [uniformDblRand
					getDoubleWithMin: minPressureRatio
					withMax: maxPressureRatio]];
      // incentive ratio will be set by recruitment phase of mf's

      // do this only if there's a canvas
      if (awarenessCanvas != nil) {
	int nodesPerRow;

	[tempMember setCanvas: awarenessCanvas]; 

	nodesPerRow = [awarenessCanvas getWidth]/nodeSize;
	if (incr % nodesPerRow == 0) {
	  newX = 0;
	  newY += nodeSize;
	}
	newX += nodeSize;
	[tempMember setCanvasPositionToX: newX Y: newY];

	tempMember = [tempMember createEnd];
      }
      else
	tempMember = [tempMember createEnd];


      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] <=
	  initMembershipProb)
	[tempMember setMembershipTo: 0];
      else
	[tempMember setMembershipTo: 1];

      // put her in the population
      [members add: tempMember];
    }

    [nameMaker drop];
    // set up the awareness nets for each agent
    index1 = [members begin: modelZone];
    while (([index1 getLoc] != End) &&
	   ((me = [index1 next]) != nil)) {
      id <List> tempList;
      id <Index> index2;
      int tlIncr;
      
      // build the tempList
      tempList = [List create: modelZone];
      index2 = [members begin: modelZone];
      while (([index2 getLoc] != End) && 
	     ((tempMember = [index2 next]) != nil)) {
	if (tempMember != me) 
	  [tempList addLast: tempMember];
      }
     [index2 drop];

      // sample from tempList w/o replacement
      for (tlIncr = 0;
	   (tlIncr < [uniformUnsRand 
		       getUnsignedWithMin: minAwarenessLinks
		       withMax: maxAwarenessLinks]) &&
	     (tlIncr < [tempList getCount]);
	   tlIncr++) {
	tempMember = [tempList atOffset: [uniformUnsRand getUnsignedWithMin: 0L
					     withMax: [tempList getCount]-1]];
	[me toAwarenessAdd: tempMember];
	[tempList remove: tempMember];
      }
      [tempList drop];
    }

    [index1 drop];
  }
  else {
    fprintf(stderr, "Custom File-read nets not implemented yet.\n");
    exit(1);
  }
  [flip drop];
  [flipGen drop];

  [self drawNets];

  return self;
}


-buildMemeFactories {
  int incr;
  MemeFactory * tempMF;

  institutions = [List createBegin: modelZone];
  //[institutions setDupOption: DupIsError];
  institutions = [institutions createEnd];

  for (incr=0; incr<numberOfMemeFactories; incr++) {
    tempMF = [MemeFactory createBegin: modelZone];
    tempMF->profitLastCycle = [uniformIntRand 
				getIntegerWithMin: minInitProfit
				withMax: maxInitProfit];
    tempMF->incentiveRatio = [uniformDblRand
			       getDoubleWithMin: minInitIR
			       withMax: maxInitIR];
    tempMF->IRIncrement = [uniformDblRand
			    getDoubleWithMin: minInitIRIncr
			    withMax: maxInitIRIncr];
    tempMF->acceptableProfit = [uniformIntRand
				 getIntegerWithMin: minAcceptableProfit
				 withMax: maxAcceptableProfit];
    tempMF->defectorPunishProb = [uniformDblRand
			       getDoubleWithMin: defectorProbPunishMin
			       withMax: defectorProbPunishMax];
    tempMF->opponentPunishProb = [uniformDblRand
			       getDoubleWithMin: opponentProbPunishMin
			       withMax: opponentProbPunishMax];
    tempMF->probRemoval = [uniformDblRand
			    getDoubleWithMin: minProbRemoval
			    withMax: maxProbRemoval];
    tempMF->maxRecruitNumber = maxRecruitNumber;

    tempMF->paroleTimeMin = paroleTimeMin;
    tempMF->paroleTimeMax = paroleTimeMax;

    [tempMF setWorld: members];

    [tempMF setName: (incr == 0? "Cartel" : "Cops")];

    if (awarenessCanvas != nil) {
      [tempMF setNodeType: RectangleNode];
      [tempMF setCanvas: awarenessCanvas];
    }

    tempMF = [tempMF createEnd];
    [tempMF setMembershipTag: incr];

    [institutions addLast: tempMF];

  }
  return self;
}


- buildActivity
{
  mfPlan = [ActionGroup createBegin: modelZone];
  // [mfPlan setDefaultOrder: Randomized];
  mfPlan = [mfPlan createEnd];
  [mfPlan createActionForEach: institutions message: M(plan)];

  mfRecruit = [ActionGroup createBegin: modelZone];
  // [mfRecruit setDefaultOrder: Randomized];
  mfRecruit = [mfRecruit createEnd];
  [mfRecruit createActionForEach: institutions message: M(recruit)];

  mfOrganize = [ActionGroup createBegin: modelZone];
  // [mfOrganize setDefaultOrder: Randomized];
  mfOrganize = [mfOrganize createEnd];
  [mfOrganize createActionForEach: institutions message: M(organize)];

  mfEvaluate = [ActionGroup createBegin: modelZone];
  // [mfEvaluate setDefaultOrder: Randomized];
  mfEvaluate = [mfEvaluate createEnd];
  [mfEvaluate createActionForEach: institutions message: M(evaluate)];

  schedule = [Schedule createBegin: modelZone];
  [schedule setRepeatInterval: 7];
  schedule = [schedule createEnd];
  [schedule at: 0 createAction: mfPlan];
  [schedule at: 1 createAction: mfRecruit];
  [schedule at: 2 createAction: mfOrganize];
  [schedule at: 3 createActionForEach: members message: M(evaluate)];
  [schedule at: 4 createActionForEach: members message: M(act)];
  [schedule at: 5 createAction: mfEvaluate];
  [schedule at: 6 createActionTo: self message: M(takeData)];

  return self;
}
-activateIn: (id) swarmContext {
  [super activateIn: swarmContext];
  [schedule activateIn: self];
  return [self getActivity];
}

-go {
  [[self getActivity] run];
  return [[self getActivity] getStatus];
}

-takeData {
  fflush(0);
  return self;
}

-setCanvas: (id) aCanvas {
  awarenessCanvas = aCanvas;
  return self;
}

-drawNets {
  Agent * folk;
  Agent * friend;
  id <Index> memberNdx, awareNdx;

  // loop over the members of the society and make links between them
  memberNdx = [members begin: modelZone];
  while (([memberNdx getLoc] != End) &&
	 ((folk = [memberNdx next]) != nil)) {

    // loop over the people in folk's awarenessNet
    awareNdx = [[folk getAwarenessNet] begin: modelZone];
    while (([awareNdx getLoc] != End) &&
	   ((friend = [awareNdx next]) != nil))
      [folk makeLinkTo: friend];
    [awareNdx drop];
  }
  [memberNdx drop];
  return self;
}
@end


