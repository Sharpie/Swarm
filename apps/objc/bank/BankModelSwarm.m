#import <stdlib.h>
#import <simtools.h>
#import "BankModelSwarm.h"

@implementation BankModelSwarm

+createBegin: (id) aZone {
  BankModelSwarm * obj;
  id <ProbeMap> probeMap;

  obj = [super createBegin: aZone];

  obj->population = 20;
  obj->averageIncome = 100 ;
  obj->probIOP = 0.7 ;
  obj->probIOPSuccess = 0.5 ;
  obj->IOPmultiplier = 2.0 ;
  obj->probEncounter = 1.0 ;

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [self class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "population"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "averageIncome"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "probIOP"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "probIOPSuccess"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "IOPmultiplier"
				    inClass: [self class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "probEncounter"
				    inClass: [self class]]];

  [probeMap addProbe: [[probeLibrary getProbeForMessage: "Randomize"
			     inClass: [self class]]
			setHideResult: 1]];

  [probeLibrary setProbeMap: probeMap For: [self class]];

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [FEntity class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "borrowLink"
				    inClass: [FEntity class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "investLink"
				    inClass: [FEntity class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "prevCapital"
				    inClass: [FEntity class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "prevROI"
				    inClass: [FEntity class]]];
  [probeMap addProbe: [probeLibrary getProbeForMessage: "countBorrowers"
			     inClass: [FEntity class]]];

  [probeLibrary setProbeMap: probeMap For: [FEntity class]];

  probeMap = [EmptyProbeMap createBegin: aZone];
  [probeMap setProbedClass: [DiGraphLink class]];
  probeMap = [probeMap createEnd];

  [probeMap addProbe: [probeLibrary getProbeForVariable: "from"
				    inClass: [DiGraphLink class]]];
  [probeMap addProbe: [probeLibrary getProbeForVariable: "to"
				    inClass: [DiGraphLink class]]];

  [probeLibrary setProbeMap: probeMap For: [DiGraphLink class]];
  
  return obj;
}


-createEnd {
  return [super createEnd];
}

-getTheFNet {
  return theFNet ;
}

-getEntityList {
  return entityList ;
}

-(double) getProbEncounter {
  return probEncounter ;
}

-(double) getProbIOP {
  return probIOP ;
}

-(double) getProbIOPSuccess {
  return probIOPSuccess ;
}
	
-(double) getIOPmultiplier {
  return IOPmultiplier ;
}

-getRandomFEntity {
  return [entityList atOffset: [uniformIntRand getIntegerWithMin: 0
                      withMax: ([entityList getCount] - 1)]
  ] ;
}

- setGraphCanvas: aCanvas
{
  graphCanvas = aCanvas ;
  if (theFNet)
    [theFNet setCanvas: aCanvas];
  return self ;
}

-buildObjects {
  int i;
  char *aName ;

  [super buildObjects];

  theFNet = [FNet createBegin: [self getZone]] ;
  [theFNet setCanvas: graphCanvas] ;
  [theFNet createEnd] ;

  for(i = 0 ; i < population ; i++){
    aName = malloc(10) ;
    sprintf(aName,"FE%d",i) ;
    [theFNet addNode: [[[[[[FEntity createBegin: [self getZone]]
                       setModel: self]
                       setFixedIncome: averageIncome]
                       setTotalAgentNum: population]
                       setEntityName: aName]
                       createEnd]] ;
  }

  entityList = [theFNet getNodeList] ;
  
  return self;
}

-buildActions {
  [super buildActions];

  modelActions = [ActionGroup create: [self getZone]];
  [modelActions createActionForEach: entityList message: M(encounter)];
  [modelActions createActionForEach: entityList message: M(invest)];
  [modelActions createActionForEach: entityList message: M(generateIOP)];
  [modelActions createActionForEach: entityList message: M(lend)];
  
  modelSchedule = [Schedule createBegin: [self getZone]];
  [modelSchedule setRepeatInterval: 1];
  modelSchedule = [modelSchedule createEnd];
  [modelSchedule at: 0 createAction: modelActions];

  return self;
}

-activateIn: (id) swarmContext {

  [super activateIn: swarmContext];

  [modelSchedule activateIn: self];

  return [self getSwarmActivity];
}

-Randomize {
  
  int list_length ;
  id index, entity ;

  list_length = [entityList getCount] ;
  index = [entityList begin: [self getZone]] ;

  while( (entity = [index next]) ){
    [entity transferInvestLinkTo: 
       [entityList atOffset: [uniformIntRand getIntegerWithMin: 0
                                                       withMax: list_length-1]]
    ] ;
    [entity transferBorrowLinkTo: 
       [entityList atOffset: [uniformIntRand getIntegerWithMin: 0
                                                       withMax: list_length-1]]
    ] ;
  }

  [index drop] ;

  return self ;
}

@end
