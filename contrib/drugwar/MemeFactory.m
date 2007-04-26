// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import <collections.h>
#import <simtoolsgui.h>
#import <random.h>
#import "MemeFactory.h"
#import "Agent.h"

@implementation MemeFactory

+ createBegin: aZone
{
  MemeFactory * newMe;

  newMe = [super createBegin: aZone];
  newMe->memeZone = [Zone create: aZone];

  return newMe;
}

- setWorld: aList
{
  world = aList;
  return self;
}

- setMembershipTag: (int)aTag
{
  membershipTag = aTag;
  if (canvas != nil) {
    if (aTag == 0)
      [nodeItem setColor: "red"];
    else
      [nodeItem setColor: "blue"];
    GUI_UPDATE_IDLE_TASKS();
  }
 
  return self;
}

- createEnd
{
  networkMembers = [Set createBegin: memeZone];
  //[networkMembers setDupOption: KeepCountOnly];
  //[networkMembers setKeyFunction: (id (*)(id)) aFunction]; // use names
  networkMembers = [networkMembers createEnd];

  defectors = [Set createBegin: memeZone];
  // count the times a defector is registered as defecting
  //[defectors setDupOption: KeepCountOnly];
  //[defectors setKeyFunction: (id (*)(id))aFunction]; // use names
  defectors = [defectors createEnd];

  return [super createEnd];
}

-plan
{
  return self;
}

// The recruitment function could result in severe biases
// dependent on the order in which viing meme factories recruit
// a possible way around it would be to disallow one mf to recruit
// another mf's potential member or to have two make the whole 
// incentive program agent motivated, such that the agents have 
// to be aware of what mf's are out there and ask each (or a 
// public info source) for info.
- recruit
{
  Agent *folk;
  id <Index> index;
  id <List> tempList;
  int incr;

  tempList = [List create: memeZone];
  index = [world begin: memeZone];
  while (([index getLoc] != End) &&
	 ((folk = [index next]) != nil))
    {
      if (![networkMembers contains: folk] &&
          [folk getMembership] != -1)
        [tempList addLast: folk];
    }
  [index drop];

  incr = 0;
  while ((incr < maxRecruitNumber) && 
	 (incr < [tempList getCount]))
    {
      // get a random member of the world who's not in our net already
      if ([tempList getCount] > 1) 
        folk = [tempList atOffset: 
                           [uniformUnsRand getUnsignedWithMin: 0L
                                           withMax: [tempList getCount] - 1]];
      else
        folk = [tempList getFirst];
      
      [tempList remove: folk];
      [folk setIncentiveRatio: incentiveRatio];
      incr++;
    }
  [tempList drop];
  return self;
}

- organize
{
  id worldIndex, person;

  worldIndex = [world begin: memeZone];
  while (([worldIndex getLoc] != End) &&
	 ((person = [worldIndex next]) != nil))
    {
      // if this person has elected to go with me, 
      //  and she isn't already in my network, then add her
      if (([person getMembership] == membershipTag) &&
          ![networkMembers contains: person])
        {
          [networkMembers add: person];
        }
      
    }
  [worldIndex drop];
  return self;
}

- evaluate
{
  [self evalDefectors];
  [self evalOpponents];
  [self evalSuccess];   // might be overridden
  return self;
}

- evalSuccess
{
  // right now, success will be determined by whether or not the 
  //  network is bigger this cycle.
  profitLastCycle = [networkMembers getCount] - sizeOfNetwork;
  sizeOfNetwork = [networkMembers getCount];

  if (profitLastCycle <= acceptableProfit)
    incentiveRatio = incentiveRatio + IRIncrement;
  if (incentiveRatio > 1.0) incentiveRatio = 1.0;

  return self;
}

- evalDefectors
{
  id index, member;
  id tempList;
  
  index = [networkMembers begin: memeZone];
  while (([index getLoc] != End) &&
	 ((member = [index next]) != nil))
    {
      // if he's on our list and membership shows the tag for the other guys
      //  and it shows that he's not inactive,
      //  then he's a defector.  The set will keep track of how many 
      //  times the guy's registered as a defector.
      if (([member getMembership] != membershipTag) &&
          ([member getMembership] != -1))
        {
          [defectors add: member];  //add to the set of defectors
        }
    }
  [index drop];
  
  // of the defectors, we will remove some, kill some, and leave 
  //  some alone
  tempList = [List create: memeZone];
  index = [defectors begin: memeZone];
  while (([index getLoc] != End) &&
	 ((member = [index next]) != nil))
    {
      // remove some from the network with no punishment
      if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < 
          defectorPunishProb)
        {
          [member setMembershipTo: -1]; // set member inactive
          [member setParoleTime: [uniformUnsRand getUnsignedWithMin: paroleTimeMin 
                                                 withMax: paroleTimeMax]];
          numberDeactivated++;
          [networkMembers remove: member];
          [tempList addLast: member]; // remove him when we're out of this loop
        }
      else if ([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] > 
               probRemoval)
        {
          [networkMembers remove: member];
          [tempList addLast: member]; // remove him later
        }
    }
  [index drop];
  
  // cleanup
  index = [tempList begin: memeZone];
  while (([index getLoc] != End) &&
	 ((member = [index next]) != nil))
    [defectors remove: member];
  [index drop];
  [tempList drop];
  
  return self;
}

- evalOpponents {
  id <Index> worldNdx = [world begin: scratchZone];
  Agent *folk;

  while ( ([worldNdx getLoc] != End)
          && ( (folk = [worldNdx next]) != nil) ) {
    int m=[folk getMembership];
    if ( ( m != -1 ) && ( m != membershipTag ) ) {
      if ( [uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] <= opponentPunishProb ) {
        [folk setMembershipTo: -1];
        [folk setParoleTime: [uniformUnsRand getUnsignedWithMin: paroleTimeMin 
                                             withMax: paroleTimeMax]];
      }
    }
  }
  
  return self;
}

- setName: (const char *)aName
{
  label = aName;
  return self;
}

- (const char *)getInstanceName
{
  return label;
}

- setNodeType: aNodeType
{
  nodeType = aNodeType;
  return self;
}
@end
