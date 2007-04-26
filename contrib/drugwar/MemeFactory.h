// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import "graph/DiGraphNode.h"

@interface MemeFactory: DiGraphNode
{
@public
  id world;

  id networkMembers;
  int sizeOfNetwork;
  id defectors;
  int profitLastCycle;
  float incentiveRatio;
  int maxRecruitNumber;
  float IRIncrement;
  int acceptableProfit;
  int membershipTag;

  float defectorPunishProb;
  float opponentPunishProb;
  float probRemoval; // non-punishment
  // number of people who were in the network, defected, and were punished
  unsigned numberDeactivated;

  unsigned paroleTimeMin;
  unsigned paroleTimeMax;

  id memeZone;
}

+ createBegin: aZone;
- setWorld: aList;
- setMembershipTag: (int)aTag;
- createEnd;
- plan;
- recruit;
- organize;
- evaluate;
- evalSuccess;
- evalDefectors;
- evalOpponents;
- setName: (const char *)aName;
- (const char *)getInstanceName;
- setNodeType: aNodeType;
@end
