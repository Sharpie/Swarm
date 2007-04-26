// DrugWar model. Copyright © 2000 Swarm Development Group
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.
#import <objectbase/Swarm.h>
#import <gui.h>

@interface Society: Swarm
{
  int numberOfMembers;
  id members; // Set of members of the society
  id <Canvas> awarenessCanvas;
  unsigned nodeSize;  // defined if guimode is on

  BOOL randomNet;
  // if randomNet, then need these
  int maxAwarenessLinks;
  int minAwarenessLinks;
  float minFactRatio, maxFactRatio;
  float minPressureRatio, maxPressureRatio;
  float initMembershipProb;

  // if not randomNet, then we need these
  // not implemented, yet

  int numberOfMemeFactories;
  id institutions; // Set of Meme Factories

  // if randomNet, we need these
  int minInitProfit, maxInitProfit;
  float minInitIR, maxInitIR, minInitIRIncr, maxInitIRIncr;
  int minAcceptableProfit, maxAcceptableProfit;

  float defectorProbPunishMin, defectorProbPunishMax;
  float opponentProbPunishMin, opponentProbPunishMax;

  float minProbRemoval, maxProbRemoval;
  int maxRecruitNumber;

  unsigned paroleTimeMin;
  unsigned paroleTimeMax;

  id mfPlan;
  id mfRecruit;
  id mfOrganize;
  id mfEvaluate;
   id schedule;

  id modelZone;
}
-setNodeSize: (unsigned) anInt;
-build;
-buildAgents;
-buildMemeFactories;
-buildActivity;
-activateIn: (id) swarmContext;
-go;
-takeData;
-setCanvas: (id) aCanvas;
-drawNets;
@end
