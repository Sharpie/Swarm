#import <objectbase.h>
#import <activity.h>
#import <collections.h>
#import <simtools.h>
#import <analysis.h>
#import "BankModelSwarm.h"

@interface BankObserverSwarm : GUISwarm {
  int displayFrequency;
  int interactiveGraph;
  id displayActions;	
  id displaySchedule;

  BankModelSwarm * bankModelSwarm;

  id graphCanvas ;

  BLTGraph * investorGraph;			  
  GraphElement * investorData;			  
  Entropy * investorEntropy;   		  
  id <ActiveGraph> investorGrapher;		  
  GraphElement * borrowerData;			  
  Entropy * borrowerEntropy;   		  
  id <ActiveGraph> borrowerGrapher;		  
  id activeBanks ;
  
  /*
  XColormap * colormap;				  // allocate colours
  ZoomRaster * worldRaster;			  // 2d display widget


  Value2dDisplay * heatDisplay;			  // display the heat
  Object2dDisplay * bankDisplay;		  // display the banks
  */
}

+createBegin: (id) aZone;
-createEnd;
-buildObjects;
-buildActions;
-activateIn: (id) swarmContext;

-redistribute ;

@end
