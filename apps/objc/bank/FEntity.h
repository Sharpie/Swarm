#import <graph.h>

@interface FEntity: DiGraphNode {
  int prevROI ;
  int prevCapital ;
  int capital ;
  int investCount ;
  int loanCount ;
  int IOPbool ;
  int fixedIncome ;
  int totalAgentNum ;
  id investLink ;
  id borrowLink ;
  id model ;
}

-(int) getPrevROI ;
-(int) getPrevCapital ;
-setEntityName: (char *) the_name ;
-setFixedIncome: (int) aVal ;
-setModel: aModelSwarm ;
-setTotalAgentNum: (int) total ;
-(int)investingIn: anFEntity ;
-(int)borrowingFrom: anFEntity ;

-makeInvestLinkTo: aFEntity ;
-makeBorrowLinkTo: aFEntity ;
-transferInvestLinkTo: aFEntity ;
-transferBorrowLinkTo: aFEntity ;

-encounter ;
-invest ;
-generateIOP ;
-lend ;
-(double) generateROI: (double) investment ;

-(int) incident ;
-(int) countInvestors ;
-(int) countBorrowers ; // Note: we are only concerned with the links
                        // (whether or not the node actually borrowed 
                        // on a given turn)...

-(double) getInvestorLinkProbability ;
-(double) getBorrowLinkProbability ;

@end
