#import <graph.h>

@interface FEntity: DiGraphNode
{
  int prevROI;
  int prevCapital;
  int capital;
  int investCount;
  int loanCount;
  int IOPbool;
  int fixedIncome;
  int totalAgentNum;
  id investLink;
  id borrowLink;
  id model;
}

- setCanvas : aCanvas;
- (int) getPrevROI;
- (int) getPrevCapital;
- setEntityName: (const char *) the_name;
- setFixedIncome: (int)aVal;
- setModel: aModelSwarm;
- setTotalAgentNum: (int) total;

- (BOOL)investingIn: anFEntity;
- (BOOL)borrowingFrom: anFEntity;

- makeInvestLinkTo: aFEntity;
- makeBorrowLinkTo: aFEntity;
- transferInvestLinkTo: aFEntity;
- transferBorrowLinkTo: aFEntity;

- encounter;
- invest;
- generateIOP;
- lend;
- (double)generateROI: (double) investment;

- (BOOL)incident;
- (int)countInvestors;

// Note: we are only concerned with the links
// (whether or not the node actually borrowed 
// on a given turn)...
- (int)countBorrowers; 

- (double)getInvestorLinkProbability;
- (double)getBorrowLinkProbability;

@end
