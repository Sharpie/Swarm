#import "FEntity.h"
#import "ILink.h"
#import "BLink.h"
#import "BankModelSwarm.h"

#import <random.h>

#include <math.h>

@implementation FEntity

- setCanvas: aCanvas
{
  [super setCanvas: aCanvas];
  if (investLink)
    [investLink setCanvas: aCanvas];
  if (borrowLink)
    [borrowLink setCanvas: aCanvas];
  return self;
}

- setFixedIncome: (int)aVal
{
  fixedIncome = aVal;
  return self;
}

- setTotalAgentNum: (int)total
{
  totalAgentNum = total;
  return self;
}

- makeInvestLinkTo: aFEntity
{
  investLink = [[[[ILink createBegin: [self getZone]] 
                   setCanvas: canvas]
                  setFrom: self To: aFEntity] 
                 createEnd];
  
  return self;
}

- makeBorrowLinkTo: aFEntity
{
  borrowLink = [[[[BLink createBegin: [self getZone]] 
                   setCanvas: canvas]
                  setFrom: self To: aFEntity] 
                 createEnd];

  return self;
}

- transferInvestLinkTo: aFEntity
{
  [investLink drop];
  [self makeInvestLinkTo: aFEntity];
  return self;
}

- transferBorrowLinkTo: aFEntity
{
  [borrowLink drop];
  [self makeBorrowLinkTo: aFEntity];
  return self;
}

- createEnd
{
  [super createEnd];
  
  [self makeInvestLinkTo: self];
  [self makeBorrowLinkTo: self];
  
  IOPbool = 0;
  loanCount = 0;
  investCount = 0;
  capital = 0;
  
  return self;
}

- (int)getPrevROI
{
  return prevROI;
}

- (int)getPrevCapital
{
  return prevCapital;
}

- (const char *)getInstanceName
{
  return label;
}

- setEntityName: (const char *)the_name
{
  label = the_name;
  return self;
}

- setModel: aModelSwarm
{
  model = aModelSwarm;
  return self;
}

- encounter
{ 
  id someone;
  
  if([uniformDblRand getDoubleWithMin: 0.0
                     withMax: 1.0] < [model getProbEncounter])
    {
      someone = [model getRandomFEntity];
      if([someone getPrevROI] > [[investLink getTo] getPrevROI])
        [self transferInvestLinkTo: someone];
      if([someone getPrevCapital] > [[borrowLink getTo] getPrevCapital])
        [self transferBorrowLinkTo: someone];
    }
  
  return self;
}

- collectInvestment: (int)amount
{
  investCount++;
  capital += amount;
  return self;
}

- invest
{
  [[investLink getTo] collectInvestment: fixedIncome];
  return self;
}

- requestLoan
{
  loanCount++;
  return self;
}

- generateIOP
{
  if([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0] < [model getProbIOP])
    {
      if (canvas)
        {
          //set the border width once and for all somewhere else...
          [[nodeItem setBorderColor: "blue"] setBorderWidth: 3];
          GUI_UPDATE_IDLE_TASKS ();
        }
      [[borrowLink getTo] requestLoan];
      IOPbool = 1;
    }
  return self;
}

- (BOOL)borrowingFrom: anFEntity 
{
  if(([borrowLink getTo] == anFEntity) && IOPbool)
    return YES;
  else
    return NO;
}

- (BOOL)investingIn: anFEntity
{
  if([investLink getTo] == anFEntity)
    return YES;
  else
    return NO;
}

- (double)generateROI: (double)investment
{
  if(canvas)
    {
      [nodeItem setBorderColor: "black"];
      GUI_UPDATE_IDLE_TASKS ();
    }
  
  IOPbool = 0;
  
  if([uniformDblRand getDoubleWithMin: 0.0 withMax: 1.0]
     < [model getProbIOPSuccess])
    return investment * [model getIOPmultiplier];
  
  return 0.0;
}

- (BOOL)incident
{
  if ([fromList getCount])
    return YES;
  else 
    return NO;
}

- (int)countInvestors
{
   id index,aLink;
   int count;

   count = 0;

   index = [fromList begin: [self getZone]];
   while ((aLink = [index next]))
     if ([aLink isInvestLink])
       count++;
   [index drop];
   
   return count;
}

- (int)countBorrowers
{
  id index,aLink;
  int count;
  
  count = 0;
  
  index = [fromList begin: [self getZone]];
  while ((aLink = [index next]))
    if ([aLink isBorrowLink])
      count++;
  [index drop];
  
  return count;
}

- (double)getInvestorLinkProbability
{
  return ((double) [self countInvestors]) / ((double) totalAgentNum);
}

- (double)getBorrowLinkProbability
{
  return ((double) [self countBorrowers]) / ((double) totalAgentNum);
}

- lend
{  
  double amountLoaned, cumulativeResult;
  id index, link, someone;
  
  // less than amountLoaned
  // this represents the potential
  // loan assuming an extra client

  prevCapital = capital / (loanCount + 1); 
                                            
  if (loanCount)
    {
      cumulativeResult = 0.0;
      amountLoaned = ((double) capital) / ((double) loanCount);
      index = [fromList begin: globalZone];
      while ((link = [index next]))
        {
          someone = [link getFrom];
          if ([someone borrowingFrom: self])
            {
              double partial;
              partial = [someone generateROI: amountLoaned];
              cumulativeResult += partial;
            }
        }
      [index drop];
    }
  else
    cumulativeResult = (double) capital;
  
  
  if (investCount)
    prevROI = floor(0.5 + (cumulativeResult / ((double)investCount)));
  else 
    prevROI = fixedIncome;
  
  investCount = 0;
  loanCount = 0;
  capital = 0;
  
  return self;
}

@end
