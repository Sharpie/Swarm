// Copyright © 1996-2000 Swarm Development Group.
#import "FLink.h"

@implementation FLink

- (int)isInvestLink
{
  [self subclassResponsibility: @selector(isInvestLink)];
  return -1;
}

- (int)isBorrowLink
{
  [self subclassResponsibility: @selector(isBorrowLink)];
  return -1;
}

@end
