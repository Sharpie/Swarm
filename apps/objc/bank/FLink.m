// Copyright © 1996-1999 Santa Fe Institute.
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
