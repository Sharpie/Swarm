// Copyright (C) 1996-1998 Santa Fe Institute.
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
