// Copyright (C) 1996-1998 Santa Fe Institute.
#import "BLink.h"

@implementation BLink

- createEnd
{
  [super createEnd];
  if (canvas)
    [linkItem setColor: "red"];
  return self;
}


- (int)isInvestLink
{
  return 0;
}

- (int)isBorrowLink
{
  return 1;
}

@end
