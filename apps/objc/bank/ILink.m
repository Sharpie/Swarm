// Copyright (C) 1996-1998 Santa Fe Institute.
#import "ILink.h"

@implementation ILink

- createEnd
{
  [super createEnd];
  if (canvas) 
    [linkItem setColor: "PaleGreen4"];
  return self;
}


- (int)isInvestLink 
{
  return 1;
}

- (int)isBorrowLink
{
  return 0;
}

@end
