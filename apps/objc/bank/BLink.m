#define __USE_FIXED_PROTOTYPES__  // for gcc headers

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
