#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#import "FLink.h"

@implementation FLink

-(int) isInvestLink {
  [self subclassResponsibility: @selector(isInvestLink)] ;
    return -1 ;
}

-(int) isBorrowLink {
  [self subclassResponsibility: @selector(isBorrowLink)] ;
    return -1 ;
}

@end
