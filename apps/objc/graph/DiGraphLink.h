// Copyright (C) 1995 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.

#import <objectbase.h>

@interface DiGraphLink: SwarmObject {
  id from ;
  id to ;
  id canvas ;
  id linkItem ;
}

-setCanvas: aCanvas ;
-setFrom: from To: to ;
-createEnd ;
-getFrom ;
-getTo ;
-getLinkItem ;
-update ;
-(void) drop ;
@end
