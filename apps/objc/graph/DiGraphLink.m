#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <tkobjc.h>

#import "DiGraphNode.h"
#import "DiGraphLink.h"

@implementation DiGraphLink

-setFrom: the_from To: the_to {
  from = the_from ;
  to = the_to ;

  return self ;
}

- setCanvas: theCanvas
{
  canvas = theCanvas ;   
  
  if (canvas == nil)
    return self;

  if (from && to) 
        linkItem = [[[[[LinkItem createBegin: [self getZone]] 
                        setFrom: [from getNodeItem]] setTo: [to getNodeItem]] 
                      setCanvas: canvas] createEnd] ;
  return self ;
}

-createEnd {  
 
   if (canvas) {
      if (linkItem) {
      }
      else 
         linkItem = [[[[[LinkItem createBegin: [self getZone]] 
                          setFrom: [from getNodeItem]] setTo: [to getNodeItem]] 
                         setCanvas: canvas] createEnd] ;
   }
   
   [from addTo: self] ;
   [to addFrom: self] ;
  
  return self;
}

-getFrom {
  return from ;
}

-getTo {
  return to ;
}

-getLinkItem {
  return linkItem ;
}

-update {
  [linkItem update] ;
  return self ;
}

-hideLink {
   canvas = nil;
   [linkItem drop] ;
   return self;
}

-(void) drop {
  [from removeTo: self] ;
  [to removeFrom: self] ;
  if(canvas)  
     [self hideLink];
   
  [super drop] ;
}

@end

