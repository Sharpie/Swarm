#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <gui.h>
#import "DiGraphNode.h"
#import "DiGraphLink.h"

@implementation DiGraphLink

- setFrom: the_from To: the_to
{
  from = the_from;
  to = the_to;

  return self;
}

- setCanvas: theCanvas
{
  canvas = theCanvas;   
  
  if (canvas == nil)
    return self;

  if (from && to) 
        linkItem = [[[[[LinkItem createBegin: [self getZone]] 
                        setFrom: [from getNodeItem]] setTo: [to getNodeItem]] 
                      setCanvas: canvas] createEnd];
  return self;
}

- createEnd
{  
   if (canvas)
     if (!linkItem)
       linkItem = [[[[[LinkItem createBegin: [self getZone]] 
                       setFrom: [from getNodeItem]] setTo: [to getNodeItem]] 
                     setCanvas: canvas] createEnd];
   [from addTo: self];
   [to addFrom: self];
  
   return self;
}

- getFrom
{
  return from;
}

- getTo
{
  return to;
}

- getLinkItem
{
  return linkItem;
}

- (void)update
{
  if (linkItem)
    [linkItem update];
}

- hideLink
{
  canvas = nil;
  if (linkItem)
    [linkItem drop];
  return self;
}

- (void)drop
{
  [from removeTo: self];
  [to removeFrom: self];
  if (canvas)  
    [self hideLink];
  
  [super drop];
}

@end

