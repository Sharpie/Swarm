// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// TopLevels and Frames are about the same, so we put these in one class.

#import <javaobjc/Frame.h>

#import <javaobjc/common.h>

@implementation Frame

- setBorderWidth: (int)theBorderWidth
{
  borderWidth = borderWidth;
  return self;
}

- setReliefFlag: (BOOL)theReliefFlag
{
  reliefFlag = theReliefFlag;
  return self;
}

// Make a new top level frame.  Can't use Widget default createEnd, because
// this is where the toplevel is actually built.
- createEnd
{
  if (parent == nil)
    {
      [self setWidgetNameFromParentName: "."];
      [self addInt: 417];
      [self addInt: 200];
      [self createBackingObject];
      [self registerAndLoad];
    }
  else
    {
      [self setClassIdFromAWTName: "Panel"];
      [super createEnd];
      
      [parent callObjectMethod: 
                [parent findMethod: "add" signature: "(Ljava/awt/Component;)Ljava/awt/Component;"]
              O: jobj];
    }

  // Relief and borderWidth
  return self;
}

- assertPosition
{
  printf ("Frame assertPosition\n");
  return self;
}

- assertGeometry
{
  id canvas = [self getParent];
  const char *canvasName = [canvas getWidgetName];

  printf ("Frame assertGeometry canvasName: %s\n", canvasName);
  return self;
}

- withdraw
{
  printf ("Frame withdraw\n");
  return self;
}

- deiconify
{
  printf ("Frame deiconify\n");
  return self;
}

- (void)drop
{
  void archiverUnregister (id client);
  
  archiverUnregister (self);

  if (parent == nil && !destroyedFlag)
    printf ("Frame destroy\n");
}

@end

