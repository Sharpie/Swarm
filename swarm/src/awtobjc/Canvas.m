// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <javaobjc/global.h>
#import <javaobjc/Canvas.h>

@implementation Canvas

- createEnd
{
  [self addInt: 400];
  [self addInt: 247];

  [super createEnd];
  return self;
}

// the tk world places objects within a container using calls to the
// interpreter, passing the name of the object's widget.  we pass through
// a more formal path from objective C into a java placeholder object...

- placeObj: item X: (int)x Y: (int)y
{
  jobject component = [item getJobject];

  if (_placeObj == 0)
    _placeObj = [self findMethod: "placeObj"
                      signature: "(Ljava/awt/Component;II)V"];
  
  [self callVoidMethod: _placeObj O: component :x :y];  
  return self;
}

- moveObj: item X: (int)x Y: (int)y
{
  jobject component = [item getJobject];

  if (_moveObj == 0)
    _moveObj = [self findMethod: "moveObj"
                     signature: "(Ljava/awt/Component;II)V"];
  
  [self callVoidMethod: _moveObj O: component : x : y];
  return self;  
}

- drawLineFX: (int)fx FY: (int)fy TX: (int)tx TY: (int)ty
{
  if (_drawLine == 0)
    _drawLine = [self findMethod: "drawLine" signature: "(IIII)V"];
  [self callVoidMethod: _drawLine : fx : fy : tx : ty];
  return self;
}

- setColor: (const char *)color
{
  if (_setColor == 0)
    _setColor = [self findMethod: "setColor"
                      signature: "(Ljava/lang/String;)V"];
  [self callVoidMethod: _setColor S: color];   
  return self;
}

@end

