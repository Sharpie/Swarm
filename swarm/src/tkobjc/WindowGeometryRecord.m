// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <tkobjc/WindowGeometryRecord.h>
#import <collections.h>

#import <collections/predicates.h> // for stringp
#import "global.h"

#include <objc/objc-api.h> // _C_INT

@implementation WindowGeometryRecord

PHASE(Creating)

- setX: (int)theX Y: (int)theY
{
  positionFlag = YES;
  x = theX;
  y = theY;

  return self;
}

- setWidth: (unsigned)theWidth Height: (unsigned)theHeight
{
  sizeFlag = YES;
  width = theWidth;
  height = theHeight;

  return self;
}

PHASE(Using)

static int
getVal (id obj)
{
  if (!valuep (obj))
    [WindowGeometryRecordError
      raiseEvent: "Object is not a ArchiverValue (%s)", [obj name]];
  if ([obj getValueType] != _C_INT)
    [WindowGeometryRecordError
      raiseEvent: "Object is not an integer (%s)", [obj name]];
    
  return [obj getInteger];
}

static id
getValueList (id index)
{
  id l = [index next];
  
  if (!ARCHIVERLITERALP ([l getFirst]))
    [WindowGeometryRecordError raiseEvent: "ArchiverLiteral expected"];

  return [l getLast];
}

- lispin: expr
{
  id index = [expr begin: scratchZone];
  id obj;
      
  while ((obj = [index next]))
    {
      if (keywordp (obj))
        {
          const char *str = [obj getKeywordName];

          if (strcmp (str, "position") == 0)
            {
              id l = getValueList (index);
              
              positionFlag = YES;
              x = getVal ([l getFirst]);
              y = getVal ([l getLast]);
            }
          else if (strcmp (str, "size") == 0)
            {
              id l = getValueList (index);
              
              sizeFlag = YES;
              width = getVal ([l getFirst]);
              height = getVal ([l getLast]);
            }
          else
            [WindowGeometryRecordError
              raiseEvent: "Unknown keyword: `%s'\n", str];
        }
      else
        [WindowGeometryRecordError raiseEvent: "String expected (%s)\n",
                                   [obj name]];
    }
  [index drop];
}

- lispout: stream
{
  char buf[20];

  [stream catC: "(" MAKE_OBJC_FUNCTION_NAME " '"];
  [stream catC: [self name]];
  [stream catC: " "];

  if (sizeFlag)
    {
      [stream catC: " #:size '("];
      sprintf (buf, "%u %u", width, height);
      [stream catC: buf];
      [stream catC: ")"];
    }
  if (positionFlag)
    {
      [stream catC: " #:position '("];
      sprintf (buf, "%d %d", x, y);
      [stream catC: buf];
      [stream catC: ")"];
    }
  [stream catC: ")"];
  return self;
}

- (BOOL)getPositionFlag
{
  return positionFlag;
}

- (BOOL)getSizeFlag
{
  return sizeFlag;
}

- (unsigned)getWidth
{
  return width;
}

- (unsigned)getHeight
{
  return height;
}

- (int)getX
{
  return x;
}

- (int)getY
{
  return y;
}

@end
