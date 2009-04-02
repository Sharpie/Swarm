// Swarm library. Copyright © 1996-2000 Swarm Development Group.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <tkobjc/WindowGeometryRecord.h>
#import <collections.h>

#import <collections/predicates.h> // stringp
#import "global.h"

#import <defobj/swarm-objc-api.h> // _C_INT

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

PHASE(Setting)
static int
getVal (id obj)
{
  if (!valuep (obj))
    raiseEvent (WindowGeometryRecordError,
                "Object is not a ArchiverValue (%s)",
                [obj name]);
  return [obj getInteger];
}

- lispIn: expr
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
              id l = [[index next] getQuotedObject];
              
              positionFlag = YES;
              x = getVal ([l getFirst]);
              y = getVal ([l getLast]);
            }
          else if (strcmp (str, "size") == 0)
            {
              id l = [[index next] getQuotedObject];
              
              sizeFlag = YES;
              width = getVal ([l getFirst]);
              height = getVal ([l getLast]);
            }
          else
            raiseEvent (WindowGeometryRecordError,
                        "Unknown keyword: `%s'\n",
                        str);
        }
      else
        raiseEvent (WindowGeometryRecordError, 
                    "String expected (%s)\n",
                    [obj name]);
    }
  [index drop];
  return self;
}

PHASE(Using)

- lispOutShallow: stream
{
  [stream catStartMakeInstance: [self getTypeName]];
  if (sizeFlag)
    {
      [stream catSeparator];
      [stream catKeyword: "size"];
      [stream catSeparator];
      [stream catStartQuotedList];
      [stream catUnsigned: width];
      [stream catSeparator];
      [stream catUnsigned: height];
      [stream catEndQuotedList];
    }
  if (positionFlag)
    {
      [stream catSeparator];
      [stream catKeyword: "position"];
      [stream catSeparator];
      [stream catStartQuotedList];
      [stream catInt: x];
      [stream catSeparator];
      [stream catInt: y];
      [stream catEndQuotedList];
    }
  [stream catEndMakeInstance];
  return self;
}

- lispOutDeep: stream
{
  return [self lispOutShallow: stream];
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
