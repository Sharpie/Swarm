// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <tkobjc/WindowGeometryRecord.h>
#import <collections.h>

#import <collections/predicates.h> // for stringp

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

+ in: aZone expr: expr
{
  return [[WindowGeometryRecord create: aZone] in: expr];
}

PHASE(Using)

static int
getVal (id obj)
{
  if (stringp (obj))
    return atoi ([obj getC]);
  else
    [WindowGeometryRecordError raiseEvent: "Object is not a string (%s)",
                               [obj name]];
}

static id
getValueList (id index)
{
  id l = [index next];
  
  if ([l getFirst] != ArchiverLiteral)
    [WindowGeometryRecordError raiseEvent: "ArchiverLiteral expected"];

  return [l getLast];
}

- in: expr
{
  id index = [expr begin: scratchZone];
  id obj;

  while ((obj = [index next]))
    {
      if (stringp (obj))
        {
          const char *str = [obj getC];
          if (str[0] == ':')
            {
              str++;

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
        }
      else
        [WindowGeometryRecordError raiseEvent: "String expected (%s)\n",
                                   [obj name]];

    }

  return self;
}

- out: outputCharStream
{
  char buf[20];

  if (sizeFlag)
    {
      [outputCharStream catC: " :size '("];
      sprintf (buf, "%u %u", width, height);
      [outputCharStream catC: buf];
      [outputCharStream catC: ")"];
    }
  if (positionFlag)
    {
      [outputCharStream catC: " :position '("];
      sprintf (buf, "%d %d", x, y);
      [outputCharStream catC: buf];
      [outputCharStream catC: ")"];
    }
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
