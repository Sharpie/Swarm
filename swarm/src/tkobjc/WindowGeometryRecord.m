// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <tkobjc/WindowGeometryRecord.h>
#import <collections.h>

#import <collections/predicates.h> // for literal_string_p

@implementation WindowGeometryRecord

PHASE(Creating)

- setWindowGeometry: (const char *)theWindowGeometryString;
{
  if (theWindowGeometryString)
    {
      windowGeometryString = [[[String createBegin: [self getZone]]
                                setLiteralFlag: YES]
                               createEnd];
      [windowGeometryString setC: theWindowGeometryString];
    }
  else
    windowGeometryString = nil;

  return self;
}

+ in: aZone expr: expr
{
  return [[WindowGeometryRecord create: aZone] in: expr];
}

PHASE(Using)

- (const char *)getWindowGeometry;
{
  return [windowGeometryString getC];
}

- _badType_: obj
{
  abort ();
}

- _badValue_: obj
{
  abort ();
}

- in: expr
{
  if (!literal_string_p (expr))
    [self _badType_: expr];
  [self setWindowGeometry: [expr getC]];
  return self;
}

- out: outputCharStream
{
  [windowGeometryString out: outputCharStream];
  return self;
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: [windowGeometryString getC]];
}

@end
