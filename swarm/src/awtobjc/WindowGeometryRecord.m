// Swarm library. Copyright (C) 1996-1998, 2000 Swarm Development Group.  This
// library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file LICENSE for details and terms of copying.

#import <awtobjc/WindowGeometryRecord.h>
#import <collections.h>

#import <collections/predicates.h> // for literal_string_p

@implementation WindowGeometryRecord

- (const char *)getWindowGeometry;
{
  return [windowGeometryString getC];
}

- setWindowGeometry : (const char *)theWindowGeometryString;
{
  if (theWindowGeometryString)
    {
      windowGeometryString = [LiteralString create: [self getZone]];
      [windowGeometryString setC: theWindowGeometryString];
    }
  else
    windowGeometryString = nil;
  return self;
}

- _badType_ : obj
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
    [self _badType_ : expr];
  [self setWindowGeometry : [expr getC]];
  return self;
}

- out: outputCharStream
{
  [windowGeometryString out : outputCharStream];
  return self;
}

+ in : aZone expr: expr
{
  return [[WindowGeometryRecord create: aZone] in: expr];
}

- (void)describe: outputCharStream
{
  [outputCharStream catC: [windowGeometryString getC]];
}

@end
