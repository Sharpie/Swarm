// Swarm library. Copyright (C) 1996-1997 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <simtools/GUIComposite.h>
#import <collections.h>

@implementation GUIComposite

- setWindowGeometryRecordName : (const char *)windowGeometryRecordName
{
  baseWindowGeometryRecordName = windowGeometryRecordName;
  return self;
}

- (const char *)windowGeometryRecordName
{
  return baseWindowGeometryRecordName;
}

- (const char *)windowGeometryRecordNameForComponent: (const char *)componentName
{
  if (baseWindowGeometryRecordName)
    {
      id string = [String create: [self getZone]
                          setC: baseWindowGeometryRecordName];
      [string appendC: "-"];
      [string appendC: componentName];
      return [string getC];
    }
  else
    return NULL;
}

@end
