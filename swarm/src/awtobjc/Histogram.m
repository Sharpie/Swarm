// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

#import <awtobjc/global.h>
#import <awtobjc/Histogram.h>

#include <string.h>

@implementation Histogram

- createEnd
{
  [super createEnd];
  numPoints = 0;
  return self;
}

// you can only call this once. Fix it.
- setNumPoints: (int)n Labels: (char **)l Colors: (char **)c
{
  int i;

  numPoints = n;

  elements = [[self getZone] alloc: (sizeof(*elements) * n)];
  colors = [[self getZone] alloc: (sizeof(*colors) * n)];
  labels = [[self getZone] alloc: (sizeof(*labels) * n)];
  for (i = 0; i < n; i++)
    {
      char strBuffer[256];
      
      sprintf(strBuffer, "%d", i);
      elements[i] = strdup(strBuffer);
      labels[i] = l[i] ? strdup(l[i]) : NULL;
      colors[i] = c[i] ? strdup(c[i]) : NULL;
    }
  return self;
}

- _reset_
{
  if (_reset == 0)
    _reset = [self findMethod: "reset" signature: "()V"];
  
  [self callVoidMethod: _reset];  
  return self;
}

// add the next bar value
- _addBar_: (const char *)label value: (double)dval color: (const char *)color
{
  if (_addBar == 0)
    _addBar = [self findMethod: "addBar"
                    signature: "(Ljava/lang/String;Ljava/lang/String;D)V"];
  
  [self callVoidMethod: _addBar S: label S: color D: dval];
  
#if 0
  //  XXX until we figure out where it should really happen
  [self update];
#endif
  return self;
}
      
// show the current graphed bars on the screen
- _update_
{
  if (_update == 0)
    _update = [self findMethod: "update" signature: "()V"];
  
  [self callVoidMethod: _update];
  return self;
}

- drawHistoWithDouble: (double *)points
{
  int i;

  [self _reset_];
  for (i = 0; i < numPoints; i++)
    {
      const char *color = colors[i] ? colors[i] : "black";
      const char *label = labels[i] ? labels[i] : "";
      
      [self _addBar_: label value: points[i] color: color];
    }
  [self _update_];
  return self;
}

// ick. How to do two data formats right?
- drawHistoWithInt: (int *)points
{
  int i;

  [self _reset_];
  for (i = 0; i < numPoints; i++)
    {
      const char *color = colors[i] ? colors[i] : "black";
      const char *label = labels[i] ? labels[i] : "";
      
      fprintf(stderr, "hist int: adding %s %s %d\n", label, color, points[i]);
      
      [self _addBar_: label value: (double)points[i] color: color];
    }
  [self _update_];
  return self;
}

- drawHistoWithInt: (int *)points atLocations: (double *)locations
{
  return self;
}

-drawHistoWithDouble: (double *)points atLocations: (double *)locations
{
  return self;
}

// this code is in common with BLTGraph
- setTitle: (const char *)t
{
  [self setWindowTitle: t];
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  return self;
}

- pack
{
  return self;
}

@end

