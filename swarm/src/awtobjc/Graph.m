// Swarm library. Copyright (C) 1996-1998 Santa Fe Institute.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

// Interface to Graphs. Legends, etc need to be handled more
// gracefully. Bindings to mouse, autoscrolling, etc should be added.

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#import <javaobjc/global.h>
#import <javaobjc/Graph.h>
#import <collections.h>

@implementation Graph

- createEnd
{
  [self addInt: 400];
  [self addInt: 247];

  [super createEnd];

  elementList = [List create: [self getZone]];
  
  return self;
}

- setRangesXMin: (double)minx Max: (double)maxx
           YMin: (double)miny Max: (double)maxy 
{
  if (_setRanges == 0)
    _setRanges = [self findMethod: "setRanges"
                       signature: "(DDDD)V"];
  
  [self callVoidMethod: _setRanges D: minx D: maxx D: miny D: maxy]; 
  return self;
}

- setScaleModeX: (int)xs Y: (int)ys 
{
  if (_setScaleMode == 0)
    _setScaleMode = [self findMethod: "setScaleMode"
                          signature: "(II)V"];
  
  [self callVoidMethod: _setScaleMode D: xs D: ys];  
  return self;
}

- (GraphElement *)createElement
{
  GraphElement *newElement;

  newElement = [GraphElement createOwnerGraph: self];
  [elementList addLast: newElement];
  
  return newElement;
}

- destroyElement: (GraphElement *)g
{
  [elementList remove: g];
  [g drop];
  return self;
}

- setTitle: (const char *)t
{
  [self setWindowTitle: t];
  if (_setTitle == 0)
    _setTitle = [self findMethod: "setTitle"
                      signature: "(Ljava/lang/String;)V"];
  
  [self callVoidMethod: _setTitle S: t]; 
  return self;
}

- setAxisLabelsX: (const char *)xl Y: (const char *)yl
{
  if (_setAxisLabels == 0)
    _setAxisLabels = [self findMethod: "setAxisLabels"
                           signature: "(Ljava/lang/String;Ljava/lang/String;)V"];
  
  [self callVoidMethod: _setAxisLabels S: xl S: yl];  
  return self;
}

- pack
{
  return self;
}

// first destroy all the elements, then ourselves.
- (void)drop
{
  while ([elementList getCount] > 0)
    [self destroyElement: [elementList getFirst]];

  [super drop];
}

- createJavaElement: (const char *)name
{
  // Let the java graph know it has a data stream with
  // this name; we'll be adding data to it and drawing it
  // shortly.
  if (_createElem == 0)
    _createElem = [self findMethod: "createElement"
                        signature: "(Ljava/lang/String;)V"];
  [self callVoidMethod: _createElem S: name]; 
  return self;
}

- update
{
  if (_update == 0)
    _update = [self findMethod: "update" signature: "()V"];
  [self callVoidMethod: _update];  
  return self;
}

- addElem: (const char *)name X: (double)x Y: (double)y
{
  if (_addElem == 0)
    _addElem = [self findMethod: "addElem"
                     signature: "(Ljava/lang/String;DD)V"];
  [self callVoidMethod: _addElem S: name D: x D: y]; 
  return self;
}

- setElem: (const char *)name Len: (int)n
{
  if (_setElemLen == 0)
    _setElemLen = [self findMethod: "setElemLen"
                        signature: "(Ljava/lang/String;I)V"];
  return self;
}

- dropElem: (const char *)name
{
  if (_dropElem == 0)
    _dropElem = [self findMethod: "dropElem" 
                      signature: "(Ljava/lang/String;)V"];
  [self callVoidMethod: _dropElem S: name];
  return self;
}

- setColor: (const char *)objname Color: (const char *)color
{
  if (_setColor == 0)
    _setColor = [self findMethod: "setColor"
                      signature: "(Ljava/lang/String;Ljava/lang/String;)V"];
  [self callVoidMethod: _setColor S: objname S: color]; 
  return self;
}

@end


@implementation GraphElement

- setOwnerGraph: (Graph *)og
{
  ownerGraph = og;
  return self;
}

static int vid = 0;

// create ourselves and two vectors.
- createEnd
{
  char v[20];

  if (ownerGraph == nil)
    [InvalidCombination raiseEvent: "This element has no owner graph!\n"];
  sprintf (v, "V%d", vid);
  ++vid;
  name = strdup (v);

  // associate a data stream with our name
  [ownerGraph createJavaElement: name];
  xData = [GraphVector create: [self getZone]];
  yData = [GraphVector create: [self getZone]];

  return self;
}

+ createOwnerGraph: (Graph *)og
{
  return [[[self createBegin: [og getZone]] setOwnerGraph: og] createEnd];
}

- (void)drop
{
  [ownerGraph dropElem: [self getName]];
  [xData drop];
  [yData drop];
  [super drop];
}

- (const char *)getName
{
  return name;
}

- (GraphVector *)getXData
{
  return xData;
}

- (GraphVector *)getYData
{
  return yData;
}

- addX: (double)x Y: (double)y
{
  [xData append: x];
  [yData append: y];
  [ownerGraph addElem: [self getName] X: x Y: y];
  [ownerGraph update];
  return self;
}

- resetData
{
  [xData resetData];
  [yData resetData];
  return self;
}

- setLabel: (const char *)label
{
  fprintf(stderr,"elem: set label %s\n", label);
  return self;
}

- setColor: (const char *)color
{
  [ownerGraph setColor: [self getName] Color: color];
  return self;
}

- setWidth: (unsigned)w
{
  fprintf(stderr,"elem: set width %d\n", w);
  return self;
}

- setSymbol: (const char *)s
{
  fprintf (stderr,"elem: set symbol %s\n", s);
  return self;
}

// set the dash style - 0 means solid.
- setDashes: (int)d
{
  fprintf (stderr,"elem: set dashes %d\n", d);
  return self;
}

@end

// Optimize append so it doesn't regrow the vector for every single
// value - regrow in chunks.
@implementation GraphVector
- createEnd
{
  static int cnt=0;
  char n[64];
  sprintf (n, "V%d", cnt); ++cnt;
  name = strdup (n);
  val = (double *)malloc (GRAPHV_CHUNK * sizeof (double));
  max_val = GRAPHV_CHUNK;
  cur_val = 0;
  return self;
}

- (const char *)getName
{
  return name;
}

- (unsigned)getLength
{
  return cur_val;
}

- setLength: (unsigned)n
{
  unsigned i;

  if (n > max_val)
    {
      // extend our array 
      unsigned new_size = max_val;
      
      while (new_size < n)
        new_size += new_size;
      
      val = (double *)realloc (val, new_size * sizeof (double));
      max_val = new_size;
    }
  
  // zero out any newly extended data
  for (i = cur_val; i < n; i++)
    val[i] = 0.0;
  
  cur_val = n;
  return self;
}

- append: (double)v
{
  int idx = cur_val;
  
  // make sure we have room for 1 more
  [self setLength: cur_val + 1];
  
  // set it
  val[idx] = v;
  return self;
}

// vector ranges - ":" is like the range "5:7", but it assumes you
// mean from min to max.
- resetData
{
  [self setLength: 0];
  return self;
}

- delete: (int)n
{
  if (n >= 0 && n < (int)cur_val)
    {
      int i;
      
      // we have one fewer datum
      cur_val--;
      
      // shift everyone down
      for (i=n; i < (int)cur_val; i++)
        val[i] = val[i+1];
    }
  return self;
}

- (void)drop
{
  free (val);
}

@end

