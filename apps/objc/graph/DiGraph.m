// Copyright (C) 1995-1998 The Santa Fe Institute.
// No warranty implied, see LICENSE for terms.
#define __USE_FIXED_PROTOTYPES__  // for gcc headers

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#import <gui.h>
#import <simtools.h>

#import <graph.h>

#define MAX_EDGES 	       200
#define MAX_NODES 	       200
#define MAX_ITERATIONS	        40
#define DEFAULT_SPRING_LENGTH  100

@implementation DiGraph

+ createBegin: aZone
{
  DiGraph *obj;

  obj = [super createBegin: aZone];
  obj->springLength = DEFAULT_SPRING_LENGTH;
  obj->randPosSeed = DEFAULT_RANDOM_SEED;

  return obj;
}

- setRandPosSeed: (int)seed
{
  randPosSeed = seed;
  return self;
}

- setCanvas: aCanvas
{
  [self setCanvas: aCanvas withRandPosSeed: randPosSeed];
  return self;
}

- setCanvas: aCanvas withRandPosSeed: (int)seed
{  
  canvas = aCanvas;

  randGPosition = [PMMLCG1 createBegin: [self getZone]];
  [randGPosition setStateFromSeed: seed];
  randGPosition = [randGPosition createEnd];
  
  // distribution for the random node positions
  uRandPosition = [UniformInteger createBegin: [self getZone]];
  [uRandPosition setGenerator: randGPosition];
  uRandPosition = [uRandPosition createEnd];

  [nodeList forEach: @selector(setCanvas:withRandPosFunc:) 
            :aCanvas: uRandPosition];

  return self;
}

- setSpringLength: (float)aLength
{
  springLength = aLength;
  return self;
}

- createEnd
{
  nodeList = [List create: [self getZone]];
  return self;
}

- showCanvas: aCanvas
{
   id index, node;

   if (canvas)
     {
       fprintf (_obj_xdebug, "DiGraph already has canvas\n");
       return self;
     }
   else
     {
       canvas = aCanvas; // set the DiGraph's canvas

       // generator for the random node positions
       randGPosition = [PMMLCG1 createBegin: [self getZone]];
       [randGPosition setStateFromSeed: randPosSeed];
       randGPosition = [randGPosition createEnd];
       
       // distribution for the random node positions
       uRandPosition = [UniformInteger createBegin: [self getZone]];
       [uRandPosition setGenerator: randGPosition];
       uRandPosition = [uRandPosition createEnd];
       
       [nodeList forEach: @selector(setCanvas:withRandPosFunc:) 
                 :aCanvas: uRandPosition];

       // step through all nodes in list
       // creating  graphical links for each node 
       index = [nodeList begin: globalZone];
       
       while ((node = [index next]))
         {
           [[node getToLinks] forEach: M(setCanvas:): aCanvas];
         }
       [index drop];
     }
   
   [self update]; // display node status
   
   return self;
}

- hideCanvas
{
  id index, node;
  
  if (canvas)
    {
      index = [nodeList begin: globalZone];
      
      while ((node = [index next]))
        [node hideNode];
      [index drop];
      
      // step through all nodes in list
      // creating  graphical links for each node 
      index = [nodeList begin: globalZone];
      
      while ((node = [index next]))
        {
          [[node getToLinks] forEach: M(hideLink)];
        }
      [index drop];
      
      canvas = nil;  // make sure canvas is no longer referenced.
    } 
  else 
    fprintf (_obj_xdebug, "Canvas doesn't exist for this DiGraph!\n"); 
  return self;
}

- getCanvas
{
  return canvas;
}

- getNodeList
{
  return nodeList;
}

- addNode: aNode
{
  [nodeList addFirst: aNode];
  if (canvas)
    [aNode setCanvas: canvas withRandPosFunc: uRandPosition];
  return self;
}

- dropNode: which
{
  [nodeList remove: which];
  [which drop];
  return self;
}

- addLinkFrom: this To: that
{
  id aLink;
  
  aLink = [[[[DiGraphLink createBegin: [self getZone]] 
              setCanvas: canvas]
             setFrom: this To: that] 
            createEnd];
  return self;
}

- removeLink: aLink
{
  [aLink drop];
  return self;
}

- redistribute
{
  int i, n, h, w, r, x, y, bx, by;
  double phase;
  id obj;

  if (canvas)
    {
      h = [canvas getHeight];
      w = [canvas getWidth];
      
      r = ( (h > w) ? w : h );
      r /= 3;
      
      bx = w / 2;
      by = h / 2;
      
      n = [nodeList getCount];
      
      for (i = 0; i < n; i++)
        {
          phase = 6.2831853 * ((double) i) / ((double) n);
          x = bx + ((int) (((double) r) * cos(phase)));
          y = by + ((int) (((double) r) * sin(phase)));
          obj = [[nodeList atOffset: i] getNodeItem]; 
          [obj initiateMoveX: x - [obj getX] Y: y - [obj getY]];
        }
    }
  
  return self;
}

- boingDistribute
{
  return [self boingDistribute: MAX_ITERATIONS];
}

- boingDistribute: (int)iterations
{
  int i;
  for (i = 0; i < iterations; i++)
    {
      
      // This is the beginnings of an idea on some automatic termination condition:
      //   if([self boingStep] < 0.0000000002) i = MAX_ITERATIONS;
      
      [self boingStep];
    }
  
  return self;
}

- (double)boingStep 
{
  //
  //	This method updates the nodes in
  //	a directed graph using 
  //	``Spring Embedding''
  //
  
  int   from[MAX_EDGES];
  int     to[MAX_EDGES];
  double len[MAX_EDGES];
  
  double nodedx[MAX_NODES];
  double nodedy[MAX_NODES];
  
  int nedges;
  
  int i, j, h, w, m, posx, posy;
  int n = 0;
  double vx, vy, length, f, dx, dy, dlength;
  id obj, another_obj;
  
  for (i = 0; i < MAX_NODES; i++)
    {
      nodedx[i] = 0.0;
      nodedy[i] = 0.0;
    }
  
  nedges = 0;
  if (canvas)
    {
      
      h = [canvas getHeight];
      w = [canvas getWidth];
      n = [nodeList getCount];
      
      for (i = 0; i < n; i++)
        {
          
          obj = [nodeList atOffset: i];
          m=0;
          for(j = i + 1; j < n; j++){
            
            another_obj = [nodeList atOffset: j];
            
            if ([obj linkedTo: another_obj] == 1)
              {
                from[nedges] = i;
                to[nedges] = j;
                len[nedges] = springLength;
                nedges++;
              }
            
            if ([obj linkedFrom: another_obj] == 1)
              {
                from[nedges] = j;
                to[nedges] = i;
                len[nedges] = springLength;
                nedges++;
              }
          }		
        }
      
      for (i = 0; i < nedges; i++)
        {
          vx = [[[nodeList atOffset: to[i]] getNodeItem] getX] 
            - [[[nodeList atOffset: from[i]] getNodeItem] getX];
          vy = [[[nodeList atOffset: to[i]] getNodeItem] getY] 
            - [[[nodeList atOffset: from[i]] getNodeItem] getY];
          length = pow(vx * vx + vy * vy, 0.5);
          f = (len[i] - length) / (length * 3.0);
          dx = f * vx;
          dy = f * vy;
          nodedx[to[i]] += dx;
          nodedy[to[i]] += dy;
          nodedx[from[i]] -= dx;
          nodedy[from[i]] -= dy;
        }
      
      for (i = 0; i < n; i++)
        {
          obj = [nodeList atOffset: i];
          dx = 0.0;
          dy = 0.0;
          
          for (j = 0; j < n; j++)
            {
              if (i != j)
                {
                  another_obj = [nodeList atOffset: j];
                  
                  vx = [[obj getNodeItem] getX] - 
                    [[another_obj getNodeItem] getX];
                  vy = [[obj getNodeItem] getY] - 
                    [[another_obj getNodeItem] getY];
                  
                  length = vx * vx + vy * vy;
                  
                  if (length == 0.0)
                    {
                      dx += [uniformDblRand getDoubleWithMin: -1.0 
                                            withMax:  1.0];
                      dy += [uniformDblRand getDoubleWithMin: -1.0 
                                            withMax:  1.0];
                    }
                  else
                    {
                      dx += vx / (length);
                      dy += vy / (length);
                    }
                }
            }
          
          dlength = dx * dx + dy * dy;
          
          if (dlength > 0.0)
            {
              dlength = pow(dlength,0.5) / 2.0;
              nodedx[i] += (dx / dlength) * 2.5;
              nodedy[i] += (dy / dlength) * 2.5;
            }
        }	
      
      for (i = 0; i < n; i++)
        {
          
          obj = [nodeList atOffset: i];
          
          if(nodedx[i] >  5.0) nodedx[i] =  5.0;
          if(nodedx[i] < -5.0) nodedx[i] = -5.0;
          if(nodedy[i] >  5.0) nodedy[i] =  5.0;
          if(nodedy[i] < -5.0) nodedy[i] = -5.0;
          
          posx = [[obj getNodeItem] getX] + nodedx[i];
          
          if (posx < 0 + (w / 10.0))
            posx = 0 + (w / 10.0);
          if (posx > w - (w / 10.0))
            posx = w - (w / 10.0);
          
          posy = [[obj getNodeItem] getY] + nodedy[i];
          
          if (posy < 0 + (h / 10.0))
            posy = 0 + (h / 10.0);
          if (posy > h - (h / 10.0))
            posy = h - (h / 10.0);
          
          [[obj getNodeItem] initiateMoveX: posx - [[obj getNodeItem] getX] 
                             Y: posy - [[obj getNodeItem] getY]];
        }
    }
  
  vx = 0.0;
  
  for (i = 0; i < n; i++)
    {
      vx += nodedx[i];
      vx += nodedy[i];
    }
  
  vx = vx / (2.0 * n);
  
  return vx;
}

- update
{
  if (canvas)
    GUI_UPDATE_IDLE_TASKS();
  return self;
}

@end
