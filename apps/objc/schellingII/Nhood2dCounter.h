#import <objectbase.h>
#import <objectbase/SwarmObject.h>


/* Nhood2dCounter

  I struggled muchly to find a good name for this class. I hope this
  gives you the idea:

  It lets you ask a grid "how many items can you see from (x,y)" and
  the grid quickly gives you the answer. This, of course, presupposes
  you have told the grid to add or remove items.

  It is new kind of grid for Swarm simulations.  This is primarily
  intended to make models on grids run faster!  It adds much
  functionality as well, customizing the type of neighborhood and
  dealing with the question of "edge wrapping".  I've used many
  concepts and terms from Swarm's Discrete2d, but this is a
  free-standing class that does not inherit from Discrete2d.

  One Warning.  This Nhood2dCounter does not actually keep objects,
  it just Counts them.  If you want to put in objects as well, I will
  give you a copy of the companion class Nhood2dObjects, which will
  quickly give back a list of visible neighbors.

  This class is for "visible counts", such as "the number of agents
  (people, trees, mice, etc) I can see from coordinates (x,y)".  When
  we insert a agent at (x,y), then the class handles the problem
  of calculating the impact of that new item on the view from
  neighboring cells. That way, the grid is always ready with a
  quick answer to a question like "how many are visible from (x,y)"

  Why do this?


  Models run slowly if each agent in a grid has to look around
  to all neighboring cells and do a "little survey" to find out
  how many "items" of a certain type exist within view. This class
  is called Int2d because it is intended to measure integer
  values that are visible within a neighborhood. There are other
  classes that record visible objects and visible double-valued
  variables.

  This class is one solution to that problem.  Instead of having
  each agent who wants information do a little survey of its 
  own, instead this class does once-and-for-all calculations 
  to measure the number of visible objects within a given 
  neighborhood.  It does that by taking items and then
  measuring out their impact on all neighbors.  So, for example,
  if I say "addOneAtX: 10 Y: 12", then one item is added
  at each cell that is visible from (10,12).

  The Int2d grid is sized xsize by ysize. 

  Around each cell there is a neighborhood. That neighborhood is
  inside a square with a given radius (number of cells from edge to
  center). If all cells within the square are indeed visible from a
  square, then we have a simple Moore neighborhood.  But not all cells
  inside the square need be in the neighborhood of a cell.  Consider
  the target cell (10,12).  Suppose the radius is 2, meaning the total
  width of the neighborhood is 5 (2+2+1). In this example, a Y in a
  cell indicates it is in the neighborhood of 10,12, and N if it is
  not.


 0 . . . . . . . . . . . . . . .
 1 . . . . . . . . . . . . . . .
 2 . . . . . . . . . . . . . . .
 3 . . . . . . . . . . . . . . .
 4 . . . . . . . . . . . . . . .
 5 . . . . . . . . . . . . . . .
 6 . . . . . . . . . . . . . . .
 7 . . . . . . . . . . . . . . .
 8 . . . . . . . . . N N Y N N .
 9 . . . . . . . . . N Y Y Y N .
10 . . . . . . . . . Y Y Y Y Y .
11 . . . . . . . . . N Y Y Y N .
12 . . . . . . . . . N N Y N N .
13 . . . . . . . . . . . . . . .
14 . . . . . . . . . . . . . . .
15 . . . . . . . . . . . . . . .
16 . . . . . . . . . . . . . . .

the Y's and N's are recorded in the concept of a "neighborhood mask",
and so, if we want to take a given cell and iterate over cells that
are close to it, we use the mask to simply ignore the cells that are
not inside the neighborhood of the cell.

Conceivably, a person could design a completely different pattern of
Y's and N's to make a neighborhood. I've not done it yet because I've
not thought of a good reason to do so.  But maybe somebody will send
me an email.

When an Int2d object is initialized, the terminal from which the model 
is started prints out a reminder of the neighborhood.  

Here is the nhood matrix
0 0 0 0 1 0 0 0 0
0 0 0 1 1 1 0 0 0
0 0 1 1 1 1 1 0 0
0 1 1 1 1 1 1 1 0
1 1 1 1 1 1 1 1 1
0 1 1 1 1 1 1 1 0
0 0 1 1 1 1 1 0 0
0 0 0 1 1 1 0 0 0
0 0 0 0 1 0 0 0 0

There are two other vital things to know.  

First, the variable "edgeWrap" can be true or false.  If edgeWrap=True, 
then the neighborhoods wrap around the edges and the space is a torus.

If edgeWrap=False, then cells that are close to the edges of the grid have
truncated neighborhoods.  They treat the outer edge as if it were the 
end of the world.

Second, the variable SYNCHRONOUS determines the kind of updating 
in the model.  If SYNCHRONOUS=NO, then when an item is added to the grid, 
that item's impact on the neighborhood is registered immediately.

If SYNCHRONOUS=YES, then the impact of new items is not registered
until the stepRule method is called.  StepRule causes all of the 
accumulated changes to "flush out" onto the grid.  SYNCHRONOUS=YES 
might imply the following. Agents move about while looking at a years-old
census report.  Then after many periods of movement, a new census
gets published by the stepRule method, and then the agents learn 
the true state of the world.


*/

@interface Nhood2dCounter: SwarmObject
{
  BOOL edgeWrap;
  BOOL SYNCHRONOUS;
  unsigned int xsize;
  unsigned int ysize;
  unsigned int totalCount; //total number of items added into grid


  long * gridOffsets;//Swarm-style offset used to speedup access to values
  long * change; //units of change in items at x,y
  long * count; //how many items are there at x,y
  long * visible; //units inside grid visible from x,y


  int radius;// radius of square around each cell
  unsigned int nhoodWidth; //    radius + 1 + radius
  int nhoodType;  //1=von Neumann neighborhood, else = Moore 
  long * nhoodOffsets;
  BOOL * nhoodInclude; // BOOL indicating a cell is in the
                       // neighborhood of the center point inside the
                       // nhoodWidth x nhoodWidth area.
 
}

+ createBegin: aZone setSizeX: (unsigned)x Y: (unsigned)y;


- (BOOL *)getNhoodMaskPtr;

- (long *)getNhoodOffsets;

- (void)setNhoodRadius: (int)r NhoodType: (int)n EdgeWrap: (BOOL)wrap Sync: (BOOL)sync;


- (void)setNhoodRadius: (int)r;


- (void)setNhoodType: (int)n;


- createNhoodMask;

- createEnd;

- stepRule;

// How many items are there at x,y at the moment?
- (long)getCountAtX: (unsigned)x Y: (unsigned)y;

// From x,y, how many items are visible throughout the neighborhood?
- (long)getNumVisibleAtX: (unsigned)x Y: (unsigned)y;

// Somebody might want to request the whole visible dataset (vector).
- (long*)getPointer;

// If somebody is going to grab the visible dataset, they
// also need the offsets that help to access values in that dataset (vector)
- (long *)getGridOffsets;


// This method does the work of "spreading" out the impact of an
// added item by incrementing the visible items for each cell in the neighbhood.
- (long)updateIntPtr: (long int*)p Impact: (int)input AtX: (unsigned)x Y: (unsigned)y;


// here is the main method through which one can add or remove a number
// of items at x,y
- (long)changeValue: (int) value AtX: (unsigned)x Y: (unsigned)y;



// a convenience method to add one item at x,y
- (long)addOneAtX: (unsigned)x Y: (unsigned)y;

// a convenience method to remove one item at x,y
- (long)removeOneAtX: (unsigned)x Y: (unsigned)y ;


- (void)printDiagnostics;


- (unsigned)wrapCoord: (unsigned)inCoord atModulus: (unsigned)inModulus;

- (unsigned)wrapXCoord: (unsigned)inCoord;

- (unsigned)wrapYCoord: (unsigned)inCoord;


@end


