// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;
import swarm.defobj.SymbolImpl;

import swarm.defobj.FArguments;
import swarm.defobj.FArgumentsImpl;
import swarm.defobj.FCall;
import swarm.defobj.FCallImpl;

import swarm.activity.Activity;
import swarm.activity.ScheduleActivity;
import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.FActionForEach;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.VarProbe;
import swarm.objectbase.MessageProbe;
import swarm.objectbase.EmptyProbeMapImpl;

import java.util.ArrayList;

import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

/**
Since this class is the core of the jheatbugs simulation, we present 
here an overview of the entire application.

<p>
The following diagram depicts the structure of the six custom classes of the
jheatbugs application:

<xmp>
|                     +-----------------+
|                     | StartHeatbugs   |
|                     +-----------------+
|                              | 1
|                              |
|              +---------------+-----------+
|              |                           |
|              | 1                         | 1
|   +---------------------+          +------------------------+
|   | HeatbugBatchSwarm   |  - OR -  | HeatbugObserverSwarm   |
|   | extends             |          | extends                |
|   | SwarmImpl           |          | GUISwarmImpl           |
|   +---------------------+          +------------------------+
|              | 1                         | 1
|              |                           |
|              +---------------+-----------+
|                              |
|                              | 1
|                    +---------------------+
|                    | HeatbugModelSwarm   |-----------------+
|                    | extends             | 1               |
|                    | SwarmImpl           |                 |
|                    +---------------------+                 |
|                              | 1                           | 1
|                              |                     +---------------+
|                              |                     | HeatSpace     |
|                              |                     | extends       |
|                              |                     | Diffuse2dImpl |
|                              |                     +---------------+
|                              | 0..n                        | 1
|                      +-----------------+                   |
|                      |     Heatbug     |-------------------+
|                      +-----------------+ 0..n
</xmp>

Five of the custom classes involved in the simulation are:

 <dir>
 1. HeatbugModelSwarm:
 the Heatbug simulation mechanism, which also manages supporting classes 
 such as Heatbug and HeatSpace, and is available for 
 viewing through either of the two observer classes.
 </dir>

 <dir>
 2. StartHeatbugs: the class with the <tt>main()</tt> method that 
 kicks off the simulation. Depending on the command-line option <i>-b</i>,
 StartHeatbugs creates either a HeatbugBatchSwarm or a HeatbugObserverSwarm
 in which to present the view of the HeatbugModelSwarm.
 </dir>

 <dir>
 3. HeatbugBatchSwarm: 
 the non-GUI observer: a view of the model presented through
 text output. <i>Batch</i> does not refer to a
 batch of Heatbugs, but rather to batch mode as opposed to GUI mode.
 HeatbugBatchSwarm writes to files and to standard output.
 </dir>

 <dir>
 4. HeatbugObserverSwarm:
 the GUI observer: a view of the model presented through
 graphs and plots.
 </dir>

 <dir>
 5. Heatbug: the class that defines the individual
 agents. A Heatbug has data (including its position in 2-dimensional
 space), behavior (including the ability to move in 2-dimensional space),
 and the ability to perceive quantities of heat in its immediate neighborhod.
 </dir>

We use two 2-dimensional data structures in the model: one to record
the positions of the Heatbugs, and another, of identical size, to record the
heat that the Heatbugs produce as well as seek.
The following diagram provides a rough illustration of the two data
structures:

<xmp>
^ Position:                          ^ Heat:
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   |   |   |   |   |   |   |  | |   |   |   |   |   |   |   |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   |   |   |   |   |   |   |  | |   | h | h | h |   |   |   |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   | b |   |   |   |   |   |  | |   | h | h | h |   |   |   |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   | b |   |   |   |   |   |  | |   | h | h | h |   |   |   |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   |   |   |   |   |   |   |  | |   | h | h | h | h | h | h |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
y |   |   |   |   |   | b |   |   |  y |   |   |   |   | h | h | h |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| |   |   |   |   |   |   |   |   |  | |   |   |   |   | h | h | h |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
0 |   |   |   |   |   |   |   |   |  0 |   |   |   |   |   |   |   |   |
| +---+---+---+---+---+---+---+---+  | +---+---+---+---+---+---+---+---+
| --0-------x--------------------->  | --0-------x--------------------->
</xmp>

where b represents a bug and h represents a non-zero quantity of heat.

<p>
HeatbugObserverSwarm displays the positions and the heat together on a 
ZoomRaster. HeatbugBatchSwarm does not report the positions or heat, but only
the average unhappiness of the Heatbugs.

<p>
For the model's positional data,
we use Swarm's class Grid2d. For the heat data,
we use Swarm's Diffuse2dImpl, whose behavior includes the dissipation
and "evaporation" of heat.
Rather than use Diffuse2dImpl directly, we need to specialize it, so
we define HeatSpace, the last of the six custom classes involved in the
Heatbug simulation:

 <dir>
 6. HeatSpace: a 2-dimensional array of heat values.
 We don't specify the heat units, but they might be degrees Celsius.
 HeatSpace inherits diffusion and evaporation behaviors from Diffuse2dImpl, 
 and it also provides the ability to
 report the hottest or coldest cells within a neighborhood.
 </dir>

*/

public class HeatbugModelSwarm extends SwarmImpl
{

// Begin variables referenced by the SCM file -- they must be public:
public int numBugs = 101;
    public void setNumBugs (int numBugs) 
    { if (numBugs != -1) this.numBugs = numBugs; }
    // ... See StartHeatbugs.java for an explanation of the value -1. 
public int minIdealTemp = 17000;
public int maxIdealTemp = 31000;
public int minOutputHeat = 3000;
public int maxOutputHeat = 10000;
public boolean randomizeHeatbugUpdateOrder = false;
public double randomMoveProbability = 0.4;
    public double getRandomMoveProbability ()
    { return randomMoveProbability; }
    public void setRandomMoveProbability (double randomMoveProbability)
    { this.randomMoveProbability = randomMoveProbability; }
// ... End variables referenced by the SCM file -- they must be public.

private int _worldXSize = 80;
private int _worldYSize = 80;
private Schedule _modelSchedule;
private ArrayList _heatbugList;
    public ArrayList getHeatbugList ()
    { return _heatbugList; }
private Grid2d _world;
    public Grid2d getWorld ()
    { return _world; }
private HeatSpace _heatSpace;
    public HeatSpace getHeatSpace ()
    { return _heatSpace; }
private FActionForEach _actionForEach;
private boolean _immobile = false;
// ... If _immobile == true we will see what happens when Heatbugs never move. 
    public boolean getImmobile () { return _immobile; }
    public void setImmobile (boolean immobile) { _immobile = immobile; }
private boolean _startInOneCluster = false;
// ... If _startInOneCluster == true we will see what happens when Heatbugs all
// start in a contiguous cluster.
    public boolean getStartInOneCluster () { return _startInOneCluster; }
    public void setStartInOneCluster (boolean startInOneCluster) 
    { _startInOneCluster = startInOneCluster; }
private int _printDiagnostics = 0;
    public void setPrintDiagnostics (int printDiagnostics) 
    { _printDiagnostics = printDiagnostics; }
// Todo: _diffusionConstant and some other variables are copies of variables in
// Diffuse2d -- can we figure out a way to get rid of them?
private double _diffusionConstant = 1.0; 
// ... 0 = minimum, 1 = maximum diffusion of heat in _heatSpace.
    public double getDiffusionConstant ()
    { return _diffusionConstant; }
    public Object setDiffusionConstant (double diffusionConstant)
    { _diffusionConstant = diffusionConstant; return this; }
private double _evaporationRate = 0.99; 
// ... 0 = minimum, 1 = maximum retention of heat in _heatSpace.
    public double getEvaporationRate ()
    { return _evaporationRate; }
    public Object setEvaporationRate (double evaporationRate)
    { _evaporationRate = evaporationRate; return this; }
// ... According to the documentation for Diffuse2d, newHeat = "evapRate * 
// (self + diffusionConstant*(nbdavg - self)) where nbdavg is the weighted 
// average of the 8 neighbours" -- but what does "weighted" mean?

/**
The only task this constructor performs is to construct and install a ProbeMap. 

<p>
A Probe is a mechanism through which any object can access a 
variable or method you define in a Swarm. In practice, Probes are used 
almost exclusively by a Swarm GUI
to provide interactive access to variable and methods. 

<p>
A ProbeMap is a set of Probes, stored as a Map. 
(A Map is a set of name-value pairs. 
In a ProbeMap, each name is the name of the variable or method; 
each value is a pointer to the variable or method itself.)

<p>
All the Probes in a ProbeMap must belong to the same class; and all the
Probes in a particular class must be in one ProbeMap (unless you are choosing
one among several ProbeMaps at run time).

<p>
A ProbeDisplay is a GUI widget that presents a ProbeMap interactively. 
You can see an example by running this application in GUI mode: note that 
the variables and methods shown in the ProbeDisplay for HeatbugModelSwarm 
exactly match the variables and methods 
listed in addProbe() statements in this constructor.

<p>
Probes do not use getters and setters. The variables and methods in a 
ProbeMap need not be public. (But variables
referenced by the SCM file, which is used in batch mode, must be public.)  

<p>
Because Probes do not use getters and setters, you cannot use Probes to
control write access differently from read access. 

<p>
By default, a ProbeMap will contain <i>all</i> the variables of the specified 
class. To control explicitly which variables are in the ProbeMap, create
an EmptyProbeMap rather than a ProbeMap, and insert into 
the EmptyProbeMap the variables and methods you want, as we do in this method.

<p>
ProbeMaps are collected into a ProbeLibrary. A Swarm has at most one 
ProbeLibrary, and a ProbeLibrary has one ProbeMap for each class that has
one or more Probes, as illustrated in this diagram:

<xmp>
swarmObject1
|
|
|
probeLibrary (for swarmObject1)
|
+----------------------------+
|                            |
probeMap1 (for Class1)       probeMap2 (for Class2)
|                            |
+------+------+              +-------+-------+-------+----------+
|      |      |              |       |       |       |          |
var11  var12  method11       var21   var22   var23   method21   method22
</xmp>

*/
public HeatbugModelSwarm (Zone aZone)
{
    super (aZone);

    EmptyProbeMapImpl heatbugModelProbeMap = new EmptyProbeMapImpl
     (aZone, getClass ());

    heatbugModelProbeMap.addProbe (probeVariable ("numBugs"));
    heatbugModelProbeMap.addProbe (probeVariable ("minIdealTemp"));
    heatbugModelProbeMap.addProbe (probeVariable ("maxIdealTemp"));
    heatbugModelProbeMap.addProbe (probeVariable ("minOutputHeat"));
    heatbugModelProbeMap.addProbe (probeVariable ("maxOutputHeat"));
    heatbugModelProbeMap.addProbe (probeVariable ("randomMoveProbability"));
    heatbugModelProbeMap.addProbe (probeVariable ("_printDiagnostics"));
    heatbugModelProbeMap.addProbe (probeVariable ("_diffusionConstant"));
    heatbugModelProbeMap.addProbe (probeVariable ("_evaporationRate"));
    heatbugModelProbeMap.addProbe (probeVariable ("_worldXSize"));
    heatbugModelProbeMap.addProbe (probeVariable ("_worldYSize"));
    // The number of colons after the name of each method must match the number
    // of arguments in the method's signature:
    heatbugModelProbeMap.addProbe (probeMessage ("addHeatbugs:"));
    heatbugModelProbeMap.addProbe (probeMessage ("toggleRandomizedOrder"));

    Globals.env.probeLibrary.setProbeMap$For
     (heatbugModelProbeMap, getClass ());

} /// constructor

/**
This method activates the schedules so they're ready to run.

<p>
Todo: explain more thoroughly what this method does. Explain who calls it.
Explain what an Activity is.  
 
@param swarmContext (in)
    the larger context within which this model is activated; a model swarm
    usually runs in the context of an observer swarm or a super-model swarm;
    the effect of specifying a context is to specify a native memory 
    allocation zone
*/
public Activity activateIn (Swarm swarmContext)
{
    super.activateIn (swarmContext);
    // Add our own schedule to the base class's schedule:
    _modelSchedule.activateIn (this);
    // Return the schedule that we inherit from the base class:
    return getActivity ();
} /// activateIn()

/**
If Heatbugs gave birth or immigrated, you could use this method to introduce
them into the model. 

<p>
Since the program does not consult numBugs after initialization, invocation
of this method from the probe display after the simulation has started will
have no effect. 
*/
public Object addHeatbugs (int numNewBugs)
{
    numBugs += numNewBugs;
    System.out.println ("I will add " + numNewBugs + " new Heatbugs.");
    return this;
}

/**
This method schedules the actions of this model Swarm. 

<p>
Why do we need to "build actions"? Why can't we just write standard Java
methods? -- they build actions, don't they?

<p>
The answer is that Swarm's simulation engine runs in Objective-C code, behind 
the Java Native Interface (JNI). To invoke the behavior of the objects we model
in Java, the simulation engine relies on callbacks, which are methods that we
ask the engine to invoke at the appropriate times. We use 
<tt>buildActions()</tt> to define those methods and the timing of the calls.

<p>
This Swarm contains the Schedules, ActionGroups, and Actions depicted in the 
following diagram. 

<xmp>
Swarm
this
|
|
|
Schedule
modelSchedule
|
+-------------------------------------------------------+
|                                                       |
ActionGroup                                             implied
modelActions                                            ActionGroup
|                                                       |
+--------------+--------------------+                   |
|              |                    |                   |
Action         Action               Action              Action
_heatSpace     _heatbugList.get()   _heatSpace          this
.stepRule()    .heatbugStep()       .updateLattice()    .modelStep()
</xmp>

<p>
In the remainder of this documentation section we discuss Schedules, 
ActionGroups, Actions, repeat intervals, and time values. If you're happy
letting Swarm invoke every behavior at every step of the simulation, 
you don't need
to understand all this -- just create one Schedule and no ActionGroups, and
for each behavioral method in your simulation invoke 

<xmp>
    <schedule>.createActionTo$message
     (this, new Selector (this.getClass (), <methodname>, false))
</xmp>

or another <tt>createAction...()</tt> or <tt>createFAction...()</tt> method.

<p>
A Schedule is a three-level hierarchy whose full structure
is illustrated by this example:

<xmp>
Simulation
|
+-----------------+
|                 |
Schedule          Schedule
|                 |
|                 +-------------+-------------+------------+--------+--------+
|                 |             |             |            |        |        |
ActionGroup       ActionGroup   ActionGroup   ActionGroup  |        |        |
|                 |             |             |            |        |        |
+--------+        |             |             |            |        |        |
|        |        |             |             |            |        |        |
Action   Action   Action        Action        Action       Action   Action   Action
</xmp>

Consider first a Schedule with this subset of that full sample structure:

<xmp>
Simulation
|
+-----------------+
|                 |
Schedule          Schedule
RI = 1            RI = 3
|                 |
|                 +-------------+-------------+
|                 |             |             |
ActionGroup       ActionGroup   ActionGroup   ActionGroup
TV = 0            TV = 0        TV = 1        TV = 2
|                 |             |             |
+--------+        |             |             |
|        |        |             |             |
Action   Action   Action        Action        Action
</xmp>

where RI indicates the <i>repeat interval</i> of a Schedule, 
as specified in the invocation of the Schedule, and
TV indicates the <i>time value</i> of an ActionGroup, 
as specified in the invocation 
of the Schedule's method <tt>at$createAction()</tt>. 

<p>
That Schedule would have the following timeline:

<xmp>
|       |       |       |       |       |       |       |
S1      S1      S1      S1      S1      S1      S1      S1 
S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0
|       |       |       |       |       |       |       |
S3      |       |       S3      |       |       |       S3
S3-tv0  |       |       S3-tv0  |       |       |       S3-tv0
|       S3-tv1  |       |       S3-tv1  |       |       |
|       |       S3-tv2  |       |       S3-tv2  |       |
|       |       |       S3-tv3  |       |       S3-tv3  | Error: see note.
|       |       |       |       |       |       |       |
time --->
</xmp>

where

 <p><dir>
 S1 represents the Schedule with a repeat interval of 1;
  <p><dir>
  S1-tv0 represents the ActionGroup in S1 (with a repeat interval of 0);
  </dir>
 </dir>

 <p><dir>
 S3 represents the Schedule with a repeat interval of 3;
  <p><dir>
  S3-tv0 represents the ActionGroup in S3 with a time value of 0;
  </dir>
  <p><dir>
  S3-tv1 represents the ActionGroup in S3 with a time value of 1;
  </dir>
  <p><dir>
  and so on.
  </dir>
 </dir>

<p>
Note: We introduced Action S3-tv3 into the diagram to illustrate what
happens when the time value of an action group is greater than or equal to
the repeat interval of its ActionGroup. S3-tv3 will cause a run-time error,
because its time value will make the ActionGroup 
"run over" into the next iteration of the Schedule. 

<p>
As depicted in the first illustration at the top of this section, you can also
insert an Action directly into the Schedule, rather than insert the Action
into an ActionGroup that you insert into the Schedule. 
If you insert Actions directly into a Schedule, 
you must specify the time value of the Action, and (as for ActionGroups)
the time value must be less than the repeat inverval of the Schedule. 
The Schedule collects all Actions with a particular time value
into an implied ActionGroup with that time value. For 
example, inserting two Actions
with a time value of 0 and one with a time value of 1 directly into
a Schedule with a repeat interval of 3
would result in the following timeline:

<xmp>
|       |       |       |       |       |       |       |
S3      |       |       S3      |       |       S3      |
IAG-tv0 |       |       IAG-tv0 |       |       IAG-tv0 |
a-tv0-p |       |       a-tv0-p |       |       a-tv0-p |
a-tv0-q |       |       a-tv0-q |       |       a-tv0-q |
|       |       |       |       |       |       |       |
|       IAG-tv1 |       |       IAG-tv1 |       |       IAG-tv1
|       a-tv1   |       |       a-tv1   |       |       a-tv1
|       |       |       |       |       |       |       |
time --->
</xmp>

where

 <p><dir>
 IAG-tv0 represents an implied ActionGroup with a time value of 0;
  <p><dir>
  a-tv0-p represents an Action with a time value of 0;
  </dir>
  <p><dir>
  a-tv0-q represents the other Action with a time value of 0; and
  </dir>
 </dir>

 <p><dir>
 IAG-tv1 represents an implied ActionGroup with a time value of 1;
  <p><dir>
  a-tv1 represents the Action with a time value of 1.
  </dir>
 </dir>

<p>
In conclusion, the following diagram and timeline summarize the entire
discussion above, without presenting any additional scheduling concepts:

<xmp>
Simulation
|
+-----------------+
|                 |
Schedule          Schedule
RI = 1            RI = 3
|                 |
|                 +-------------+-------------+------------+-----------------+
|                 |             |             |            |                 |
ActionGroup       ActionGroup   ActionGroup   ActionGroup  IAG-tv0           IAG-tv1
TV = 0            TV = 0        TV = 1        TV = 2       |                 |
|                 |             |             |            |                 |
+--------+        |             |             |            +--------+        |
|        |        |             |             |            |        |        |
Action   Action   Action        Action        Action       Action   Action   Action
-        -        -             -             -            TV = 0   TV = 0   TV = 1
</xmp>

<xmp>
|       |       |       |       |       |       |       |
S1      S1      S1      S1      S1      S1      S1      S1 
S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0  S1-tv0
|       |       |       |       |       |       |       |
S3      |       |       S3      |       |       |       S3
S3-tv0  |       |       S3-tv0  |       |       |       S3-tv0
|       S3-tv1  |       |       S3-tv1  |       |       |
|       |       S3-tv2  |       |       S3-tv2  |       |
|       |       |       S3-tv3  |       |       S3-tv3  | Error: TV >= RI.
IAG-tv0 |       |       IAG-tv0 |       |       IAG-tv0 |
a-tv0-p |       |       a-tv0-p |       |       a-tv0-p |
a-tv0-q |       |       a-tv0-q |       |       a-tv0-q |
|       |       |       |       |       |       |       |
|       IAG-tv1 |       |       IAG-tv1 |       |       IAG-tv1
|       a-tv1   |       |       a-tv1   |       |       a-tv1
|       |       |       |       |       |       |       |
time --->
</xmp>

*/
public Object buildActions ()
{
    super.buildActions();

    ActionGroup modelActions = new ActionGroupImpl (getZone ());

    // Define the first Action of ActionGroup modelActions:
    try
    {
    modelActions.createActionTo$message
     (_heatSpace, new Selector (_heatSpace.getClass (), "stepRule", false));
      // ... "stepRule" is the name of a callback method in Diffuse2d, which
      // HeatSpace inherits from. It is Diffuse2d's sole non-inherited 
      // behavioral method. In other words, it's the only thing that a 
      // Diffuse2d as such knows how to do at any step of the simulation. 
      // In particular, it applies diffusion and evaporation.
      // 
      // ... We haven't yet figured out what "false" does; the Swarm Reference
      // Guide has no entry for Selector. 
    } catch (Exception e)
    { System.err.println ("Exception stepRule: " + e.getMessage ()); }

    // Define the second Action of ActionGroup modelActions:
    try
    {
    // Use Heatbug #0 as a prototype for indicating the class that the 
    // heatbugStep Action will access and for traversing _heatbugList:
    Heatbug proto = (Heatbug) _heatbugList.get (0);
    Selector sel = new Selector (proto.getClass (), "heatbugStep", false);
    _actionForEach = modelActions.createFActionForEachHomogeneous$call
     (_heatbugList,
      new FCallImpl (this, proto, sel, new FArgumentsImpl (this, sel, true))
     );
    } catch (Exception e)
    { e.printStackTrace (System.err); }
    // Tell Swarm to update Heatbugs sequentially, not randomly: 
    _actionForEach.setDefaultOrder (Globals.env.Sequential);

    // Define the third Action of ActionGroup modelActions:
    try
    {
    modelActions.createActionTo$message
     (_heatSpace, new Selector (_heatSpace.getClass (), "updateLattice", false));
      // ... "updateLattice" is the name of a callback method (see the 
      // discussion of stepRule above). It is a method in DblBuffer2d. 
      // HeatSpace extends Diffuse2d, which extends Ca2d, which extends 
      // DblBuffer2d.  
    } catch (Exception e)
    { System.err.println ("Exception updateLattice: " + e.getMessage ()); }

    // Define the Schedule:
    _modelSchedule = new ScheduleImpl 
     (getZone (), 
      1 
      // ... The repeat interval is 1, so _modelSchedule will begin every step
      // of the simulation.
     );

    // Insert the ActionGroup modelActions into the Schedule:
    _modelSchedule.at$createAction 
     (0,
      // ... The Schedule will execute ActionGroup modelActions at time 
      // value 0 relative to the beginning of the Schedule.
      modelActions
     );

    // Define the sole Action of the implied ActionGroup:
    try
    {
    _modelSchedule.createActionTo$message
     (this, new Selector (this.getClass (), "modelStep", false));
    // ... This method invocation implies a time value of 0; it is equivalent 
    // to invoking _modelSchedule.at$createActionTo$message (0, ...).
    } catch (Exception e)
    { System.err.println ("Exception modelStep: " + e.getMessage ()); }

    return this;
} /// buildActions()

/**
This method constructs the custom objects of this model Swarm. 

<p>
Why do we need to "build objects"? Why can't we just use standard Java
constructors? -- they build objects, don't they?

<p>
The answer is that Swarm's simulation engine runs in Objective-C code, behind 
the Java Native Interface (JNI). We use <tt>buildObjects()</tt> to define the 
data structures that the Swarm infrastructure will replicate behind JNI.

*/
public Object buildObjects ()
{
    // Let our parent class build anything it needs to:
    super.buildObjects();

    // Create a 2-dimensional array of Heatbug positions:
    _world = new Grid2dImpl (getZone (), _worldXSize, _worldYSize);

    // Create a HeatSpace, which is a 2-dimensional array of heat values:
    _heatSpace = new HeatSpace
     (getZone (), 
      _worldXSize, 
      _worldYSize, 
      _diffusionConstant, 
      _evaporationRate, 
      _printDiagnostics
     );

    // Create a list to keep track of the Heatbugs:
    _heatbugList = new ArrayList ();

    int x = 0;
    int y = 0;
    // Create numBugs Heatbugs:
    for (int heatbugIndex = 0; heatbugIndex < numBugs; heatbugIndex++)
    {

        // Create a Heatbug:
        Heatbug heatbug = new Heatbug 
         (_world, 
          _heatSpace, 
          this, 
          heatbugIndex, 
          _printDiagnostics
         );

        // Add the bug to the end of the list:
        _heatbugList.add (heatbug);

        // Randomly choose an ideal temperature and an output heat, within the
        // allowable range:
        int idealTemp =
         Globals.env.uniformIntRand.getIntegerWithMin$withMax
          (minIdealTemp, maxIdealTemp);
        int outputHeat =
         Globals.env.uniformIntRand.getIntegerWithMin$withMax
          (minOutputHeat, maxOutputHeat);

        // Initialize the rest of the Heatbug's state:
        heatbug.setIdealTemperature (idealTemp);
        heatbug.setOutputHeat (outputHeat);
        heatbug.setRandomMoveProbability (randomMoveProbability);

        _world.setOverwriteWarnings (true);
        if (_startInOneCluster)
        {
            // This would be all we'd need, if collisions were OK:
            /// heatbug.setX$Y (_worldXSize/2, _worldYSize/5);
            // But we're avoiding collisions, so:
            // We will allow no collisions, so we'll squeeze them into a box
            // about sqrt (numBugs) high and by sqrt (numBugs) wide:
            heatbug.setX$Y 
             ((_worldXSize/2 + x) % _worldXSize, 
              (_worldYSize/5 + y) % _worldYSize
             );
            if (++x >= Math.pow (numBugs, 0.5))
            {
                x = 0;
                ++y;
            }
        }
        else
        {
            // For simpler code, we allow collisions here: -- the Heatbugs 
            // quickly separate themselves: 
            heatbug.setX$Y
             (Globals.env.uniformIntRand.getIntegerWithMin$withMax
               (0, (_worldXSize-1)),
              Globals.env.uniformIntRand.getIntegerWithMin$withMax
               (0, (_worldYSize-1))
             );
            // ... We could eliminate collision-warning messages by invoking
            // world.setOverwriteWarnings (false) before this loop. We could
            // run more safely by setting it true again after the loop. But we
            // could, with small probability, still get a warning message if
            // two Heatbugs are initialized at the same cell and, being hemmed
            // in by other Heatbugs, they both choose to stay in the cell. 
        }
        if (_printDiagnostics >= 1)
            System.out.println 
             ("I initialized Heatbug " + heatbug + ".");
    } /// for each Heatbug

    return this;
} /// buildObjects()

public Object modelStep ()
{
    _heatSpace.setPrintDiagnostics (_printDiagnostics);
    // Monitor the heat at an arbitrary cell (2, 2) (HeatSpace monitors 
    // the same cell):
    int x = 2; int y = 2;
    if (_printDiagnostics >= 10)
        System.out.println 
         ("In modelStep(), at step "
          + getActivity ().getScheduleActivity ().getCurrentTime ()
          + ", heat at (" + x + ", " + y + ") is "
          + _heatSpace.getValueAtX$Y (x, y) + "."
         );
    // See if total heat is a function of the number of steps:
    if (_printDiagnostics >= 20)
    {
        double totalHeat = _heatSpace.totalHeat ();
        System.out.println ("Total heat / step count is " + totalHeat 
         / getActivity ().getScheduleActivity ().getCurrentTime () + ".");
    }
    return this;
}

private MessageProbe probeMessage (String name)
{
    return Globals.env.probeLibrary.getProbeForMessage$inClass
     (name, HeatbugModelSwarm.this.getClass ());
}

private VarProbe probeVariable (String name)
{
    return Globals.env.probeLibrary.getProbeForVariable$inClass
     (name, HeatbugModelSwarm.this.getClass ());
}

/** This method toggles the default Heatbug update order between sequential 
(which is the default default order) and random. Sequential order may make 
experiments easier to debug. Random order eliminates any systematic bias that 
might result from sequential updating.  
*/
public boolean toggleRandomizedOrder ()
{
    if (_actionForEach == null)
    {
        String msg = "Ignoring attempt to toggle randomized order because _actionForEach is null.";
        System.err.println ("stderr: " + msg);
        System.out.println ("stdout: " + msg);
        return false;
    }
    _actionForEach.setDefaultOrder
     (_actionForEach.getDefaultOrder () == Globals.env.Sequential
      ? Globals.env.Randomized
      : Globals.env.Sequential
     );
    System.out.println ("I toggled randomized order to " + (_actionForEach.getDefaultOrder () == Globals.env.Randomized) + ".");
    return _actionForEach.getDefaultOrder () == Globals.env.Randomized;
}

} /// class HeatbugModelSwarm
