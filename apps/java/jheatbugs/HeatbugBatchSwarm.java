// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import swarm.Globals;
import swarm.Selector;

import swarm.defobj.Zone;

import swarm.activity.Activity;
import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;

import swarm.analysis.EZGraph;
import swarm.analysis.EZGraphImpl;

/**
This class implements the batch-mode observer of the Heatbug model defined in
HeatbugModelSwarm.java.

<p>
See HeatbugModelSwarm for an overview of the heatbugs application.
*/

public class HeatbugBatchSwarm extends SwarmImpl 
{

// Begin variables referenced by the SCM file -- they must be public:
// This is the number of steps to run the simulation:
public int experimentDuration;
// This is the number of steps to run before writing to the log:
public int loggingFrequency;
// ... End variables referenced by the SCM file -- they must be public.

private String _outputFilename = "unhappiness.output";
private Schedule _displaySchedule;
private Schedule _stopSchedule;
// The Swarm we're observing:
private HeatbugModelSwarm _heatbugModelSwarm;
    public HeatbugModelSwarm getHeatbugModelSwarm ()
    { return _heatbugModelSwarm; }

// We will put the EZGraph in file mode rather than the usual graphics mode:
private EZGraph unhappyGraph;

public HeatbugBatchSwarm (Zone aZone) {
    super (aZone);
    _heatbugModelSwarm = (HeatbugModelSwarm) 
     Globals.env.lispAppArchiver.getWithZone$key (getZone (), "modelSwarm");
    // ... The lispAppArchiver is created automatically from the SCM file. 
} /// constructor

public Activity activateIn (Swarm swarmContext) 
{
    super.activateIn (swarmContext);
    _heatbugModelSwarm.activateIn (this);
    _stopSchedule.activateIn (this);
    if (loggingFrequency > 0)
        _displaySchedule.activateIn (this);
    return getActivity ();
}

/**
This method schedules the actions of this batch Swarm. 

<p>
This Swarm contains the Schedules, ActionGroups, and Actions depicted in the 
following diagram. 

<xmp>
Swarm
this
|
+-----------------------------------+
|                                   |
Schedule                            Schedule
_displaySchedule                    _stopSchedule
|                                   |
|                                   |
|                                   |
ActionGroup                         implied
displayActions                      ActionGroup
|                                   |
|                                   |
|                                   |
Action                              Action
unhappygraph                        this
.step()                             .stopRunning()
</xmp>

<p>
See the documentation in HeatbugModelSwarm.buildActions() for an explanation
of Schedules, ActionGroups, and Actions.

*/
public Object buildActions () 
{
    super.buildActions();

    // Let the model Swarm build its own schedule:
    _heatbugModelSwarm.buildActions();

    if (loggingFrequency > 0) 
    {

        // Define the Schedule to run once every loggingFrequency steps:
        _displaySchedule = new ScheduleImpl (getZone (), loggingFrequency);

        // Define the explicit ActionGroup:
        ActionGroup displayActions = new ActionGroupImpl (getZone ());

        // Add to the ActionGroup an Action to write the output of the
        // graph to _outputFilename:
        try
        {
        displayActions.createActionTo$message
         (unhappyGraph,
          new Selector (unhappyGraph.getClass (), "step", true)
         );
        } catch (Exception e)
        {
            System.err.println
             ("Exception batch unhappyGraph step: " + e.getMessage ());
        }

        // Insert the ActionGroup displayActions into the Schedule:
        _displaySchedule.at$createAction 
         (0,
          // ... The Schedule will execute ActionGroup displayActions at 
          // time-step 0 relative to the beginning of the Schedule.
          displayActions
         );
    }

    _stopSchedule = new ScheduleImpl (getZone (), true);
    // ... The "true" indicates autoDrop: unlike _displaySchedule, 
    // _stopSchedule does not have a repeatInterval, because we will 
    // run it just once (at time-step experimentDuration) and then 
    // drop it from the Schedule. 

    // Define the sole Action of the implied ActionGroup:
    try
    {
    _stopSchedule.at$createActionTo$message
     (experimentDuration, 
      this, 
      new Selector (getClass (), "stopRunning", false)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception stopRunning: " + e.getMessage ());
    }
    return this;
} /// buildActions()

public Object buildObjects () 
{
    super.buildObjects();

    // Let the model Swarm build its objects:
    _heatbugModelSwarm.buildObjects ();

    // A user who sets loggingFrequency to 0 is requesting no logging at all:
    if (loggingFrequency > 0) 
    {
        unhappyGraph =  new EZGraphImpl (getZone (), true);
        try
        {
        // Create a time-series graph of average values:
        unhappyGraph.createAverageSequence$withFeedFrom$andSelector
          // ... writing the output to a file:
         (_outputFilename,
          // ... averaging over all the Heatbugs:
          _heatbugModelSwarm.getHeatbugList (),
          // ... using values obtained from Heatbug.getUnhappiness ():
          new Selector (Class.forName("Heatbug"), "getUnhappiness", false)
         );
        } catch (Exception e)
        {
            System.err.println
             ("Exception batch getUnhappiness: " + e.getMessage ());
        }
    }
    return this;
} /// buildObjects()

public Object go () 
{
    System.out.println
     ("You specified the -b or --batch option,"
      + " so we're running without graphics."
     );
    System.out.println
     ("The Heatbugs are running for " + experimentDuration + " timesteps.");
    if (loggingFrequency > 0)
        System.out.println
         ("I am logging data every " + loggingFrequency
          + " timesteps to: " + _outputFilename + "."
         );

    getActivity ().getSwarmActivity ().run ();

    if (loggingFrequency > 0)
// todo: explain this:
        // Close the output file:
        unhappyGraph.drop ();

    return getActivity ().getStatus ();
}

public Object stopRunning () {
    // Terminate the simulation.
    System.out.println
     ("Quitting after step " + Globals.env.getCurrentTime () + ".");
    Globals.env.getCurrentSwarmActivity ().terminate ();
    return this;
}

}
