// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

// All added comments copyright 2001 Timothy Howe. All rights reserved. 

import java.util.ArrayList;

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;

import swarm.activity.Activity;
import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;

import swarm.objectbase.Swarm;
import swarm.objectbase.VarProbe;
import swarm.objectbase.MessageProbe;
import swarm.objectbase.EmptyProbeMapImpl;

import swarm.gui.Colormap;
import swarm.gui.ColormapImpl;
import swarm.gui.ZoomRaster;
import swarm.gui.ZoomRasterImpl;

import swarm.analysis.EZGraph;
import swarm.analysis.EZGraphImpl;

import swarm.simtoolsgui.GUISwarm;
import swarm.simtoolsgui.GUISwarmImpl;

import swarm.space.Value2dDisplay;
import swarm.space.Value2dDisplayImpl;
import swarm.space.Object2dDisplay;
import swarm.space.Object2dDisplayImpl;

/**
This class implements the GUI-mode observer of the Heatbug model 
defined in HeatbugModelSwarm.java.

<p>
See HeatbugModelSwarm for an overview of the heatbugs application.

*/
public class HeatbugObserverSwarm extends GUISwarmImpl
{

// Todo: make these variables private as much as possible:

// This defines the number of steps after which we display a snapshot of the 
// simulation; we could speed up the simulation by displaying less frequently:
public int displayFrequency = 1;

// This defines the timing of the Swarm's Actions:  
public Schedule displaySchedule;

// This is the model we're observing:
public HeatbugModelSwarm heatbugModelSwarm;
    public HeatbugModelSwarm getHeatbugModelSwarm ()
    { return heatbugModelSwarm; }

// This is the index to the palette we will use to paint heat and Heatbugs:
public Colormap colormap;

// This is the 2-dimensional display we will use to paint both heat
// and Heatbugs:
public ZoomRaster worldRaster;

// This is the time-series graph we will use to display average
// Heatbug unhappiness:
public EZGraph unhappyGraph;

// This is the 2-dimensional graph of the heat; we will display
// it on the ZoomRaster:
public Value2dDisplay heatDisplay;

// This is the 2-dimensional graph of the Heatbugs; we will display
// it on the ZoomRaster, layered over the heatDisplay:
public Object2dDisplay heatbugDisplay;

public HeatbugObserverSwarm (Zone aZone)
{
    super(aZone);

    // Create the model that this observer will observe:
    heatbugModelSwarm = new HeatbugModelSwarm (getZone ());

    // Create a data structure to hold the Probes:
    EmptyProbeMapImpl heatbugObserverProbeMap = new EmptyProbeMapImpl 
     (aZone, getClass ());

    // Create Probes for some variables and methods (see HeatbugModelSwarm.java
    // for an explanation of Probes, ProbeMaps, and ProbeDisplays):
    heatbugObserverProbeMap.addProbe (probeVariable ("displayFrequency"));
    heatbugObserverProbeMap.addProbe (probeMessage ("graphBug:"));

    Globals.env.probeLibrary.setProbeMap$For
     (heatbugObserverProbeMap, getClass ());

    System.out.println 
     (
"\n" +
"In each field you change in the probe display, press Enter.\n" +
"\n" +
"For method invocations, enter an appropriate value in each argument\n" +
"textbox after the method button, then click the button.\n"
     );

} /// constructor

/**
This method activates the schedules so they're ready to run.

@param swarmContext (in)
    the larger context within which this Swarm is activated; an observer swarm 
    is usually the top-level Swarm, so the context is usually null; for
    sub-Swarms such as heatbugModelSwarm, this HeatbugObserverSwarm will be 
    the swarmContext 
*/
public Activity activateIn (Swarm swarmContext)
{
    super.activateIn (swarmContext);
    heatbugModelSwarm.activateIn (this);
    displaySchedule.activateIn (this);
    return getActivity();
}

/**
This method schedules the actions of this GUI observer Swarm. 

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
displaySchedule
|
+-----------------------------------+
|                                   |
ActionGroup                         ActionGroup
updateActions                       tkActions
|                                   |
+-------------+                     |
|             |                     |
Action        Action                Action
this          probeDisplayManager   getActionCache()   
._update_()   .update()             doTkEvents()
</xmp>

<p>
See the documentation in HeatbugModelSwarm.buildActions() for an explanation
of Schedules, ActionGroups, and Actions.

*/
public Object buildActions ()
{
    super.buildActions();

    // Let the model Swarm build its own schedule:
    heatbugModelSwarm.buildActions();

    ActionGroup updateActions = new ActionGroupImpl (getZone());
    ActionGroup tkActions = new ActionGroupImpl (getZone());

    // Define the first Action of ActionGroup updateActions:
    try
    {
    updateActions.createActionTo$message
     (this, new Selector (getClass (), "_update_", false));

    // Define the second Action of ActionGroup updateActions:
    updateActions.createActionTo$message
     (Globals.env.probeDisplayManager,
      new Selector (Globals.env.probeDisplayManager.getClass (), "update", true)
     );

    // Define the sole Action of ActionGroup tkActions:
    tkActions.createActionTo$message
     (getActionCache (),
      new Selector (getActionCache ().getClass (), "doTkEvents", true)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception in setting up tkActions : "
         + e.getMessage ());
    }

    // Define the Schedule:
    displaySchedule = new ScheduleImpl (getZone (), displayFrequency);
      // ... The repeat interval is displayFrequency, so the schedule will
      // begin once every displayFrequency steps of the simulation.
    // Insert the updateActions ActionGroup into the Schedule:
    displaySchedule.at$createAction 
     (0, 
      // ... Execute the ActionGroup at step 0 relative to the beginning of 
      // the schedule.
      updateActions
     );
    // Insert the tkActions ActionGroup into the Schedule:
    displaySchedule.at$createAction 
     (0, 
      // ... Execute the ActionGroup at step 0 relative to the beginning of 
      // the schedule.
      tkActions
     );

    return this;
} /// buildActions()

/**
This method creates the plots and graphs that present the 
results of the simulation. It delegates the building of the Heatbug model 
to HeatbugModelSwarm.buildObjects().

*/
public Object buildObjects ()
{
    super.buildObjects ();

    // Create probe objects on the model and on this observer, to provide
    // GUI channels for reading and writing parameters:
    Globals.env.createArchivedProbeDisplay
     (heatbugModelSwarm, "heatbugModelSwarm");
    Globals.env.createArchivedProbeDisplay (this, "heatbugObserverSwarm");

    // Wait here until the user clicks Start or Next after optionally changing 
    // parameters:
    getControlPanel ().setStateStopped ();

    heatbugModelSwarm.buildObjects ();

    // Create a colormap for displaying Heatbugs and heat:
    colormap = new ColormapImpl (getZone ());

    // Assign colors [ 0.. 63] to shades of red, for heat display;
    // assign colors [64..127] to shades of yellow-green, for Heatbug display:
    for (double i = 0; i < 64; i++)
    {
        colormap.setColor$ToRed$Green$Blue ((byte) i,        i / 63, 0, 0);
        colormap.setColor$ToRed$Green$Blue ((byte) (64 + i), i / 63, 1, 0);
    }

    // Set the colors of the heatbugs from yellow through green (the higher
    // the ideal temperature, the more the yellow):
    double tempRange 
     = heatbugModelSwarm.maxIdealTemp - heatbugModelSwarm.minIdealTemp;
    ArrayList heatbugList = heatbugModelSwarm.getHeatbugList ();
    for (int i = 0; i < heatbugList.size (); i++)
    {
        Heatbug bug = (Heatbug) heatbugList.get (i);
        bug.setColorIndex 
         ((byte) 
          (64 + 63 * 
           (bug.getIdealTemperature () - heatbugModelSwarm.minIdealTemp) 
           / tempRange
          )
         );
    }

    // Create another window for display, and set its attributes:
    worldRaster = new ZoomRasterImpl (getZone (), "worldRaster");
    try
    {
    worldRaster.enableDestroyNotification$notificationMethod
     (this, new Selector (getClass (), "_worldRasterDeath_", false));
    } catch (Exception e)
    {
        System.err.println ("Exception _worldRasterDeath_: " + e.getMessage ());
    }
    worldRaster.setColormap (colormap);
    worldRaster.setZoomFactor (4);
    worldRaster.setWidth$Height
     ((heatbugModelSwarm.getWorld ()).getSizeX (),
      (heatbugModelSwarm.getWorld ()).getSizeY ()
     );
    worldRaster.setWindowTitle ("Heat World");
    worldRaster.pack();                  // draw the window

    // Create a Value2dDisplay, to display the HeatSpace on the ZoomRaster:
    heatDisplay = new Value2dDisplayImpl
     (getZone (), worldRaster, colormap, heatbugModelSwarm.getHeatSpace ());

    heatDisplay.setDisplayMappingM$C (512, 0); // map [0..32767] to [0,63]

    // The Heatbug positional data is in the Grid2d, which we can obtain from 
    // getWorld(). The display widget is the ZoomRaster worldRaster. An 
    // Object2dDisplay knows how to draw such data on such a raster: 
    try
    {
    heatbugDisplay = new Object2dDisplayImpl
     (getZone (),
      worldRaster,
      heatbugModelSwarm.getWorld (),
      new Selector (Class.forName ("Heatbug"), "drawSelfOn", false)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception drawSelfOn: " + e.getMessage ());
    }

    // The Grid2d knows what Heatbugs are on it, and heatbugDisplay has it, so
    // heatbugDisplay could draw it without any more help from us. But it has 
    // getSizeX () times getSizeY () cells. If we give it the Heatbug list,
    // which has only numBugs elements, it can draw the Heatbugs more 
    // efficiently: 
    heatbugDisplay.setObjectCollection
     (heatbugModelSwarm.getHeatbugList ());

    // Tell the world raster to send mouse clicks to the
    // heatbugDisplay. This will allow the user to right-click on the
    // display to probe the bugs:
    try
    {
    worldRaster.setButton$Client$Message
     (3,
      heatbugDisplay,
      new Selector (heatbugDisplay.getClass (), "makeProbeAtX$Y", true)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception makeProbeAtX$Y: " + e.getMessage ());
    }

    // Create the graph widget to display unhappiness:
    unhappyGraph = new EZGraphImpl
     (getZone (),
      "Unhappiness of bugs vs. time",
      "time", 
      "unhappiness",
      "unhappyGraph"
     );

    // Todo: deal with this now-commented-out code:
    // Globals.env.setWindowGeometryRecordName (unhappyGraph, "unhappyGraph");

    // Assign the method to be used for destroying unhappyGraph:
    try
    {
    unhappyGraph.enableDestroyNotification$notificationMethod
     (this,
      new Selector (getClass (), "_unhappyGraphDeath_", false)
     );
    } catch (Exception e)
    {
        System.err.println
         ("Exception _unhappyGraphDeath_: " + e.getMessage ());
    }

    // Create the mechanism for computing the average heatbug unhappiness:
    try
    {
    unhappyGraph.createAverageSequence$withFeedFrom$andSelector
     ("unhappiness", 
      heatbugModelSwarm.getHeatbugList (),
      new Selector (Class.forName ("Heatbug"), "getUnhappiness", false)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception getUnhappiness: " + e.getMessage ());
    }
    return this;
} /// buildObjects()

public void drop ()
{
    if (unhappyGraph != null)
        unhappyGraph.disableDestroyNotification ();
    if (worldRaster != null)
        worldRaster.disableDestroyNotification ();
    super.drop ();
}

public Object graphBug (Heatbug aBug)
{
    if (unhappyGraph != null)
    try
    {
    unhappyGraph.createSequence$withFeedFrom$andSelector
     ("Bug", 
      aBug,
      new Selector (aBug.getClass (), "getUnhappiness", false)
     );
    } catch (Exception e)
    {
        System.err.println ("Exception graphBug: " + e.getMessage());
    }
    return this;
}

public Object _unhappyGraphDeath_ (Object caller)
{
    unhappyGraph.drop ();
    unhappyGraph = null;
    return this;
}

/**
This callback method defines what the observer does whenever the Schedule 
triggers it. 

*/
public Object _update_ ()
{
    if (worldRaster != null)
    {
        heatDisplay.display ();
        heatbugDisplay.display ();
        worldRaster.drawSelf ();
    }

    if (unhappyGraph != null)
        unhappyGraph.step ();
    return this;
}

public Object _worldRasterDeath_ (Object caller)
{
    worldRaster.drop ();
    worldRaster = null;
    return this;
}

private VarProbe probeVariable (String name) 
{
    return Globals.env.probeLibrary.getProbeForVariable$inClass
     (name, HeatbugObserverSwarm.this.getClass ());
}

private MessageProbe probeMessage (String name) {
    return Globals.env.probeLibrary.getProbeForMessage$inClass
     (name, HeatbugObserverSwarm.this.getClass ());
}

} /// class HeatbugObserverSwarm
