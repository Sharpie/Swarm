// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

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

import java.util.List;

/**
 The HeatbugObserverSwarm is a swarm of objects set up to observe a
 Heatbugs model when the graphical interface is running. The most
 important object is the heatbugModelSwarm, but we also have graphical
 windows and data analysis */
public class HeatbugObserverSwarm extends GUISwarmImpl {
  /** one parameter: update freq */
  public int displayFrequency;				
  
  /** ActionGroup for sequence of GUI events */
  public ActionGroup displayActions;				
  /** the single Schedule instance */
  public Schedule displaySchedule;

  /** the Swarm we're observing */
  public HeatbugModelSwarm heatbugModelSwarm;	  	

  /* Lots of display objects. First, widgets */

  /**  allocate colours */
  public Colormap colormap;		
  /**  2d display widget */
  public ZoomRaster worldRaster; 
  /** graphing widget */
  public EZGraph unhappyGraph;			
  
  /* Now, higher order display and data objects */

  /** display the heat */
  public Value2dDisplay heatDisplay;
  /** display the heatbugs */
  public Object2dDisplay heatbugDisplay;	        

  /** Constructor for class */
  public HeatbugObserverSwarm (Zone aZone) {
    super(aZone);

    // Fill in the relevant parameters (only one, in this case).
    displayFrequency = 1;

    // Now, build a customized probe map using a `local' subclass
    // (a special kind of Java `inner class') of the
    // EmptyProbeMapImpl class.  Without a probe map, the default
    // is to show all variables and messages. Here we choose to
    // customize the appearance of the probe, give a nicer
    // interface.

    class HeatbugObserverProbeMap extends EmptyProbeMapImpl {
      private VarProbe probeVariable (String name) {
        return
          Globals.env.probeLibrary.getProbeForVariable$inClass
          (name, HeatbugObserverSwarm.this.getClass ());
      }
      private MessageProbe probeMessage (String name) {
        return
          Globals.env.probeLibrary.getProbeForMessage$inClass
          (name, HeatbugObserverSwarm.this.getClass ());
      }
      private void addVar (String name) {
        addProbe (probeVariable (name));
      }
      private void addMessage (String name) {
        addProbe (probeMessage (name));
      }
      public HeatbugObserverProbeMap (Zone _aZone, Class aClass) {
        super (_aZone, aClass);
        addVar ("displayFrequency");
        addMessage ("graphBug:");
      }
    } 
        
    // Install our custom probeMap class directly into the
    // probeLibrary
    Globals.env.probeLibrary.setProbeMap$For
      (new HeatbugObserverProbeMap (aZone, getClass ()), getClass ());
  }
    
  public Object _worldRasterDeath_ (Object caller) {
    worldRaster.drop ();
    worldRaster = null;
    return this;
  }
    
  public Object _unhappyGraphDeath_ (Object caller) {
    unhappyGraph.drop ();
    unhappyGraph = null;
    return this;
  }
    
  /**
     Create the objects used in the display of the model. This code
     is fairly complicated because we build a fair number of
     widgets. It's also a good example of how to use the display
     code. */
  public Object buildObjects () {
    //int i;
      
    super.buildObjects ();
        
    // First, we create the model that we're actually observing. The
    // model is a subswarm of the observer. 
        
    heatbugModelSwarm = new HeatbugModelSwarm (getZone ());
        
    // Now create probe objects on the model and ourselves. This gives a
    // simple user interface to let the user change parameters.
        
    Globals.env.createArchivedProbeDisplay (heatbugModelSwarm,
                                            "heatbugModelSwarm");
    Globals.env.createArchivedProbeDisplay (this, "heatbugObserverSwarm");
    
    // Instruct the control panel to wait for a button event: we
    // halt here until someone hits a control panel button so the
    // user can get a chance to fill in parameters before the
    // simulation runs
    getControlPanel ().setStateStopped ();
        
    // OK - the user has specified all the parameters for the
    // simulation.  Now we're ready to start.
        
    // First, let the model swarm build its objects.
    heatbugModelSwarm.buildObjects ();
        
    // Now get down to building our own display objects.
        
    // First, create a colormap: this is a global resource, the information
    // here is used by lots of different objects.
    colormap = new ColormapImpl (getZone ());
        
    // Colours [0,64) are assigned to the range Red [0, 1), for
    // heat display.
    for (int i = 0; i < 64; i++)
      colormap.setColor$ToRed$Green$Blue 
        ((byte) i, (double) i / 63.0, 0, 0);
        
    // Colour 64 is set to green, to display heatbugs
    colormap.setColor$ToName ((byte) 64, "green");
        
    // Colour 65 is set to white, used in this case below on
    // probed heatbug.
    colormap.setColor$ToName ((byte) 65, "white");
        
    // Now go in to the heatbugs in the model and set their
    // colours to green (64)
    List heatbugList = heatbugModelSwarm.getHeatbugList ();
        
    for (int i = 0; i < heatbugList.size (); i++) {
      Heatbug bug = (Heatbug) heatbugList.get (i);
      bug.setBugColor ((byte) 64);
    } 
        
    // Next, create a 2d window for display, set its size, zoom
    // factor, title.
    worldRaster = new ZoomRasterImpl (getZone (), "worldRaster");
    try {
      worldRaster.enableDestroyNotification$notificationMethod 
        (this,
         new Selector (getClass (), "_worldRasterDeath_", false));
    } catch (Exception e) {
      System.err.println ("Exception _worldRasterDeath_: " 
                          + e.getMessage ());
    }
        
    worldRaster.setColormap (colormap);
    worldRaster.setZoomFactor (4);
    worldRaster.setWidth$Height 
      ((heatbugModelSwarm.getWorld ()).getSizeX (),
       (heatbugModelSwarm.getWorld ()).getSizeY ());
    worldRaster.setWindowTitle ("Heat World");
    worldRaster.pack();				  // draw the window.
        
    // Now create a Value2dDisplay: this is a special object that
    // will display arbitrary 2d value arrays on a given Raster
    // widget.
    heatDisplay = new Value2dDisplayImpl 
      (getZone (), worldRaster, colormap, heatbugModelSwarm.getHeat ());
        
    heatDisplay.setDisplayMappingM$C (512, 0); // turn [0,32768) -> [0,64)
        
    // And also create an Object2dDisplay: this object draws
    // heatbugs on the worldRaster widget for us, and also
    // receives probes.
    try {
      heatbugDisplay = new Object2dDisplayImpl
        (getZone (), worldRaster, heatbugModelSwarm.getWorld (),
         new Selector (Class.forName ("Heatbug"), "drawSelfOn", false));
    } catch (Exception e) {
      System.err.println ("Exception drawSelfOn: " + e.getMessage ());
    }
        
    heatbugDisplay.setObjectCollection 
      (heatbugModelSwarm.getHeatbugList ()); 
        
    // Also, tell the world raster to send mouse clicks to the
    // heatbugDisplay this allows the user to right-click on the
    // display to probe the bugs.
    try {
      worldRaster.setButton$Client$Message 
        (3, heatbugDisplay, new Selector (heatbugDisplay.getClass (), 
                                          "makeProbeAtX$Y", true));
    } catch (Exception e) {
      System.err.println ("Exception makeProbeAtX$Y: " 
                          + e.getMessage ());
    }
        
    // Create the graph widget to display unhappiness.
    unhappyGraph = new EZGraphImpl 
      (getZone (),
       "Unhappiness of bugs vs. time",
       "time", "unhappiness",
       "unhappyGraph");
        
    //Globals.env.setWindowGeometryRecordName (unhappyGraph, "unhappyGraph"); 
        
    // instruct this _unhappyGraphDeath_ method to be called when
    // the widget is destroyed
    try {
      unhappyGraph.enableDestroyNotification$notificationMethod 
        (this, new Selector (getClass (),
                             "_unhappyGraphDeath_",
                             false));
    } catch (Exception e) {
      System.err.println ("Exception _unhappyGraphDeath_: " 
                          + e.getMessage ());
    }
        
    // create the data for the average heatbug unhappiness
    try {
      unhappyGraph.createAverageSequence$withFeedFrom$andSelector 
        ("unhappiness", heatbugModelSwarm.getHeatbugList (),
         new Selector (Class.forName ("Heatbug"), "getUnhappiness", 
                       false));
    } catch (Exception e) {
      System.err.println ("Exception getUnhappiness: " 
                          + e.getMessage ());
    } 
    return this;
  }  
    
  public Object _update_ ()  {
    if (worldRaster != null) {
      heatDisplay.display ();
      heatbugDisplay.display ();
      worldRaster.drawSelf ();
    }
    
    if (unhappyGraph != null)
      unhappyGraph.step ();
    return this;
  }

  /**
     Create the actions necessary for the simulation. This is where
     the schedule is built (but not run!)  Here we create a display
     schedule - this is used to display the state of the world and
     check for user input. This schedule should be thought of as
     independent from the model - in particular, you will also want
     to run the model without any display.  */
  public Object buildActions () {
    super.buildActions();
        
    // First, let our model swarm build its own schedule.
    heatbugModelSwarm.buildActions();
  
    // Create an ActionGroup for display: a bunch of things that
    // occur in a specific order, but at one step of simulation
    // time. Some of these actions could be executed in parallel,
    // but we don't explicitly notate that here.
    displayActions = new ActionGroupImpl (getZone());

    // Add the methods to the ActionGroup to draw the display of
    // the world
    try {
      displayActions.createActionTo$message 
        (this, new Selector (getClass (), "_update_", false));
        
      // Schedule the update of the probe displays
      displayActions.createActionTo$message
        (Globals.env.probeDisplayManager, 
         new Selector (Globals.env.probeDisplayManager.getClass (),
                       "update", true));
            
      // Finally, schedule an update for the whole user
      // interface code.  This is crucial: without this, no
      // graphics update and the control panel will be
      // dead. It's best to put it at the end of the display
      // schedule
      displayActions.createActionTo$message
        (getActionCache (), new Selector 
          (getActionCache ().getClass (), "doTkEvents", true));
    } catch (Exception e) {
      System.err.println ("Exception in setting up displayActions : " 
                          + e.getMessage ());
    }

    // And the display schedule. Note the repeat interval is set
    // from our own Swarm data structure. Display is frequently
    // the slowest part of a simulation, so redrawing less
    // frequently can be a help.
  
    // note frequency!
    displaySchedule = new ScheduleImpl (getZone (), displayFrequency);
        
    // insert ActionGroup instance on the repeating Schedule
    // instance
    displaySchedule.at$createAction (0, displayActions);
  
    return this;
  }  

  /**
     activateIn: - activate the schedules so they're ready to run.
     The swarmContext argument has to do with what we were activated
     *in*.  Typically the ObserverSwarm is the top-level Swarm, so
     it's activated in "null". But other Swarms and Schedules and
     such will be activated inside of us.  */
  public Activity activateIn (Swarm swarmContext) {
    // First, activate ourselves (just pass along the context).
    super.activateIn (swarmContext);

    // Activate the model swarm in ourselves. The model swarm is a
    // subswarm of the observer swarm.
    heatbugModelSwarm.activateIn (this);

    // Now activate our schedule in ourselves. This arranges for
    // the execution of the schedule we built.
    displaySchedule.activateIn (this);
  
    // Activate returns the swarm activity - the thing that's ready to run.
    return getActivity();
  }
    
  public Object graphBug (Heatbug aBug) {
    if (unhappyGraph != null)
      try {
        unhappyGraph.createSequence$withFeedFrom$andSelector
          ("Bug", aBug, new Selector (aBug.getClass (),  
                                      "getUnhappiness", false));
      } catch (Exception e) {
        System.err.println ("Exception graphBug: " + e.getMessage());
      }
    return this;
  }
  public void drop () {
    if (unhappyGraph != null)
      unhappyGraph.disableDestroyNotification ();
    if (worldRaster != null)
      worldRaster.disableDestroyNotification ();
    super.drop ();
  }
}
