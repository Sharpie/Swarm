// Java mousetrap application. Copyright © 1999 Santa Fe Institute.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.simtoolsgui.GUISwarmImpl;

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.ZoneImpl;
import swarm.objectbase.EmptyProbeMapImpl;
import swarm.objectbase.ActivityControlImpl;
import swarm.objectbase.SwarmImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.ActionGroupImpl;
import swarm.activity.ActivityImpl;
import swarm.gui.ColormapImpl;
import swarm.gui.ZoomRasterImpl;
import swarm.analysis.EZGraphImpl;
import swarm.space.Object2dDisplayImpl;

/**
 * The MousetrapObserverSwarm is the top-level swarm that watches
 * and reports on what's happening in the MousetrapModelSwarm. It
 * is like the lab-bench on which the mousetrap world is located,
 * along with the various instruments that we construct to monitor
 * that world. */
public class MousetrapObserverSwarm extends GUISwarmImpl {
  public int displayFrequency;
  public ScheduleImpl displaySchedule;
  public MousetrapModelSwarm mousetrapModelSwarm;
  
  public ColormapImpl colormap;
  public ZoomRasterImpl displayWindow;
  public EZGraphImpl triggerGraph;
  
  public Object2dDisplayImpl mousetrapDisplay;
  
  public ActivityControlImpl observerActCont;
  public ActionGroupImpl displayActions;
  
  /**
   * MousetrapObserverSwarm constructor: since we are only interested in
   * subclassing from the `USING' phase object, this constructor does
   * the work of the createBegin, createEnd methods in Objective C */
  public MousetrapObserverSwarm (ZoneImpl aZone) {
    super (aZone);
    
    EmptyProbeMapImpl probeMap;
    
    displayFrequency = 1;
    probeMap = new EmptyProbeMapImpl (aZone, getClass ());
    
    probeMap.addProbe 
      (Globals.env.probeLibrary.getProbeForVariable$inClass
       ("displayFrequency", getClass ()));
    
    Globals.env.probeLibrary.setProbeMap$For (probeMap, getClass ());
  }
  
  public void noMethod (Object a) {
  }
  
  public Object _setupMousetraps_ () {
    int x, y, size;
    
    size = mousetrapModelSwarm.getGridSize ();
    for (x = 0; x < size; x++)
      for (y = 0; y < size; y++) {
        Mousetrap trap = mousetrapModelSwarm.getMousetrapAtX$Y (x, y);
        if (trap != null) {
          if (displayWindow != null)
            displayWindow.drawPointX$Y$Color (x, y, (byte) 1);
          trap.setDisplayWidget (displayWindow);
        }
      }
    return this;
  }
  
  public Object _displayWindowDeath_ (Object caller) {
    displayWindow.drop ();
    displayWindow = null;
    _setupMousetraps_ ();
    return this;
  }
  
  public Object _scheduleItemCanvasDeath_ (Object caller) {
    _setupMousetraps_ ();
    return this;
  }
  
  /**
   * Create the objects used in the display of the model.  Here, we
   * create the objects used in the experiment. Primarily, the
   * MousetrapModelSwarm instance, itself, but also the various
   * instrumentation that observes the model.  */
  public Object buildObjects () {
    super.buildObjects ();

    mousetrapModelSwarm = new MousetrapModelSwarm (getZone ());
    
    Globals.env.createArchivedProbeDisplay (mousetrapModelSwarm);
    Globals.env.createArchivedProbeDisplay (this);
    
    getActionCache ().waitForControlEvent ();
    
    if (getControlPanel ().getState () == Globals.env.ControlStateQuit)
      return null;
    
    mousetrapModelSwarm.buildObjects ();
    
    colormap = new ColormapImpl (getZone ());
    
    colormap.setColor$ToGrey ((byte) 1, 0.3);
    colormap.setColor$ToName ((byte) 2, "red");
    
    triggerGraph = new EZGraphImpl (getZone (),
                                    "Trigger data vs. time",  
                                    "number triggered",
                                    "time");
    
    Globals.env.setWindowGeometryRecordName (triggerGraph);
    
    try {
      triggerGraph.createSequence$withFeedFrom$andSelector 
        ("Total triggered",
         mousetrapModelSwarm.getStats (),
         new Selector (mousetrapModelSwarm.getStats ().getClass (),
                       "getNumTriggered", false));
      triggerGraph.createSequence$withFeedFrom$andSelector 
        ("Pending triggers",
         mousetrapModelSwarm.getStats (),
         new Selector (mousetrapModelSwarm.getStats ().getClass (),
                       "getNumBalls", false));
    } catch (Exception e) { 
      System.out.println ("Exception trigger : " + e.getMessage ());
    }
    
    displayWindow = new ZoomRasterImpl (getZone ());
    
    Globals.env.setWindowGeometryRecordName (displayWindow);

    try {
      displayWindow.
        enableDestroyNotification$notificationMethod
        (this, new Selector (getClass (), "_displayWindowDeath_", false));
    } catch (Exception e) {
      System.out.println ("Exception display window: " + e.getMessage ());
    }
    
    displayWindow.setColormap (colormap);
    displayWindow.setZoomFactor (6);
    displayWindow.setWidth$Height (mousetrapModelSwarm.getGridSize (),
                                   mousetrapModelSwarm.getGridSize ());
    displayWindow.setWindowTitle ("Mousetrap World");
    _setupMousetraps_ ();
    displayWindow.pack ();
    
    try {
      mousetrapDisplay = new Object2dDisplayImpl
        (getZone (), 
         (Object) displayWindow, 
         (Object) mousetrapModelSwarm.getWorld (),
         new Selector (Class.forName ("Mousetrap"), "noMethod", false));
    }
    catch (Exception e) {
      System.out.println ("Exception no method:" + e.getMessage ());
    }
    
    try  {
      displayWindow.setButton$Client$Message
        (3, mousetrapDisplay, 
         new Selector (mousetrapDisplay.getClass (), "makeProbeAtX$Y", true));
    } catch (Exception e)  {
      System.out.println ("Exception makeProbeAtX$Y$ ZoomRasterImpl: " 
                          + e.getMessage());
    }
    return this;
  }
  
  public Object _update_ () {
    if (displayWindow != null)
      displayWindow.drawSelf ();
    return this;
  }
  
  /**
   * Create the actions necessary for the simulation. This is where
   * the schedule is built (but not run!)  Here we create a
   * displaySchedule - this is used to display the state of the world
   * and check for user input. This schedule should be thought of as
   * independent from the model - in particular, you will also want to
   * run the model without any display.  */
  public Object buildActions () {
    super.buildActions ();
    mousetrapModelSwarm.buildActions ();
    
    displayActions = new ActionGroupImpl (getZone ());
    displaySchedule = new ScheduleImpl (getZone (), displayFrequency);
    
    try {
      displayActions.createActionTo$message
        (this, new Selector (getClass (), "_update_", false));
      
      displayActions.createActionTo$message
        (triggerGraph, 
         new Selector (triggerGraph.getClass(), "step", true));
      
      displayActions.createActionTo$message
        (Globals.env.probeDisplayManager,  
         new Selector (Globals.env.probeDisplayManager.getClass (), 
                       "update", true));
      
      displayActions.createActionTo$message
        (this, new Selector (getClass (), "checkToStop", true));
      
      displayActions.createActionTo$message
        (getActionCache (),
         new Selector (getActionCache ().getClass (), "doTkEvents", true));
      
      displaySchedule.at$createAction (0, displayActions);
    } catch (Exception e) {
      System.out.println ("Exception: " + e.getMessage ());
    }
    return this;
  }
  
  /**
   * activate the schedules so they're ready to run.  The swarmContext
   * argument has to do with what we were activated *in*.  Typically
   * the ObserverSwarm is the top-level Swarm, so it's activated in
   * `nil'. But other Swarms and Schedules and such will be activated
   * inside of us.
   **/
  public ActivityImpl activateIn (SwarmImpl swarmContext) {
    super.activateIn (swarmContext);
    
    mousetrapModelSwarm.activateIn (this);
    displaySchedule.activateIn (this);
    
    observerActCont = new ActivityControlImpl (getZone ());
    observerActCont.attachToActivity (getActivity ());
    
    observerActCont.setDisplayName ("Observer Swarm Controller");
    
    Globals.env.createArchivedProbeDisplay (observerActCont);
    
    return getActivity ();
  }

  /**
   * monitor method - if all the balls have landed, time to quit!
   **/
  public Object checkToStop () {
    if (mousetrapModelSwarm.getStats ().getNumBalls () == 0) {
      System.out.println ("All balls have landed!\n");
      getControlPanel ().setStateStopped ();
    }
    return this;
  }

  public void drop () {
    if (displayWindow != null)
      displayWindow.disableDestroyNotification ();
    super.drop ();
  }
}

