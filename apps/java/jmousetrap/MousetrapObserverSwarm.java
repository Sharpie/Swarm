import swarm.*;
import swarm.activity.*;
import swarm.objectbase.*;
import swarm.simtoolsgui.*;
import swarm.random.*;
import swarm.defobj.*;
import swarm.gui.*;
import swarm.analysis.*;
import swarm.space.*;
import swarm.random.*;

/**
 * The MousetrapObserverSwarm is the top-level swarm that watches
 * and reports on what's happening in the MousetrapModelSwarm. It
 * is like the lab-bench on which the mousetrap world is located,
 * along with the various instruments that we construct to monitor
 * that world. */
public class MousetrapObserverSwarm extends GUISwarmImpl
{
    public int displayFrequency;
    public ScheduleImpl displaySchedule;
    public MousetrapModelSwarm mousetrapModelSwarm;
  
    public ColormapImpl colormap;
    public ZoomRasterImpl displayWindow;
    public EZGraphImpl triggerGraph;

    public Object2dDisplayImpl mousetrapDisplay;

    public ActivityControlImpl observerActCont;
    public ActionGroupImpl displayActions;

    public void nag (String s)
    {
        System.out.println (this.getClass().getName() + ":" + s);
        System.out.flush ();
    }

    /**
     * MousetrapObserverSwarm constructor: since we are only interested in
     * subclassing from the `USING' phase object, this constructor does
     * the work of the createBegin, createEnd methods in Objective C */
    public MousetrapObserverSwarm (ZoneImpl aZone)
    {
        super(aZone);
        
        EmptyProbeMapImpl probeMap;
        
        displayFrequency = 1;
        nag("Observer: probeMap");
        probeMap = new EmptyProbeMapImpl (aZone, this.getClass());

        nag("Observer: probeMap addProbe\n");
        probeMap.addProbe 
            (Globals.env.probeLibrary.getProbeForVariable$inClass
             ("displayFrequency", this.getClass()));
        
        nag("Observer: probeLibrary.setProbeMap$For");
        
        Globals.env.probeLibrary.setProbeMap$For (probeMap, this.getClass());
    }

    public void noMethod (Object a)
    {
    }

    public Object _setupMousetraps_ ()
    {
        int x, y, size;
    
        size = mousetrapModelSwarm.getGridSize();
        for (x = 0; x < size; x++)
            for (y = 0; y < size; y++) {
                Mousetrap trap = mousetrapModelSwarm.getMousetrapAtX$Y (x, y);
                if (trap != null)
                    {
                        if (displayWindow != null)
                            displayWindow.drawPointX$Y$Color (x, y, (byte) 1);
                        trap.setDisplayWidget (displayWindow);
                    }
            }
        return this;
    }
    
    public Object _displayWindowDeath_ (Object caller)
    {
        displayWindow.drop();
        displayWindow = null;
        this._setupMousetraps_();
        return this;
    }

    public Object _scheduleItemCanvasDeath_ (Object caller)
    {

        this._setupMousetraps_();
        return this;
    }


  /**
   * Create the objects used in the display of the model.  Here, we
   * create the objects used in the experiment. Primarily, the
   * MousetrapModelSwarm instance, itself, but also the various
   * instrumentation that observes the model.  */
    public Object buildObjects ()
    {
        super.buildObjects();

        mousetrapModelSwarm 
            = new MousetrapModelSwarm((ZoneImpl)this.getZone());

        Globals.env.createArchivedProbeDisplay (mousetrapModelSwarm);
        Globals.env.createArchivedProbeDisplay (this);
        
        ((ActionCacheImpl)getActionCache()).waitForControlEvent();
        
        if (((ControlPanelImpl)this.getControlPanel()).getState() 
            == Globals.env.ControlStateQuit)
            return this;

        mousetrapModelSwarm.buildObjects ();
        
        colormap = new ColormapImpl ((ZoneImpl)this.getZone());
    
        colormap.setColor$ToGrey ((byte) 1, 0.3);
        colormap.setColor$ToName ((byte) 2, "red");
    
        triggerGraph = 
          new EZGraphImpl ((ZoneImpl)this.getZone(), "Trigger data vs. time",  
                           "number triggered", "time");

        Globals.env.setWindowGeometryRecordName (triggerGraph);
        
        try {
            Selector slct1, slct2;
            slct1 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                "getNumTriggered", false);
            triggerGraph.createSequence$withFeedFrom$andSelector 
                ("Total triggered", mousetrapModelSwarm.getStats(),
                 slct1);
            slct2 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                  "getNumBalls", false);
            triggerGraph.createSequence$withFeedFrom$andSelector 
                ("Pending triggers", mousetrapModelSwarm.getStats(),
                 slct2);
        } catch (Exception e) { 
          System.out.println ("Exception trigger : " + e.getMessage());
        }

        displayWindow = new ZoomRasterImpl ((ZoneImpl)this.getZone());

        Globals.env.setWindowGeometryRecordName (displayWindow);

        try {
            Selector slct;
            slct = new Selector (this.getClass(), 
                                 "_displayWindowDeath_", false);
            
            displayWindow.enableDestroyNotification$notificationMethod (this, 
                                                                        slct);
        } catch (Exception e) {
            System.out.println ("Exception display window: " + e.getMessage());
        }
        
        displayWindow.setColormap (colormap);
        displayWindow.setZoomFactor (6);
        displayWindow.setWidth$Height (mousetrapModelSwarm.getGridSize (),
                                       mousetrapModelSwarm.getGridSize ());
        displayWindow.setWindowTitle ("Mousetrap World");
        this._setupMousetraps_();
        displayWindow.pack();

        try {
            Selector slct = new Selector (Class.forName ("Mousetrap"), 
                                          "noMethod", false);
            mousetrapDisplay = new Object2dDisplayImpl 
                ((ZoneImpl)this.getZone(), displayWindow, 
                 mousetrapModelSwarm.getWorld(), slct);
        }
        catch (Exception e) {
            System.out.println ("Exception no method:" + e.getMessage());
        }

        try  {
            Selector slct = new Selector (mousetrapDisplay.getClass(),
                                          "makeProbeAtX$Y", true);
            displayWindow.setButton$Client$Message (3, mousetrapDisplay,
                                                slct);
        } catch (Exception e)  {
            System.out.println ("Exception makeProbeAtX$Y$ on ZoomRasterImpl: " 
                                + e.getMessage());
        }
        return this;
    }
  
    public Object _update_ ()
    {
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
    public Object buildActions ()
    {
        Selector slct;
        
        super.buildActions();
        mousetrapModelSwarm.buildActions();
    
        displayActions = new ActionGroupImpl ((ZoneImpl)this.getZone());

        displaySchedule = new ScheduleImpl((ZoneImpl)this.getZone(), 
                                           displayFrequency);

        try {
                
            slct = new Selector (this.getClass(), "_update_", false);
            displayActions.createActionTo$message (this, slct);
            
            slct = new Selector (triggerGraph.getClass(), "step", true);
            displayActions.createActionTo$message (triggerGraph, slct);
            
            slct = new Selector (Globals.env.probeDisplayManager.getClass(), 
                                 "update", true);
            displayActions.createActionTo$message 
                (Globals.env.probeDisplayManager,  slct);
            
            slct = new Selector (this.getClass (), "checkToStop", true);
            displayActions.createActionTo$message (this, slct);
            
            slct = new Selector (this.getActionCache().getClass (), 
                                 "doTkEvents", true);
            displayActions.createActionTo$message (this.getActionCache(), 
                                                       slct);
            
            displaySchedule.at$createAction (0, displayActions);
            
        } catch (Exception e) {
            System.out.println ("Exception doTkE: " + e.getMessage());
        }
        
        return this;
    }

  /**
   * activate the schedules so they're ready to run.  The swarmContext
   * argument has to do with what we were activated *in*.  Typically
   * the ObserverSwarm is the top-level Swarm, so it's activated in
   * "nil". But other Swarms and Schedules and such will be activated
   * inside of us.
   **/
    public Object activateIn (Object swarmContext)
    {
        super.activateIn (swarmContext);

        mousetrapModelSwarm.activateIn (this);
        displaySchedule.activateIn (this);

        observerActCont = new ActivityControlImpl ((ZoneImpl)this.getZone());
        observerActCont.attachToActivity (this.getActivity());
    
        observerActCont.setDisplayName ("Observer Swarm Controller");

        Globals.env.createArchivedProbeDisplay (observerActCont);

        return this.getActivity();
    
    }
  
  /**
   * monitor method - if all the balls have landed, time to quit!
   **/
  public Object checkToStop ()
  {
    if (((MousetrapStatistics)mousetrapModelSwarm.getStats()).
        getNumBalls() == 0)
      {
        System.out.println ("All balls have landed!\n");
        ((ControlPanelImpl)this.getControlPanel()).setStateStopped();
      }
    return this;
  }
}
