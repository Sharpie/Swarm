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

public class MousetrapObserverSwarmImpl extends GUISwarmImpl
{
    public int displayFrequency;
    public ScheduleImpl displaySchedule;
    public MousetrapModelSwarmImpl mousetrapModelSwarm;
  
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

    public Object buildObjects ()
    {
        MousetrapModelSwarmCImpl immswarm;
        super.buildObjects();

        mousetrapModelSwarm = new MousetrapModelSwarmImpl();
        immswarm = new MousetrapModelSwarmCImpl(mousetrapModelSwarm);
        mousetrapModelSwarm = 
            (MousetrapModelSwarmImpl) immswarm.create (this.getZone());
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
                           "time", "number triggered");

        Globals.env.setWindowGeometryRecordName (triggerGraph);

        try {
            Selector slct1, slct2;
            slct1 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                  "getNumTriggered", false);
            triggerGraph.createSequence$withFeedFrom$andSelector 
                ("Total triggered", mousetrapModelSwarm.getStats(), slct1);
            //      nag ("first part");
            slct2 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                  "getNumBalls", false);
            //      nag ("done selector");
            triggerGraph.createSequence$withFeedFrom$andSelector 
                ("Pending triggers", mousetrapModelSwarm.getStats(),
                 slct2);
            //      nag ("done sequence");
            
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

        /*
          try
          {
          Selector slct = new Selector (mousetrapDisplay.getClass(),
          "makeProbeAtX$Y$", true);
          displayWindow.setButton$Client$Message (3, mousetrapDisplay,
          slct);
          } catch (Exception e)
          {
	  System.out.println ("Exception make probe: " + e.getMessage());
	  }*/
        return this;
    }
  
    public Object _update_ ()
    {
        if (displayWindow != null)
            displayWindow.drawSelf ();
        return this;
    }
   
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

    public Object activateIn (Object swarmContext)
    {
        ActivityControlCImpl iac;
        super.activateIn (swarmContext);

        mousetrapModelSwarm.activateIn (this);
        displaySchedule.activateIn (this);

        observerActCont = new ActivityControlImpl ();
        iac = new ActivityControlCImpl (observerActCont);
        iac.createBegin (this.getZone());
        nag ("control create begin");
        iac.attachToActivity (this.getActivity());
        nag ("attach to activity"); 
    
        observerActCont = (ActivityControlImpl) iac.createEnd ();
        nag ("control create end");
    
        observerActCont.setDisplayName ("Observer Swarm Controller");

        nag ("archived probe display");
        Globals.env.createArchivedProbeDisplay (observerActCont);
        nag ("probe display");
        return this.getActivity();
    
    }

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
