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
    public  SwarmEnvironment se;
    public int displayFrequency;
    public ScheduleImpl displaySchedule;
    public MousetrapModelSwarmImpl mousetrapModelSwarm;
  
    public ColormapImpl colormap;
    public  ZoomRasterImpl displayWindow;
    public EZGraphImpl triggerGraph;

    public Object2dDisplayImpl mousetrapDisplay;

    public ActivityControlImpl observerActCont;
    public  ActionGroupImpl displayActions;
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
        ZoneImpl modelZone = new ZoneImpl ();
        ZoneCImpl imodelZone = new ZoneCImpl (modelZone);
        ZoomRasterCImpl izr;
        Object2dDisplayCImpl io2d;
        MousetrapModelSwarmCImpl immswarm;
        ColormapCImpl icmp;
        EZGraphCImpl iezg;
        super.buildObjects();
        //    nag ("zone create");
        modelZone = (ZoneImpl) imodelZone.create(this.getZone());
        //    nag ("mousetrapmodelswarm");
        mousetrapModelSwarm = new MousetrapModelSwarmImpl ();
        immswarm = new MousetrapModelSwarmCImpl(mousetrapModelSwarm);
        immswarm.se = se;
        //    nag ("create");
        mousetrapModelSwarm = 
            (MousetrapModelSwarmImpl) immswarm.create (this.getZone());
        mousetrapModelSwarm.se = se;
        //    nag ("createArchivedProbeDisplay (mousetrapModelSwarm)");
        se.createArchivedProbeDisplay (mousetrapModelSwarm);
        //    nag ("createArchivedProbeDisplay (this)");
        se.createArchivedProbeDisplay (this);
        //    nag ("Action cache");
        ((ActionCacheImpl)getActionCache()).waitForControlEvent();
        //    nag ("control panel");
        if (((ControlPanelImpl)this.getControlPanel()).getState() == se.ControlStateQuit)
            return this;
        //    nag ("mousetrapModel swarm buildObjects");
        mousetrapModelSwarm.buildObjects ();
        //    nag ("colormpa");
        colormap = new ColormapImpl ();
        icmp = new ColormapCImpl (colormap);
    
        colormap = (ColormapImpl) icmp.create (se.globalZone);
    
        colormap.setColor$ToGrey ((byte) 1, 0.3);
        colormap.setColor$ToName ((byte) 2, "red");
    
        triggerGraph = new EZGraphImpl ();
        iezg = new EZGraphCImpl (triggerGraph);

        iezg.createBegin(this.getZone());
        se.setWindowGeometryRecordName (iezg);
        iezg.setTitle ("Tirgger data vs. time");
        iezg.setAxisLabelsX$Y ("time", "number triggered");
        triggerGraph = (EZGraphImpl) iezg.createEnd();

        try {
      
            Selector slct1, slct2;
            slct1 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                  "getNumTriggered", false);
            triggerGraph.createSequence$withFeedFrom$andSelector ("Total triggered",
                                                                  mousetrapModelSwarm.getStats(), slct1);
            //      nag ("first part");
            slct2 = new Selector (mousetrapModelSwarm.getStats().getClass(),
                                  "getNumBalls", false);
            //      nag ("done selector");
            triggerGraph.createSequence$withFeedFrom$andSelector ("Pending triggers",
                                                                  mousetrapModelSwarm.getStats(),
                                                                  slct2);
            //      nag ("done sequence");
				   
        } 
        catch (Exception e) 
            { 
                System.out.println ("Exception trigger : " + e.getMessage());
      
            }
        displayWindow = new ZoomRasterImpl ();
        //    nag ("display window");
        izr = new ZoomRasterCImpl (displayWindow);
        izr.createBegin (se.globalZone);
        //    nag (" izr");
        se.setWindowGeometryRecordName (izr);
        //    nag (" se.setWindowGeometryRecordName (izr);");
        displayWindow = (ZoomRasterImpl) izr.createEnd ();
        //    nag ("done display Window createEnd");
        try 
            {
                Selector slct;
                slct = new Selector (this.getClass(), "_displayWindowDeath_", false);
	
                displayWindow.enableDestroyNotification$notificationMethod (this, 
                                                                            slct);
            } catch (Exception e)
                {
                    System.out.println ("Exception display window: " + e.getMessage());
                }
        displayWindow.setColormap (colormap);
        displayWindow.setZoomFactor (6);
        displayWindow.setWidth$Height (mousetrapModelSwarm.getGridSize (),
                                       mousetrapModelSwarm.getGridSize ());
        displayWindow.setWindowTitle ("Mousetrap World");
        this._setupMousetraps_();
        displayWindow.pack();

        mousetrapDisplay = new Object2dDisplayImpl ();
        io2d = new Object2dDisplayCImpl (mousetrapDisplay);
    
        io2d.createBegin (this.getZone());
        io2d.setDisplayWidget (displayWindow);
        io2d.setDiscrete2dToDisplay (mousetrapModelSwarm.getWorld());
        try {
            /// strange noMehtod:
            //      nag ("strange method");
            Selector slct = new Selector (Class.forName ("Mousetrap"), "noMethod", false);
            io2d.setDisplayMessage (slct);
    
        }
        catch (Exception e) { 
            System.out.println ("Exception no mehtod:" + e.getMessage());
        }
        //    nag ("$$");
        mousetrapDisplay = (Object2dDisplayImpl) io2d.createEnd();
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
        ActionGroupCImpl iag;
        ScheduleCImpl isch;
        Selector slct;
        super.buildActions();
        //    nag ("mousetrap model actions");
        mousetrapModelSwarm.buildActions();
    
        displayActions = new ActionGroupImpl();
    
        iag = new ActionGroupCImpl (displayActions);
        displayActions = (ActionGroupImpl) iag.create (this.getZone());
        //    nag ("display actions done");
        displaySchedule = new ScheduleImpl();
        isch = new ScheduleCImpl (displaySchedule);
    
        isch.createBegin (this.getZone());
        isch.setRepeatInterval (displayFrequency);
        displaySchedule = (ScheduleImpl) isch.createEnd();
        //    nag ("display schedule");
        try
            {
	
                slct = new Selector (this.getClass(), "_update_", false);
                displayActions.createActionTo$message (this, slct);
	
                slct = new Selector (triggerGraph.getClass(), "step", true);
                displayActions.createActionTo$message (triggerGraph, slct);
                slct = new Selector (se.probeDisplayManager.getClass(), "update", 
                                     true);
                displayActions.createActionTo$message (se.probeDisplayManager, slct);
	
                slct = new Selector (this.getClass (), "checkToStop", true);
                displayActions.createActionTo$message (this, slct);

                slct = new Selector (this.getActionCache().getClass (), 
                                     "doTkEvents", true);
                displayActions.createActionTo$message (this.getActionCache(), 
                                                       slct);
	
                displaySchedule.at$createAction (0, displayActions);
                //	nag ("display schedule done");
            } catch (Exception e) 
                {
                    System.out.println ("Exception doTkE: " + e.getMessage());
                }
        return this;
    }

    public Object activateIn (Object swarmContext)
    {
        ActivityControlCImpl iac;
        super.activateIn (swarmContext);
        //    nag ("super");
        mousetrapModelSwarm.activateIn (this);
        //    nag ("mousetrapmodel");
        displaySchedule.activateIn (this);
        //    nag ("display scheudle");
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
        se.createArchivedProbeDisplay (observerActCont);
        nag ("probe display");
        return this.getActivity();
    
    }

    public Object checkToStop ()
    {
        if (((MousetrapStatistics)mousetrapModelSwarm.getStats()).getNumBalls() == 0)
            {
                System.out.println ("All balls have landed!\n");
                ((ControlPanelImpl)this.getControlPanel()).setStateStopped();
            }
        return this;
    }
			
}
