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

public class MousetrapBatchSwarmImpl extends SwarmImpl
{
    int loggingFrequency = 1;		     // Frequency of fileI/O

    ActionGroupImpl displayActions;	     // schedule data structs
    ScheduleImpl displaySchedule;

    MousetrapModelSwarmImpl mousetrapModelSwarm;   // the Swarm we're observing
    
    EZGraphImpl triggerGraph;

    public void nag (String s)
    {
        System.out.println (this.getClass().getName() + ":" + s);
        System.out.flush ();
    }

    public MousetrapBatchSwarmImpl (ZoneImpl aZone)
    {
        super(aZone);
    }
    
    public Object buildObjects ()
    {
        super.buildObjects();

        mousetrapModelSwarm 
            = new MousetrapModelSwarmImpl((ZoneImpl)this.getZone());

        mousetrapModelSwarm.buildObjects();
      
        triggerGraph = new EZGraphImpl((ZoneImpl)this.getZone(), true);

        try {
        
            triggerGraph.createSequence$withFeedFrom$andSelector
                ("trigger.output", mousetrapModelSwarm.getStats(), 
                 new Selector ((mousetrapModelSwarm.getStats()).getClass(), 
                               "getNumTriggered", false));

            triggerGraph.createSequence$withFeedFrom$andSelector
                ("delta-trigger.output", mousetrapModelSwarm.getStats(), 
                 new Selector ((mousetrapModelSwarm.getStats()).getClass(), 
                               "getNumBalls", false));
        
        } catch (Exception e) {
            System.out.println ("Exception EZGraph: " + e.getMessage());
        }
    
        return this;
    }  

    public Object buildActions()
    {
        super.buildActions();
  
        mousetrapModelSwarm.buildActions();

        if (loggingFrequency > 0) {
            
            displayActions = new ActionGroupImpl((ZoneImpl)this.getZone());
      
            try {
                
                displayActions.createActionTo$message 
                    (triggerGraph, new Selector 
                        (triggerGraph.getClass(), "step", true));
                displayActions.createActionTo$message (this, new Selector
                    (this.getClass(), "checkToStop", false));
            } catch (Exception e) {
                System.out.println ("Exception batch EZGraph: " 
                                    + e.getMessage());
            }
            
            displaySchedule = new
                ScheduleImpl ((ZoneImpl)this.getZone(), loggingFrequency);
            
            displaySchedule.at$createAction (0, displayActions);
        }
    
        return this;
    }


    public Object activateIn (Object swarmContext)
    {
    
        super.activateIn(swarmContext);
    
        mousetrapModelSwarm.activateIn(this);
    
        if (loggingFrequency > 0)
            displaySchedule.activateIn(this);
    
        return this.getActivity();
    }
  
    public Object go ()
    {
        System.out.println
            ("You typed `mousetrap --batchmode' or `mousetrap -b'," 
             + " so we're running without graphics.");
  
        System.out.println("mousetrap is running to completion.");
 
        if (loggingFrequency > 0) 
            System.out.println 
                ("It is logging data every " + loggingFrequency +
                 " timesteps to: trigger.output");
    
        ((SwarmActivityImpl) this.getActivity()).run(); 
    
        if (loggingFrequency > 0)
            triggerGraph.drop();
    
        return ((SwarmActivityImpl) this.getActivity()).getStatus();
    }
    
    public Object checkToStop ()
    {
        if (((MousetrapStatistics)mousetrapModelSwarm.getStats()).
            getNumBalls() == 0)
            {
                System.out.println("All the balls have landed!");
                (Globals.env.getCurrentSwarmActivity()).terminate(); 
            }
    
        return this;
    }
}



