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

class MousetrapBatchSwarmCImpl extends SwarmCImpl
{
    public MousetrapBatchSwarmCImpl ()
    {
        super();
    }
    
    public MousetrapBatchSwarmCImpl (Object nextPhase)
    {
        super(nextPhase);
    }
}

public class MousetrapBatchSwarmImpl extends SwarmImpl
{
  int loggingFrequency = 1;		       	 // Frequency of fileI/O

  ActionGroupImpl displayActions;		 // schedule data structs
  ScheduleImpl displaySchedule;

  MousetrapModelSwarmImpl mousetrapModelSwarm;	 // the Swarm we're observing

  public void nag (String s)
  {
    System.out.println (this.getClass().getName() + ":" + s);
    System.out.flush ();
  }

  public MousetrapBatchSwarmImpl (ZoneImpl aZone)
    {
        super();
        new MousetrapBatchSwarmCImpl ((Object) this).create(aZone);
    }
    
  public Object buildObjects ()
    {
      super.buildObjects();

    MousetrapModelSwarmCImpl immswarm;
    
    mousetrapModelSwarm = new MousetrapModelSwarmImpl();
    immswarm = new MousetrapModelSwarmCImpl(mousetrapModelSwarm);
    mousetrapModelSwarm = 
      (MousetrapModelSwarmImpl) immswarm.create (this.getZone());

    mousetrapModelSwarm.buildObjects();
    
    /* implement logging to EZGraphImpl, here */
    
    return this;
  }  

  public Object printValues()
  {
    System.out.println((mousetrapModelSwarm.getStats()).getNumTriggered() 
                     + " " + (mousetrapModelSwarm.getStats()).getNumBalls());
    return this;
  }

  public Object buildActions()
  {
    super.buildActions();
  
    mousetrapModelSwarm.buildActions();

    if (true) {
            
        displayActions = new ActionGroupImpl((ZoneImpl)this.getZone());
      
        try {

            displayActions.createActionTo$message (this, new Selector 
                (this.getClass(), "printValues", false));
            displayActions.createActionTo$message (this, new Selector
                (this.getClass(), "checkToStop", false));
        } catch (Exception e) {
            System.out.println ("Exception batch printValues: " 
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
    
    if (true)  // logging
      displaySchedule.activateIn(this);
    
    return this.getActivity();
  }
  
  public Object go ()
  {
    System.out.println
      ("You typed `mousetrap --batchmode' or `mousetrap -b'," 
       + " so we're running without graphics.");
  
    System.out.println("mousetrap is running to completion.");
 
    if (true)  // logging frequency
        System.out.println 
            ("It is logging data every" + loggingFrequency +
             "timesteps to: trigger.output");
  
    ((SwarmActivityImpl) this.getActivity()).run(); 
    
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



