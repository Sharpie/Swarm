// Java mousetrap application. Copyright © 1999 Santa Fe Institute.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.objectbase.SwarmImpl;

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.ZoneImpl;
import swarm.activity.ActionGroupImpl;
import swarm.activity.ActivityImpl;
import swarm.activity.ScheduleImpl;
import swarm.activity.SwarmActivityImpl;

import swarm.analysis.EZGraphImpl;

public class MousetrapBatchSwarm extends SwarmImpl {
  int loggingFrequency = 1;		     // Frequency of fileI/O
  
  ActionGroupImpl displayActions;	     // schedule data structs
  ScheduleImpl displaySchedule;
  
  MousetrapModelSwarm mousetrapModelSwarm;   // the Swarm we're observing
  
  EZGraphImpl triggerGraph;

  public MousetrapBatchSwarm (ZoneImpl aZone) {
    super (aZone);
  }
  
  public Object buildObjects ()   {
    super.buildObjects();

    mousetrapModelSwarm = new MousetrapModelSwarm (getZone ());

    mousetrapModelSwarm.buildObjects ();
      
    triggerGraph = new EZGraphImpl (getZone (), true);

    try {
      triggerGraph.createSequence$withFeedFrom$andSelector
        ("trigger.output", mousetrapModelSwarm.getStats(), 
         new Selector (mousetrapModelSwarm.getStats().getClass(), 
                       "getNumTriggered", false));
      
      triggerGraph.createSequence$withFeedFrom$andSelector
        ("delta-trigger.output", mousetrapModelSwarm.getStats(), 
         new Selector (mousetrapModelSwarm.getStats().getClass(), 
                       "getNumBalls", false));
      
    } catch (Exception e) {
      System.err.println ("Exception EZGraph: " + e.getMessage ());
    }
    
    return this;
  }  

  public Object buildActions () {
    super.buildActions ();
    
    mousetrapModelSwarm.buildActions ();
    
    if (loggingFrequency > 0) {
      
      displayActions = new ActionGroupImpl (getZone ());
      
      try {
        displayActions.createActionTo$message 
          (triggerGraph, 
           new Selector (triggerGraph.getClass (), "step", true));
        displayActions.createActionTo$message
          (this,
           new Selector (getClass (), "checkToStop", false));
      } catch (Exception e) {
        System.err.println ("Exception batch EZGraph: " + e.getMessage ());
      }
      
      displaySchedule = new ScheduleImpl (getZone (), loggingFrequency);
      
      displaySchedule.at$createAction (0, displayActions);
    }
    
    return this;
  }

  public ActivityImpl activateIn (Object swarmContext) {
    super.activateIn (swarmContext);
    
    mousetrapModelSwarm.activateIn (this);
    
    if (loggingFrequency > 0)
      displaySchedule.activateIn (this);
    
    return getActivity ();
  }
  
  public Object go () {
    System.out.println
      ("You typed `mousetrap --batchmode' or `mousetrap -b'," 
       + " so we're running without graphics.");
    
    System.out.println ("mousetrap is running to completion.");
    
    if (loggingFrequency > 0) 
      System.out.println 
        ("It is logging data every " + loggingFrequency +
         " timesteps to: trigger.output");
    
    ((SwarmActivityImpl) (Object) getActivity ()).run (); 
    
    if (loggingFrequency > 0)
      triggerGraph.drop ();
    
    return getActivity ().getStatus ();
  }
  
  public Object checkToStop () {
    if (mousetrapModelSwarm.getStats ().getNumBalls () == 0) {
      System.out.println ("All the balls have landed!");
      Globals.env.getCurrentSwarmActivity ().terminate (); 
    }
    return this;
  }
}
