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
import swarm.objectbase.SwarmImpl;

import swarm.analysis.EZGraph;
import swarm.analysis.EZGraphImpl;

public class HeatbugBatchSwarm extends SwarmImpl {

    /** Frequency of fileI/O */
    public int loggingFrequency;	       	       
    
    /** When to Stop the Sim */
    public int experimentDuration;               
    
    public ActionGroup displayActions;		
    public Schedule displaySchedule;
    public Schedule stopSchedule;
    
    /** the Swarm we're observing */
    public HeatbugModelSwarm heatbugModelSwarm;	  

    /** The EZGraph will be used in FileI/O mode rather than the usual
        Graphics mode... */
    public EZGraph unhappyGraph;                        

    public HeatbugBatchSwarm (Zone aZone) {
        super (aZone);
    }
    
    public Object buildObjects () {
        super.buildObjects();
        
        // But since we don't have any graphics, we load the
        // object from the global `lispAppArchiver' instance which
        // is created automatically from the file called
        // `heatbugs.scm'
        
        // `modelSwarm' is the key in `heatbugs.scm' which
        // contains the instance variables for the
        // HeatbugModelSwarm class, such as numBugs etc.

        heatbugModelSwarm =
          (HeatbugModelSwarm)
          Globals.env.lispAppArchiver.getWithZone$key (getZone (),
                                                       "modelSwarm");

        // Now, let the model swarm build its objects.
        heatbugModelSwarm.buildObjects ();
        
        // Finally, build some data analysis objects. In this case
        // we're just going to create an EZGraph (with graphics turned
        // off and fileI/O turned on) collect some statistics (the
        // average) over the collection of heatbugs (which we get from
        // the heatbugModelSwarm).
        
        // If the user sets loggingFrequency to 0 s/he does not
        // require the logging of results at all. Consequently, some
        // objects will not be created -> the schedule will also be
        // simplified. This sort of switch is useful when the Sim
        // could potentially log many different aspects of the
        // model...
        if(loggingFrequency > 0) {
            unhappyGraph =  new EZGraphImpl (getZone (), true);
            
            try {
                unhappyGraph.createAverageSequence$withFeedFrom$andSelector
                    ("unhappiness.output", 
                     heatbugModelSwarm.getHeatbugList (), new Selector 
                         (Class.forName("Heatbug"), "getUnhappiness", 
                          false));
            } catch (Exception e) {
                System.err.println ("Exception batch getUnhappines: "
                                    + e.getMessage ());
            }
        }
        // All done - we're ready to build a schedule and go.
        return this;
    }  
    
    /** Create the actions necessary for the simulation. This
        is where the schedule is built (but not run!) */
    public Object buildActions () {
        super.buildActions();
        
        // First, let our model swarm build its own schedule.
        heatbugModelSwarm.buildActions();
        
        if (loggingFrequency > 0) {
            
            // Create an ActionGroup for display. This is pretty
            // minimal in this case. Note, there's no doTkEvents
            // message - no control panel!
            displayActions = new ActionGroupImpl (getZone ());
            
            // Now schedule the update of the unhappyGraph, which will
            // in turn cause the file I/O to occur...
            try {
                displayActions.createActionTo$message 
                    (unhappyGraph, 
                     new Selector (unhappyGraph.getClass (), "step", true));
            } catch (Exception e) {
                System.err.println ("Exception batch unhappyGraph step: "
                                    + e.getMessage ());
            }                 

            // the displaySchedule controls how often we write data out.
            displaySchedule = 
                new ScheduleImpl (getZone (), loggingFrequency);
                 
            displaySchedule.at$createAction (0, displayActions);
        }
            
        // We also add in a "stopSchedule", another schedule
        // with an absolute time event - stop the system at
        // time .
        stopSchedule = new ScheduleImpl (getZone (), true);
            
        try {
            stopSchedule.at$createActionTo$message
                (experimentDuration, this, new Selector 
                    (getClass (), "stopRunning", false));
        } catch (Exception e) {
            System.err.println ("Exception stopRunning: "
                                + e.getMessage ());
                                } 
        return this;
    }  
        
    /** activateIn: - get the Swarm ready to run. */
    public Activity activateIn (Swarm swarmContext) {

        // First, activate ourselves (just pass along the context).
        super.activateIn (swarmContext);
        
        // We need to activate the model swarm.
        heatbugModelSwarm.activateIn (this);
                
        // Now activate our schedules in ourselves. Note that we just
        // activate both schedules: the activity library will merge
        // them properly.
        stopSchedule.activateIn (this);

        if (loggingFrequency > 0)
            displaySchedule.activateIn (this);
          
        // Activate returns the swarm activity - the thing that's
        // ready to run.
        return getActivity ();
    }

    /** the HeatbugObserverSwarm had a go method inherited from
        GUISwarm, but we have to define our own here. It's pretty
        simple. There's also a friendly message printed out here just
        in case someone is confused when they run heatbugs and see no
        graphics. */
    public Object go () {
        System.out.println ("You typed `heatbugs -b' or  `heatbugs --batch'"
                            + " so we're running without graphics.");
        
        System.out.println ("Heatbugs is running for "
                            + experimentDuration + " timesteps");
        
        if (loggingFrequency > 0)
            System.out.println ("It is logging data every " 
                                + loggingFrequency +
                                " timesteps to: unhappiness.output");
        
        (getActivity ().getSwarmActivity ()).run ();
        
        if (loggingFrequency > 0)
            // Close the output file.
            unhappyGraph.drop() ;              

        return getActivity ().getStatus ();
    }

    /** The termination method. When this fires we just terminate
        everything that's running and close our output file(s) by
        dropping the EZGraph which "owns" the sequence(s) we are
        logging. */
    public Object stopRunning () {
        // Terminate the simulation.
        System.out.println("quitting at " + Globals.env.getCurrentTime ());
        Globals.env.getCurrentSwarmActivity ().terminate (); 
        return this;
    }
}
