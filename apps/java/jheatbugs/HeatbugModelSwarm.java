// Java Heatbugs application. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

import swarm.Globals;
import swarm.Selector;
import swarm.defobj.Zone;
import swarm.defobj.SymbolImpl;

import swarm.defobj.FArguments;
import swarm.defobj.FArgumentsImpl;
import swarm.defobj.FCall;
import swarm.defobj.FCallImpl;

import swarm.activity.Activity;
import swarm.activity.ActionGroup;
import swarm.activity.ActionGroupImpl;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.activity.FActionForEach;

import swarm.objectbase.Swarm;
import swarm.objectbase.SwarmImpl;
import swarm.objectbase.VarProbe;
import swarm.objectbase.MessageProbe;
import swarm.objectbase.EmptyProbeMapImpl;

import java.util.LinkedList;
import java.util.List;

import swarm.space.Grid2d;
import swarm.space.Grid2dImpl;

/** 
 * The HeatbugModelSwarm encapsulates all the objects used in the
 * simulated heatbug world itself (but not the user interface objects)
 * */
public class HeatbugModelSwarm extends SwarmImpl
{
  // simulation parameters
  public int numBugs;				
  public double evaporationRate;
  public double diffuseConstant;
  public int worldXSize, worldYSize;
  public int minIdealTemp, maxIdealTemp;
  public int minOutputHeat, maxOutputHeat;
  public double randomMoveProbability;
    
  public boolean randomizeHeatbugUpdateOrder;
 
  /** ActionGroup for holding an ordered sequence of action */
  public ActionGroup modelActions;			
  /** the single Schedule */
  public Schedule modelSchedule;
    
  /** list of all the heatbugs */
  public List heatbugList;			
  
  /**  the 2d world  */
  public Grid2d world;				
  /** the 2d heat space */
  public HeatSpace heat;				  

  FActionForEach actionForEach;

  // These methods provide access to the objects inside the
  // ModelSwarm.  These objects are the ones visible to other
  // classes via message call.  In theory we could just let other
  // objects use Probes to read our state, but message access is
  // frequently more convenient.

  public List getHeatbugList () {
    return heatbugList;
  }
  
  public Grid2d getWorld () {
    return world;
  }
    
  public HeatSpace getHeat () {
    return heat;
  }
  
  public boolean toggleRandomizedOrder () {
    randomizeHeatbugUpdateOrder = !randomizeHeatbugUpdateOrder;
    syncUpdateOrder ();
    return randomizeHeatbugUpdateOrder;
  }

  public void syncUpdateOrder () {
    if (actionForEach != null)
      actionForEach.setDefaultOrder
        (randomizeHeatbugUpdateOrder
         ? Globals.env.Randomized
         : Globals.env.Sequential);
  }
  
  /** 
     * This method isn't normally used, but is convenient when running
     * probes: it lets you easily clone a heatbug and drag it into the
     * model. */
  public Object addHeatbug (Heatbug bug) {
    heatbugList.add (bug);
    return this;
  }

  public HeatbugModelSwarm (Zone aZone) {
    super (aZone);
        
    // Now fill in various simulation parameters with default values.
    numBugs = 100;
    evaporationRate = 0.99;
    diffuseConstant = 1.0;
    worldXSize = 80;
    worldYSize = 80;
    minIdealTemp = 17000;
    maxIdealTemp = 31000;
    minOutputHeat = 3000;
    maxOutputHeat = 10000;
    randomizeHeatbugUpdateOrder = false;
    randomMoveProbability = 0.0;

    // Now, build a customized probe map using a `local' subclass (a
    // special kind of Java `inner class') of the EmptyProbeMapImpl
    // class. Without a probe map, the default is to show all
    // variables and messages. Here we choose to customize the
    // appearance of the probe, to display a nicer interface.

    class HeatbugModelProbeMap extends EmptyProbeMapImpl {
      private VarProbe probeVariable (String name) {
        return
          Globals.env.probeLibrary.getProbeForVariable$inClass
          (name, HeatbugModelSwarm.this.getClass ());
      }
      private MessageProbe probeMessage (String name) {
        return
          Globals.env.probeLibrary.getProbeForMessage$inClass
          (name, HeatbugModelSwarm.this.getClass ());
      }
      private void addVar (String name) {
        addProbe (probeVariable (name));
      }
      private void addMessage (String name) {
        addProbe (probeMessage (name));
      }
      public HeatbugModelProbeMap (Zone _aZone, Class aClass) {
        super (_aZone, aClass);
        addVar ("numBugs");
        addVar ("diffuseConstant");
        addVar ("worldXSize");
        addVar ("worldYSize");
        addVar ("minIdealTemp");
        addVar ("maxIdealTemp");
        addVar ("minOutputHeat");
        addVar ("maxOutputHeat");
        addVar ("evaporationRate");
        addVar ("randomMoveProbability");
        addMessage ("toggleRandomizedOrder");
        addMessage ("addHeatbug:");
      }
    }
    
    // Now, install our custom probeMap class directly into the
    // probeLibrary
    Globals.env.probeLibrary.setProbeMap$For
      (new HeatbugModelProbeMap (aZone, getClass ()), getClass ());
  }
    
  /** 
     * Now it's time to build the model objects. We use various
     * parameters inside ourselves to choose how to create things.
     */
  public Object buildObjects ()
  {
    int i;
            
    // allow our parent class to build anything.
    super.buildObjects();
  
    // First, set up objects used to represent the
    // environment.  The heatspace agent represents the
    // spatial property of heat.  It is initialized via
    // various model parameters.

    heat = new HeatSpace (getZone (), worldXSize, worldYSize, 
                          diffuseConstant, evaporationRate);
  
    // Now set up the grid used to represent agent position
            
    world = new Grid2dImpl (getZone (), worldXSize, worldYSize);

    // Create a list to keep track of the heatbugs in the model.

    heatbugList = new LinkedList ();
            
    // Create heatbugs themselves. This is a fairly complex
    // step, as is appropriate: the heatbugs are essential
    // aspects of the simulation.

    // First, a quick hack. During creation we might put
    // several heatbugs in the same square. This is a design
    // flaw, but it's one that's not fatal, so we ask the
    // world object not to warn us about it. This is not an
    // example to be emulated :-)

    world.setOverwriteWarnings (false);
            
    // Now a loop to create a bunch of heatbugs.
            
    for (i = 0; i < numBugs; i++) {
      Heatbug hbug;
      int idealTemp, outputHeat;
                
      // Choose a random ideal temperature, output heat
      // from the specified range (model parameters).
      idealTemp = 
        Globals.env.uniformIntRand.
        getIntegerWithMin$withMax (minIdealTemp, maxIdealTemp);
      outputHeat = 
        Globals.env.uniformIntRand.
        getIntegerWithMin$withMax (minOutputHeat, maxOutputHeat);
            
            
      // Create the heatbug, with a standard Java constructor
      hbug =  new Heatbug (world, heat);
            
      // Add the bug to the end of the list.
      heatbugList.add (hbug);
      
      // Now initialize the rest of the heatbug's state.
                
      hbug.setIdealTemperature (idealTemp);
      hbug.setOutputHeat (outputHeat);
      hbug.setRandomMoveProbability (randomMoveProbability);
      
      // random position
      hbug.setX$Y ((Globals.env.uniformIntRand.
                    getIntegerWithMin$withMax (0, (worldXSize-1))),
                   Globals.env.uniformIntRand.
                   getIntegerWithMin$withMax (0, (worldYSize-1)));
    }
    world.setOverwriteWarnings (true);	   // ok, done cheating.
  
    return this;
  }

  /**
     * Here is where the model schedule is built, the data structures
     * that define the simulation of time in the mode. The core is an
     * actionGroup that has a list of actions. That's then put in a
     * Schedule.  */
  public Object buildActions () {
    super.buildActions();
    
    // Create the list of simulation actions. We put these in
    // an action group, because we want these actions to be
    // executed in a specific order, but these steps should
    // take no (simulated) time. The M(foo) means "The message
    // called <foo>". You can send a message To a particular
    // object, or ForEach object in a collection.
        
    // Note we update the heatspace in two phases: first run
    // diffusion, then run "updateWorld" to actually enact the
    // changes the heatbugs have made. The ordering here is
    // significant!
        
    // Note also, that with the additional
    // `randomizeHeatbugUpdateOrder' Boolean flag we can
    // randomize the order in which the bugs actually run
    // their step rule.  This has the effect of removing any
    // systematic bias in the iteration throught the heatbug
    // list from timestep to timestep
        
    // By default, all `createActionForEach' modelActions have
    // a default order of `Sequential', which means that the
    // order of iteration through the `heatbugList' will be
    // identical (assuming the list order is not changed
    // indirectly by some other process).
    
    modelActions = new ActionGroupImpl (getZone ());

    try {
      modelActions.createActionTo$message
        (heat, new Selector (heat.getClass (), "stepRule", false));
    } catch (Exception e) {
      System.err.println ("Exception stepRule: " + e.getMessage ());
    }

    try {
      Heatbug proto = (Heatbug) heatbugList.get (0);
      Selector sel = 
        new Selector (proto.getClass (), "heatbugStep", false);
      actionForEach =
        modelActions.createFActionForEachHomogeneous$call
        (heatbugList,
         new FCallImpl (this, proto, sel,
                        new FArgumentsImpl (this, sel)));
    } catch (Exception e) {
      e.printStackTrace (System.err);
    }
    
    syncUpdateOrder ();

    try {
      modelActions.createActionTo$message 
        (heat, new Selector (heat.getClass (), "updateLattice", false));
    } catch (Exception e) {
      System.err.println("Exception updateLattice: " + e.getMessage ());
    }
        
    // Then we create a schedule that executes the
    // modelActions. modelActions is an ActionGroup, by itself it
    // has no notion of time. In order to have it executed in
    // time, we create a Schedule that says to use the
    // modelActions ActionGroup at particular times.  This
    // schedule has a repeat interval of 1, it will loop every
    // time step.  The action is executed at time 0 relative to
    // the beginning of the loop.

    // This is a simple schedule, with only one action that is
    // just repeated every time. See jmousetrap for more
    // complicated schedules.
  
    modelSchedule = new ScheduleImpl (getZone (), 1);
    modelSchedule.at$createAction (0, modelActions);
        
    return this;
  }

  /**
     * Now set up the model's activation. swarmContext indicates where
     * we're being started in - typically, this model is run as a
     * subswarm of an observer swarm. */
  public Activity activateIn (Swarm swarmContext) {
    // First, activate ourselves via the superclass
    // activateIn: method.  Just pass along the context: the
    // activity library does the right thing.
    super.activateIn (swarmContext);
    
    // Now activate our own schedule.
    modelSchedule.activateIn (this);
    
    // Finally, return our activity.
    return getActivity ();
  }
}
