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

public class MousetrapModelSwarmCImpl extends SwarmCImpl
{
  public int gridSize;
  public double triggerLikelihood;
  public int numberOutputTriggers;
  public int maxTriggerDistance;
  public int maxTriggerTime;
  public double trapDensity;
  
  public ScheduleImpl modelSchedule;
  
  public MousetrapStatistics stats;
  public Grid2dImpl grid;
  
  public ActivityControlImpl modelActCont;

  public PMMLCG1genImpl randomGenerator;
  public UniformDoubleDistImpl uniform0to1;

  public void nag (String s)
  {
    System.out.println (this.getClass().getName() + ":" + s);
    System.out.flush ();
  }

  public MousetrapModelSwarmCImpl () 
  {
    super ();
    this.nextPhase = new MousetrapModelSwarmImpl ();
  }
  
  public MousetrapModelSwarmCImpl (MousetrapModelSwarmImpl swarm)
  {
    super (swarm);
  }
  
  public Object createBegin (Object aZone)
  {
    EmptyProbeMapCImpl iepm;
    EmptyProbeMapImpl fepm = new EmptyProbeMapImpl();

    super.createBegin (aZone);
    
    gridSize = 50;
    triggerLikelihood = 1.0;
    numberOutputTriggers = 2;
    maxTriggerDistance = 4;
    maxTriggerTime = 16;
    trapDensity = 1.0;
    
    nag ("Model: Custom probe map");
    
    //    CustomProbeMapImpl probeMap = new CustomProbeMapImpl
    //      ((ZoneImpl) aZone, nextPhase.getClass(), "gridSize", 
    //      "triggerLikelihood", "numberOutputTriggers", "maxTriggerDistance",
    //      "maxTriggerTime", "trapDensity", ":", null);
        
    iepm = new EmptyProbeMapCImpl (fepm);
    nag ("Model: Empty probe map create begin");
    iepm.createBegin (aZone);
    nag ("Model: probe nextphase");
    iepm.setProbedClass (nextPhase.getClass());
    nag ("Model: Create End");
    fepm = (EmptyProbeMapImpl) iepm.createEnd();
    nag ("add probe");
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("gridSize", nextPhase.getClass()));
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("triggerLikelihood", nextPhase.getClass()));
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("numberOutputTriggers", nextPhase.getClass()));
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("maxTriggerDistance", nextPhase.getClass()));
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("maxTriggerTime", nextPhase.getClass()));
    fepm.addProbe 
        (Globals.env.probeLibrary.getProbeForVariable$inClass 
         ("trapDensity", nextPhase.getClass()));

    Globals.env.probeLibrary.setProbeMap$For (fepm, nextPhase.getClass());
    return this;
    
  }
  public Object createEnd ()
  {
    PMMLCG1genCImpl irg;
    UniformDoubleDistCImpl iudd;
    super.createEnd ();

    randomGenerator = new PMMLCG1genImpl ();
    irg = new PMMLCG1genCImpl (randomGenerator);
    randomGenerator = 
      (PMMLCG1genImpl) irg.create$setStateFromSeed 
        (((MousetrapModelSwarmImpl) nextPhase).getZone(), 1234567890);
    uniform0to1 = new UniformDoubleDistImpl ();

    iudd = new UniformDoubleDistCImpl (uniform0to1);
    uniform0to1 = 
      (UniformDoubleDistImpl) 
        iudd.create$setGenerator$setDoubleMin$setMax 
        ( ((MousetrapModelSwarmImpl)nextPhase).getZone(), 
          randomGenerator, 0.0, 1.0 );


    ((MousetrapModelSwarmImpl) nextPhase).gridSize = gridSize;
    ((MousetrapModelSwarmImpl) nextPhase).triggerLikelihood 
      = triggerLikelihood;
    ((MousetrapModelSwarmImpl) nextPhase).numberOutputTriggers 
      = numberOutputTriggers;
    ((MousetrapModelSwarmImpl) nextPhase).maxTriggerDistance 
      = maxTriggerDistance;
    ((MousetrapModelSwarmImpl) nextPhase).maxTriggerTime = maxTriggerTime;
    ((MousetrapModelSwarmImpl) nextPhase).trapDensity = trapDensity;
    ((MousetrapModelSwarmImpl) nextPhase).modelSchedule = modelSchedule;
    ((MousetrapModelSwarmImpl) nextPhase).stats = stats;
    ((MousetrapModelSwarmImpl) nextPhase).grid = grid;
    ((MousetrapModelSwarmImpl) nextPhase).modelActCont  = modelActCont;
    ((MousetrapModelSwarmImpl) nextPhase).randomGenerator 
      = randomGenerator;
    ((MousetrapModelSwarmImpl) nextPhase).uniform0to1 = uniform0to1;
        
    return nextPhase;
  }

  public Object create (Object aZone)
  {
    createBegin (aZone);
    return createEnd ();
  }
}
