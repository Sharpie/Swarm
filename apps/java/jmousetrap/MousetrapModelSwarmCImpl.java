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
   public SwarmEnvironment se;
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
      gridSize = 50;
      //      nag ((new Integer (this.gridSize)).toString());
      super.createBegin (aZone);
      this.gridSize = 50;
      //      nag ((new Integer (this.gridSize)).toString());
      triggerLikelihood = 1.0;
      numberOutputTriggers = 2;
      maxTriggerDistance = 4;
      maxTriggerTime = 16;
      trapDensity = 1.0;
           nag ("Model: Empty probe map");
      iepm = new EmptyProbeMapCImpl (fepm);
           nag ("Model: Empty probe map create begin");
      iepm.createBegin (aZone);
            nag ("Model: probe nextphase");
      iepm.setProbedClass (nextPhase.getClass());
           nag ("Model: Create End");
      fepm = (EmptyProbeMapImpl) iepm.createEnd();
           nag ("add probe");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("gridSize", nextPhase.getClass()));
      //      nag ("probe trigger likelihod");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("triggerLikelihood", nextPhase.getClass()));
      //      nag ("probe numberoutpur");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("numberOutputTriggers", nextPhase.getClass()));
      //      nag ("probe max trigger");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("maxTriggerDistance", nextPhase.getClass()));
      //      nag ("probe ma triffer");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("maxTriggerTime", nextPhase.getClass()));
      //      nag ("probe mac trigger");
      fepm.addProbe (se.probeLibrary.getProbeForVariable$inClass ("trapDensity", nextPhase.getClass()));
      se.probeLibrary.setProbeMap$For (fepm, nextPhase.getClass());
      //      nag ("done with begin");
      return this;
    
  }
  public Object createEnd ()
  {
    PMMLCG1genCImpl irg;
    UniformDoubleDistCImpl iudd;
    super.createEnd ();
    //!!!!
    //    nag ((new Integer (gridSize)).toString());
    randomGenerator = new PMMLCG1genImpl ();
    irg = new PMMLCG1genCImpl (randomGenerator);
    randomGenerator = 
      (PMMLCG1genImpl) irg.create$setStateFromSeed (((MousetrapModelSwarmImpl) nextPhase).getZone(), 1234567890);
    uniform0to1 = new UniformDoubleDistImpl ();
    //!!!
    iudd = new UniformDoubleDistCImpl (uniform0to1);
    uniform0to1 = 
      (UniformDoubleDistImpl) 
      iudd.create$setGenerator$setDoubleMin$setMax ( ((MousetrapModelSwarmImpl)nextPhase).getZone(), 
						     randomGenerator, 
						     0.0, 1.0);
    //    nag ("createEnd grid size");
    ((MousetrapModelSwarmImpl) nextPhase).gridSize = gridSize;
    //    nag ((new Integer (gridSize)).toString());
    ((MousetrapModelSwarmImpl) nextPhase).triggerLikelihood = triggerLikelihood;
    ((MousetrapModelSwarmImpl) nextPhase).numberOutputTriggers = numberOutputTriggers;
    ((MousetrapModelSwarmImpl) nextPhase).maxTriggerDistance = maxTriggerDistance;
    ((MousetrapModelSwarmImpl) nextPhase).maxTriggerTime = maxTriggerTime;
    ((MousetrapModelSwarmImpl) nextPhase).trapDensity = trapDensity;
    ((MousetrapModelSwarmImpl) nextPhase).modelSchedule = modelSchedule;
    ((MousetrapModelSwarmImpl) nextPhase).stats = stats;
    ((MousetrapModelSwarmImpl) nextPhase).grid = grid;
    ((MousetrapModelSwarmImpl) nextPhase).modelActCont  = modelActCont;
    ((MousetrapModelSwarmImpl) nextPhase).randomGenerator = randomGenerator;
    ((MousetrapModelSwarmImpl) nextPhase).uniform0to1 = uniform0to1;

    return nextPhase;
  }

  public Object create (Object aZone)
  {
    createBegin (aZone);
    return createEnd ();
  }
}
