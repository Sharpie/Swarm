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
public class MousetrapObserverSwarmCImpl extends GUISwarmCImpl
{
   public SwarmEnvironment se;
   public int displayFrequency;
   public ScheduleImpl displaySchedule;
   public MousetrapModelSwarmImpl mousetrapModelSwarm;
  
   public ColormapImpl colormap;
   public ZoomRasterImpl displayWindow;
   public EZGraphImpl triggerGraph;

   public Object2dDisplayImpl mousetrapDisplay;

  public  ActivityControlImpl observerActCont;

   public void nag (String s)
  {
    System.out.println (this.getClass().getName() + ":" + s);
     System.out.flush ();
  }
  public MousetrapObserverSwarmCImpl ()
  {
    super ();
    // nextPhase = new MousetrapObserverSwarmImpl ();
  }

  public MousetrapObserverSwarmCImpl (MousetrapObserverSwarmImpl swarm)
  {
    super (swarm);
  }


  public Object createBegin (Object aZone)
  {
    EmptyProbeMapCImpl iprobeMap;
    EmptyProbeMapImpl probeMap;

    //    nag ("createBegin");
    super.createBegin (aZone);
    displayFrequency = 1;
    nag("Observer: probeMap");
    probeMap = new EmptyProbeMapImpl ();
    nag("Observer: iProbeMap");
    iprobeMap = new EmptyProbeMapCImpl (probeMap);

    
    nag("Observer: iProbeMap createBegin");
    iprobeMap.createBegin (aZone);
    nag("Observer: iProbeMap setProbedClass");
    iprobeMap.setProbedClass (nextPhase.getClass());
    nag("Observer: iProbeMap createEnd");
    // probeMap = (EmptyProbeMapImpl) iprobeMap.createEnd();
    iprobeMap.createEnd();
    
    nag("Observer: probeMap addProbe\n");
    probeMap.addProbe (se.probeLibrary.getProbeForVariable$inClass("displayFrequency", nextPhase.getClass()));

    nag("Observer: probeLibrary.setProbeMap$For");
    
    //    nag("setProbeMapFor");
    se.probeLibrary.setProbeMap$For (probeMap, nextPhase.getClass());
    //    nag ("done!!!!!!!");
    return this;
  }

  public Object createEnd ()
  {
    //    nag ("super createEnd");
    super.createEnd();
    //    nag ("super assigment");
    ((MousetrapObserverSwarmImpl)nextPhase).displayFrequency = displayFrequency;
    ((MousetrapObserverSwarmImpl)nextPhase).displaySchedule =  displaySchedule;

    ((MousetrapObserverSwarmImpl)nextPhase).mousetrapModelSwarm = mousetrapModelSwarm;
    ((MousetrapObserverSwarmImpl)nextPhase).colormap = colormap;
    ((MousetrapObserverSwarmImpl)nextPhase).displayWindow = displayWindow;
    ((MousetrapObserverSwarmImpl)nextPhase).triggerGraph = triggerGraph;
    ((MousetrapObserverSwarmImpl)nextPhase).mousetrapDisplay = mousetrapDisplay;
    ((MousetrapObserverSwarmImpl)nextPhase).observerActCont = observerActCont;
    //    nag ("MousetrapObserverSwarmCImpldone createEnd");
    return nextPhase;
  }
  
  public Object create (Object aZone)
  {
    createBegin (aZone);
    return createEnd ();
  }
}
