import swarm.Globals;
import swarm.objectbase.Swarm;
import swarm.activity.Activity;
import swarm.activity.Schedule;
import swarm.activity.ScheduleImpl;
import swarm.simtoolsgui.GUISwarmImpl;
import swarm.space.Object2dDisplay;
import swarm.space.Object2dDisplayImpl;
import swarm.gui.ZoomRasterImpl;
import swarm.gui.ZoomRaster;
import swarm.gui.Colormap;
import swarm.gui.ColormapImpl;
import swarm.defobj.Zone;
import swarm.Selector;

import java.util.List;
import java.util.LinkedList;

public class ObserverSwarm extends GUISwarmImpl {
  public final static byte UserTourColor = 0;
  public final static byte UserResistColor = 1;
  public final static byte UserListenColor = 2;
  public final static byte GlenTourColor = 3;
  public final static byte GlenAttackColor = 4;
  public final static byte MarcusIncubateColor = 5;
  public final static byte MarcusResistColor = 6;
  public final static byte MarcusListenColor = 7;
  public final static byte MarcusNativeColor = 8;
  public final static byte AlexTourColor = 9;
  public final static byte AlexTalkColor = 10;
  public final static byte AlexTargetColor = 11;
  Object2dDisplay display;
  ZoomRaster raster;
  SDG model;
  Colormap colormap;
  Schedule displaySchedule;

  public ObserverSwarm (Zone aZone) {
    super (aZone);
    model = new SDG (aZone);
  }

  public Object buildObjects () {
    super.buildObjects ();

    model.buildObjects ();
    
    colormap = new ColormapImpl (getZone ());
    colormap.setColor$ToName (UserTourColor, "white");
    colormap.setColor$ToName (UserResistColor, "gray");
    colormap.setColor$ToName (UserListenColor, "slate gray");

    colormap.setColor$ToName (MarcusIncubateColor, "yellow");
    colormap.setColor$ToName (MarcusResistColor, "red");
    colormap.setColor$ToName (MarcusListenColor, "pale green");
    colormap.setColor$ToName (MarcusNativeColor, "dark green");

    colormap.setColor$ToName (GlenTourColor, "cyan");
    colormap.setColor$ToName (GlenAttackColor, "pink");

    colormap.setColor$ToName (AlexTourColor, "brown");
    colormap.setColor$ToName (AlexTalkColor, "orange");
    colormap.setColor$ToName (AlexTargetColor, "brown");

    raster = new ZoomRasterImpl (getZone (), "raster");
    raster.setColormap (colormap);
    raster.setZoomFactor (3);
    raster.setWidth$Height (model.getWorld ().getSizeX (),
                            model.getWorld ().getSizeY ());
    raster.setWindowTitle ("SDG World");
    raster.pack ();
    raster.erase ();

    try {
      display = new Object2dDisplayImpl
        (getZone (),
         raster,
         model.getWorld (),
         new Selector (Class.forName ("agent2d.Agent2d"), "drawSelfOn", false));
      raster.setButton$Client$Message (3, display,
                                       new Selector (display.getClass (),
                                                     "makeProbeAtX$Y", true));
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }


    return this;
  }

  public void updateDisplay () {
    raster.erase ();
    display.display ();
    raster.drawSelf ();
    getActionCache ().doTkEvents ();
    Globals.env.probeDisplayManager.update ();
  }

  public Object buildActions () {
    super.buildActions ();

    model.buildActions ();
    displaySchedule = new ScheduleImpl (getZone (), 1);

    try {
      displaySchedule.at$createActionTo$message
        (0, 
         this,
         new Selector (getClass (), "updateDisplay", false));
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
    return this;
  }

  public Activity activateIn (Swarm context) {
    super.activateIn (context);
    
    model.activateIn (this);
    displaySchedule.activateIn (this);
    return getActivity ();
  }

  public static void main (String[] args) {
    Globals.env.initSwarm ("SDG", "0.0", "bug-swarm@swarm.org", args);
    ObserverSwarm observer = new ObserverSwarm (Globals.env.globalZone);
    observer.buildObjects ();
    observer.buildActions ();
    observer.activateIn (null);
    observer.go ();
  }
}
