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

public class StartSDG extends GUISwarmImpl {
  Object2dDisplay display;
  ZoomRaster raster;
  SDG model;
  Colormap colormap;
  Schedule displaySchedule;

  public StartSDG (Zone aZone) {
    super (aZone);
    model = new SDG (aZone);
  }

  public Object buildObjects () {
    super.buildObjects ();

    model.buildObjects ();
    
    colormap = new ColormapImpl (getZone ());
    colormap.setColor$ToName ((byte) 0, "red");
    colormap.setColor$ToName ((byte) 1, "green");

    raster = new ZoomRasterImpl (getZone (), "raster");
    raster.setColormap (colormap);
    raster.setZoomFactor (2);
    raster.setWidth$Height (model.getWorld ().getSizeX (),
                            model.getWorld ().getSizeY ());
    raster.setWindowTitle ("SDG World");
    raster.pack ();

    try {
      display = new Object2dDisplayImpl
        (getZone (),
         raster,
         model.getWorld (),
         new Selector (Class.forName ("agent2d.Agent2d"), "drawSelfOn", false));
    } catch (Exception e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
    return this;
  }

  public void updateDisplay () {
    display.display ();
    raster.drawSelf ();
    getActionCache ().doTkEvents ();
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
    StartSDG observer = new StartSDG (Globals.env.globalZone);
    observer.buildObjects ();
    observer.buildActions ();
    observer.activateIn (null);
    observer.go ();
  }
}
