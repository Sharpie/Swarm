import java.applet.Applet;
import com.sun.j3d.utils.universe.SimpleUniverse;
import com.sun.j3d.utils.behaviors.mouse.MouseRotate;
import com.sun.j3d.utils.behaviors.mouse.MouseTranslate;
import com.sun.j3d.utils.behaviors.mouse.MouseZoom;
import javax.media.j3d.Background;
import javax.media.j3d.BoundingSphere;
import javax.media.j3d.BranchGroup;
import javax.media.j3d.Canvas3D;
import javax.media.j3d.TransformGroup;
import javax.vecmath.Color3f;
import javax.vecmath.Point3d;
import javax.vecmath.Point3f;
import java.awt.BorderLayout;
import java.awt.GraphicsConfiguration;

public class Organization extends Applet {
  TransformGroup rootTrans = new TransformGroup ();

  public BranchGroup createSceneGraph () {
    BranchGroup root = new BranchGroup ();
    BoundingSphere bounds =
      new BoundingSphere (new Point3d (0.0, 0.0, 0.0), 100.0);

    rootTrans.setCapability (TransformGroup.ALLOW_TRANSFORM_WRITE);
    rootTrans.setCapability (TransformGroup.ALLOW_TRANSFORM_READ);
    
    MouseRotate behavior = new MouseRotate ();
    behavior.setTransformGroup (rootTrans);
    rootTrans.addChild (behavior);
    behavior.setSchedulingBounds (bounds);
    
    MouseZoom behavior2 = new MouseZoom ();
    behavior2.setTransformGroup (rootTrans);
    rootTrans.addChild (behavior2);
    behavior2.setSchedulingBounds (bounds);
    
    // Create the translate behavior node
    MouseTranslate behavior3 = new MouseTranslate();
    behavior3.setTransformGroup(rootTrans);
    rootTrans.addChild (behavior3);
    behavior3.setSchedulingBounds (bounds);

    // Set up the background
    Color3f bgColor = new Color3f (0.05f, 0.05f, 0.5f);
    Background bgNode = new Background (bgColor);
    bgNode.setApplicationBounds (bounds);
    rootTrans.addChild (bgNode);

    root.addChild (rootTrans);

    return root;
  }

  public void addAgent (Agent agent) {
    rootTrans.addChild (agent.getTransformGroup ());
  }

  public Organization (String[] args) {
  }

  public void init () {
    setLayout (new BorderLayout ());
    GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration ();
    Canvas3D c = new Canvas3D (config);
    add ("Center", c);

    BranchGroup scene = createSceneGraph ();
    SimpleUniverse u = new SimpleUniverse (c);
    u.getViewingPlatform ().setNominalViewingTransform ();
    u.addBranchGraph (scene);
  }
}
