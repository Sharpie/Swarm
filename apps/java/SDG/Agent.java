import swarm.objectbase.SwarmImpl;
import swarm.defobj.Zone;

import javax.media.j3d.TransformGroup;
import javax.media.j3d.Font3D;
import javax.media.j3d.FontExtrusion;
import java.awt.Font;
import javax.media.j3d.Text3D;
import javax.media.j3d.Shape3D;
import javax.media.j3d.Transform3D;
import javax.vecmath.Vector3d;

public class Agent extends SwarmImpl {
  TransformGroup trans;

  void createText (String textString) {
    Font3D f3d = new Font3D (new Font ("TestFont", Font.PLAIN, 2),
                             new FontExtrusion ());
    Text3D txt = new Text3D (f3d, textString);
    Shape3D sh = new Shape3D ();
    sh.setGeometry (txt);
    
    trans = new TransformGroup ();
    Transform3D t3d = new Transform3D ();
    
    t3d.setScale (0.1);
    trans.setCapability (TransformGroup.ALLOW_TRANSFORM_WRITE); 
    trans.setTransform (t3d);
    trans.addChild (sh);
  }
  
  Agent (Zone aZone, String textString) {
    super (aZone);

    createText (textString);
  }

  TransformGroup getTransformGroup () {
    return trans;
  }

  void moveAgent (double xoffset, double yoffset, double zoffset) {
    Vector3d vec = new Vector3d ();
    Vector3d newVec = new Vector3d (xoffset, yoffset, zoffset);
    Transform3D t3d = new Transform3D ();
    trans.getTransform (t3d);
    t3d.get (vec);
    vec.add (newVec);
    t3d.setTranslation (vec);
    trans.setTransform (t3d);
  }

}
