import swarm.Globals;
import swarm.defobj.LispArchiverImpl;
import swarm.defobj.Archiver;
import swarm.objectbase.SwarmObjectImpl;

public class TestJavaLispArchiver extends SwarmObjectImpl {
  public int a;
  public double b;
  public String str;
  
  public TestJavaLispArchiver () {
    super ();
    a = 100;
    b = 2.0;
    str = "Hello";
  }
  
  void save (Archiver archiver, int modelNumber) {
    String key = "model" + modelNumber;

    archiver.putShallow$object (key, this);
    archiver.sync ();
  }

  static void main (String args[]) {
    Globals.env.initSwarm ("TestJavaLispArchiver", "0.0",
                           "bug-swarm@swarm.org", args);
      

    TestJavaLispArchiver test = new TestJavaLispArchiver ();
    test.save (new LispArchiverImpl (Globals.env.globalZone, "ta.scm"),
               3);
  }
}
