package swarm;

public class Selector {
  public Selector (String nameVal, Class retTypeVal, Class[] argTypeVals) {
    name = nameVal;
    retType = retTypeVal;
    argTypes = argTypeVals;
  }
  String name;
  Class retType;
  Class [] argTypes;
}
