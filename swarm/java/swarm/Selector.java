package swarm;

class Selector {
  public Selector (String nameVal, Class retTypeVal, Class[] argTypeVals) {
    name = nameVal;
    retTypes = retTypeVal;
    argTypes = argTypeVals;
  }
  String name;
  Class retType;
  Class [] argTypes;
}
