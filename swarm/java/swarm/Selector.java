package swarm;
import swarm.NonUniqueMethodSignatureException;
import swarm.SignatureNotFoundException;
import java.lang.reflect.Method;

public class Selector {
  String signature;
  Class retType;
  Class [] argTypes;
  boolean objcFlag;
  
  public Selector (Class theClass, String theMethodName, boolean theObjcFlag) throws NonUniqueMethodSignatureException, SignatureNotFoundException {
    
    super ();
    
    {
      Method[] methods = theClass.getMethods ();
      int matchCount = 0;

      objcFlag = theObjcFlag;
      if (objcFlag)
        theMethodName.replace (':', '$');
      
      for (int mi = 0; mi < methods.length; mi++)
        {
          if (methods[mi].getName ().compareTo (theMethodName) == 0)
            {
              if (matchCount > 0)
                throw new NonUniqueMethodSignatureException ();
              signature = theMethodName;
              retType = methods[mi].getReturnType ();
              argTypes = methods[mi].getParameterTypes ();
              matchCount++;
            }
        }
      if (matchCount != 1)
        throw new SignatureNotFoundException ();
    }
  }
  public int hashCode () {
    return signature.hashCode ();
  }
  public boolean equals (Object obj) {
    if (obj.getClass () == this.getClass ())
      return ((Selector) obj).signature.equals (signature);
    else
      return false;
  }
}
