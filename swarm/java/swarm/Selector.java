// Swarm library. Copyright © 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

package swarm;
import swarm.NonUniqueMethodSignatureException;
import swarm.SignatureNotFoundException;
import java.lang.reflect.Method;

public class Selector {
  String signature;
  Class retType;
  Class [] argTypes;
  boolean objcFlag;
  String typeSignature;
  
  public Selector (Class theClass, String methodName, boolean theObjcFlag) throws NonUniqueMethodSignatureException, SignatureNotFoundException {
    
    super ();
    
    {
      Method[] methods = theClass.getMethods ();
      int matchCount = 0;

      objcFlag = theObjcFlag;
      if (objcFlag)
        {
          methodName = methodName.replace (':', '$');
          if (methodName.endsWith ("$"))
            methodName = methodName.substring (0, methodName.length () - 1);
        }
      
      for (int mi = 0; mi < methods.length; mi++)
        {
          if (methods[mi].getName ().compareTo (methodName) == 0)
            {
              if (matchCount > 0) {
                int i;
                Class []margTypes = methods[mi].getParameterTypes ();
                
                if (margTypes.length != argTypes.length)
                  throw new NonUniqueMethodSignatureException ();
                
                for (i = 0; i < argTypes.length; i++)
                  if (argTypes[i] != margTypes[i]) {
                    System.err.println (signature + " arg: " + i + ": " + argTypes[i] + " != " + margTypes[i]);
                    throw new NonUniqueMethodSignatureException ();
                  }
                if (retType != methods[mi].getReturnType ())
                  System.err.println (signature + " retType: "+ retType + " != " + methods[mi].getReturnType ());
              }
              signature = methodName;
              retType = methods[mi].getReturnType ();
              argTypes = methods[mi].getParameterTypes ();
              matchCount++;
            }
        }
      if (matchCount == 0)
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
