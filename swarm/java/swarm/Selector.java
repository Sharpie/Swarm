// Swarm library. Copyright (C) 1999-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
// USA
// 
// The Swarm Development Group can be reached via our website at:
// http://www.swarm.org/

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
                  
                  // Skip over same-named, but different-arity methods
                  // This comes up in ProbeMaps for base classes, e.g. wait
                  if (margTypes.length == argTypes.length)
                      {
                          for (i = 0; i < argTypes.length; i++)
                            if (argTypes[i] != margTypes[i]) {
                                System.err.println (signature + " arg: " + i + ": " + argTypes[i] + " != " + margTypes[i]);
                                throw new NonUniqueMethodSignatureException ();
                            }
                        if (retType != methods[mi].getReturnType ())
                            System.err.println (signature + " retType: "+ retType + " != " + methods[mi].getReturnType ());
                      }
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

  public String getSignature () {
    return signature;
  }
}
