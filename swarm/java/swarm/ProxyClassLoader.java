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
import java.io.IOException;
import gnu.bytecode.ClassType;
import gnu.bytecode.Access;
import gnu.bytecode.Type;
import gnu.bytecode.Method;


public class ProxyClassLoader extends ClassLoader {
  public Class findClass (String name) {

    try {
      ClassType clas = new ClassType (name);
      ClassType superClass = ClassType.make ("swarm.ObjCProxy");
      clas.setSuper (superClass);
      Method constructor = clas.addMethod ("<init>", Type.typeArray0,
                                           Type.void_type, Access.PUBLIC);
      Method superConstructor =
        superClass.addMethod ("<init>", Type.typeArray0,
                              Type.void_type, Access.PUBLIC);
      
      constructor.init_param_slots ();
      gnu.bytecode.CodeAttr code = constructor.getCode();
      code.emitPushThis();
      code.emitInvokeSpecial (superConstructor);
      code.emitReturn();
      

      byte[] b = clas.writeToArray ();
      return defineClass (name, b, 0, b.length);
    } catch (IOException e) {
      e.printStackTrace (System.err);
      System.exit (1);
    }
    return null;
  }
}
