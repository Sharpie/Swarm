// Swarm library. Copyright (C) 1999-2000 Swarm Development Group.
// This library is distributed without any warranty; without even the
// implied warranty of merchantability or fitness for a particular purpose.
// See file LICENSE for details and terms of copying.

package swarm;

import java.lang.reflect.Method;

public class Primitives {
  static public char returnCharacter () { return 'A'; }
  static public byte returnByte () { return 0; }
  static public int returnInteger () { return 0; }
  static public short returnShort () { return 0; }
  static public long returnLong () { return 0; }
  static public float returnFloat () { return 0; }
  static public double returnDouble () { return 0; }
  static public void returnVoid () { return; }
  static public boolean returnBoolean () { return false; }
  static public Method getTypeMethod (String typeName) {
    try {
      return new Primitives ().getClass ().getMethod ("return" + typeName,
                                                      null);
    }
    catch (NoSuchMethodException exception) {
      System.err.println ("No method `return" + typeName + "'");
      return null;
    }
  }
}
