// Swarm library. Copyright (C) 1999-2000 Swarm Development Group.
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
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
