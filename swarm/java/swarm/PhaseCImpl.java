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

import java.lang.reflect.*;

public class PhaseCImpl extends BaseImpl {
  public Object nextPhase;
  public void _copy_creating_phase_to_using_phase ()
  {
     Field[] creating_phase_fields;
     Field using_phase_field;
     Class nextPhaseClass;
     int i = 0;
     nextPhaseClass = nextPhase.getClass ();
     creating_phase_fields = this.getClass().getDeclaredFields();
     
     try {
       for (i = 0; i < creating_phase_fields.length; i++)
	 {
	   using_phase_field = 
	     nextPhaseClass.getDeclaredField(creating_phase_fields[i].getName());
	   
	   if (using_phase_field != null)
	     using_phase_field.set(nextPhase, 
				   creating_phase_fields[i].get(this));
	 }
     } catch (Exception e) 
       { 
	 System.err.println ("\nWhile copying field: " +
			     creating_phase_fields[i].getName() +
			     "\nfrom an instance of class: " + 
			     this.getClass().getName() +
			     "\nto an instance of class: " + 
			     nextPhase.getClass().getName() +
			     "\ngot exception: " + e.toString () + "\n");
       }
  }

  public PhaseCImpl () {
    super ();
  }
  public PhaseCImpl (Object nextPhase) {
    super ();
    this.nextPhase = nextPhase;
  }
}
