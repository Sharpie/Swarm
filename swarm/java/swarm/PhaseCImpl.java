package swarm;
import java.lang.reflect.*;
public class PhaseCImpl {
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
       for (i=0; i<creating_phase_fields.length; i++)
	 {
	   System.out.println (creating_phase_fields[i].getName());
	   using_phase_field = 
	     nextPhaseClass.getDeclaredField(creating_phase_fields[i].getName());
	   
	   if (using_phase_field != null)
	     using_phase_field.set(nextPhase, 
				   creating_phase_fields[i].get(this));
	 }
     } catch (Exception e) 
       { 
	 System.out.println ("\nWhile copying field: " +
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
