import swarm.Globals;
import swarm.Selector;

public class SwarmUtils
{
    // These two static methods create a selector.  A selector is an
    // object of the Selector class used by Swarm to encapsulate a
    // "message" destined for an object, where the message is the name
    // of a method defined for the class to which the object belongs.
    // Because the method must indeed be defined for the class of the
    // object and because this can be determined only at run time,
    // there is a possibility that the creation of the selector will
    // throw an exception if the class and the method do not match.
    // Java requires that events that might throw exceptions be
    // enclosed in try/catch blocks.  If there is an error creating
    // the new selector in the try block, the catch block can handle
    // the resulting exception.  Here we have taken a pretty crude
    // approach to handling the exception: we simply call
    // System.exit(1).  (Note that the "return null" which ends the
    // catch block is there only to tell the compiler that "return
    // sel" will never be reached if an exception occurs and sel is
    // undefined.  We'll exit on an exception before ever returning
    // sel to the calling method.)

    // Note that the getSelector method overloaded.  It can be
    // called with either a string containing the class name as its
    // first argument, or with an object of the desired class.  In the
    // first case, the string is converted to a class identifier using
    // the forName() method, while in the second case getClass() is
    // used to find the class identifier for the object.  The second
    // argument to getSelector is always a string containing the
    // method name.  (The boolean "false" at the end of the Selector
    // constructor is theobjCFlag. It allows one to use
    // ObjectiveC-type key/value method syntax.  Since we always use
    // Java-style method names, for us the flag is always false.)

    public static Selector getSelector(String name, String method)
    {
	Selector sel;

	try
	    {
	    sel = new Selector(Class.forName(name), method, false);
	    } catch (Exception e)
		{
		System.err.println("There was an error in creating a Selector for method "
				   + method + "\nin Class " + name + ".");
		System.err.println (name + "." + method + " returns " + e.getMessage());
		System.err.println("The process will be terminated.");
		System.exit(1);
		return null;
		}

	return sel;
    }

    public static Selector getSelector(Object obj, String method)
    {
	Selector sel;

	try
	    {
	    sel = new Selector(obj.getClass(), method, false);
	    } catch (Exception e)
		{
		System.err.println("There was an error in creating a Selector for method "
				   + method + "\nin Class " + 
				   (obj.getClass()).getName() + ".");
		System.err.println ((obj.getClass()).getName() + "." + method 
				    + " returns " + e.getMessage());
		System.err.println("The process will be terminated.");
		System.exit(1);
		return null;
		}

	return sel;
    }

}
