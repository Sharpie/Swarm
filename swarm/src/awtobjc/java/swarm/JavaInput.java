package swarm;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

// "input" class.  one instance of this object is maintained per session.
// as input events are made, they are queued inside this object.  the Objective
// C half references the same object, extracting and acting on those 
// input events.

// We pass events through this queue in order to postphone the eventual
// need to handle multi-threading.

class JavaInput
{
  // maintain one queue for all input events
  static public JavaInput queue = new JavaInput();

  static JavaInput getInputQueue() {
    if (queue == null) {
      queue = new JavaInput();
    }
    return queue;
  }

  private Vector _evts;			// queue of input events

  public JavaInput() {
System.out.println("JavaInput... new");
    _evts = new Vector();
  }

  // add a new plot of a dataset under the given name
  public synchronized void putEvent(AWTEvent evt) {
    _evts.addElement(evt);
  }

  // remove and return the first element in this queue
  public synchronized AWTEvent getEvent() {
    AWTEvent awtobj = null;

    if (_evts.size() != 0) {
      Object obj = _evts.firstElement();
      _evts.removeElement(obj);
      awtobj = (AWTEvent)obj;
    }

    return awtobj;
  }
}
