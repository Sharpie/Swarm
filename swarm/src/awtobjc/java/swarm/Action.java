package swarm;

import java.util.*;
import java.lang.*;
import java.math.*;

import swarm.Mutex;

class Action {
  long actionCache;	// C pointer to actionCache object
  Hashtable nameToId;	// map action names to objc id's
  Mutex schedLock;	// simple lock shared with ObjC scheduler

  public Action(long actionCache) {
    this.actionCache = actionCache;
    this.schedLock = new Mutex();
    nameToId = new Hashtable();
  }

  /* ***************************************************************** */

  // low-level objective C routine to send an action
  native void sendAction(long actionCache, 
			 String actName,			
			 long actId, 
			 long arg);

  public synchronized void sendAction(String actName, long arg) {

    if (!nameToId.containsKey(actName)) {
      System.out.println(actName+" NOT found in hash");
      return;
    }

    System.out.println("sendAction: sending "+actName);

    // take a lock so we don't bump into normal scheduler operations
    schedLock.take();

    try {
      // invoke the objc routine to schedule an action to be executed
      sendAction(actionCache, actName, 
		 ((Long)nameToId.get(actName)).longValue(), arg);
    } finally {
      // drop the lock
      schedLock.drop();
    }
  }

  public void sendAction(String actName) {
    sendAction(actName, 0);
  }

  // ObjC will tell us the name to id mappings for actions we're expecting
  public synchronized void assocAction(String actName, Long actId) {
    nameToId.put(actName, actId);
  }
}

