package swarm;

import java.lang.*;

class Mutex {
  private boolean _locked;

  public Mutex() {
    _locked = false;
  }

  public synchronized void take() {
    while (_locked)
      try { wait(); } 
    catch (InterruptedException ex) {};

    // this thread has the lock
    _locked = true;
  }

  public synchronized void drop() {
    // ASSUME this thread had the lock.
    _locked = false;
    notifyAll();
  }
}
