package swarm;

import java.awt.*;
import java.awt.event.*;

import swarm.Action;

class JavaControlPanel extends java.awt.Frame
  implements ActionListener
{
  Button stop;
  Button step;
  Button next_time;
  Button run;
  Button quit;

  Action actionCache; 

  int theState;

  static private int RUN = 0;
  static private int STOP = 1;
  static private int STEP = 2;
  static private int NEXT = 3;
  static private int QUIT = 4;

  JavaControlPanel() {
    super("Swarm Action");
    
    setLayout(new FlowLayout(FlowLayout.LEFT));

    run = new Button("Run");
    run.addActionListener(this);
    add(run);

    stop = new Button("Stop");
    stop.addActionListener(this);
    add(stop);

    step = new Button("Step");
    step.addActionListener(this);
    add(step);

    next_time = new Button("Next Time");
    next_time.addActionListener(this);
    add(next_time);

    quit = new Button("Quit");
    quit.addActionListener(this);
    add(quit);

    setVisible(true);
    setSize(100, 140);
    theState = RUN;

    Thread.currentThread().setName("Main Thread");
  }

  // called from objective C to set our action cache
  public void setActionCache(int objcActionCache) {
    this.actionCache = new Action(objcActionCache);
  }

  public void addAction(String actName, int actId) {
System.out.println("Add Action "+actName+" id="+actId);
    actionCache.assocAction(actName, new Long(actId));
  }


  public synchronized void setState(int state) {
    int oldstate = theState;

    System.out.println("setting state to "+state);
    if (oldstate == state)
      return;

    theState = state;

    if (oldstate == STOP)
      notify();
  }

  //  
  public synchronized int getState() {
    return theState;
  }

  // 
  // called from simulation to block until run/step is pressed
  public synchronized void waitRUN() {
    System.out.println("wait run "+Thread.currentThread().getName());

    while (theState == STOP) {
      System.out.println("wait run...");
      try {
	wait();
      } catch (InterruptedException ex) {};
    }

    System.out.println("wait go");
  }

  public void actionPerformed(ActionEvent event) {
    Object source = event.getSource();
    System.out.println(Thread.currentThread().getName()+" event! " + event);

    // we send "action" requests (stop, run, step, etc.) back to objective
    // C where they get put in the scheduler and executed when the scheduler
    // gets a chance

    if (source == quit) {
      // all done...
      System.out.println("quit!");
      System.exit(0);
    } else if (source == step) {
      actionCache.sendAction("Step");
      setState(STEP);
    } else if (source == next_time) {
      actionCache.sendAction("Next");
      setState(NEXT);
    } else if (source == run) {
      actionCache.sendAction("Start");
      setState(RUN);
    } else if (source == stop) {
      actionCache.sendAction("Stop");
      setState(STOP);
    }
  }

}
