// Java mousetrap application. Copyright © 1999 Santa Fe Institute.
// This application is distributed without any warranty; without even
// the implied warranty of merchantability or fitness for a particular
// purpose.  See file COPYING for details and terms of copying.

/**
 * A helper class that keeps statistics on the mousetrap world. It
 * is an object that is model specific, but does not live "in" the
 * mousetrap model. Counts how many times anything has ever been
 * triggered, also how many balls are in the air.
 **/
public class MousetrapStatistics {
  public int numTriggered = 0;
  public int numBalls = 0;

  public Object addOneTriggered () {
    numTriggered++;
    return this;
  }

  public Object addOneBall () {
    numBalls++;
    return this;
  }

  public Object removeOneBall () {
    if (numBalls > 0)
      numBalls--;
    else
      System.err.println ("Error: negative balls!\n");
    return this;
  }

  public int getNumTriggered () {
    return numTriggered;
  }

  public int getNumBalls () {
    return numBalls;
  }
}
