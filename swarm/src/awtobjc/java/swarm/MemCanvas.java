package swarm;

import java.awt.*;
import java.awt.event.*;

import swarm.JavaInput;

// MemCanvas: a canvas holding the image that needs to
// be redrawn every time we repaint

class MemCanvas extends java.awt.Canvas 
{

  private int _w;
  private int _h;
  private Image _i;
  private Graphics _ig;		// image graphics context

  MemCanvas() {
    super();

    _w = getSize().width;
    _h = getSize().height;

    if (_w != 0 && _h != 0) {
      // create a new cached image
      _i = createImage(_w, _h);
      _ig = _i.getGraphics();
    } else {
      _i = null;
    }

    // allow mouse clicks to filter through
    enableEvents(AWTEvent.MOUSE_EVENT_MASK);
  }

  public void setSize(int w, int h) {
    if (w != _w && h != _h) {
      super.setSize(w, h);
      _i = createImage(w, h);
      _w = w; _h = h;
      _ig = _i.getGraphics();
    }
  }

  public void componentResized() {
    setSize(getSize().width, getSize().height);
  }

  public Graphics imageGraphics() {
    if (_i == null) {
      setSize(getSize().width, getSize().height);
    }
    return _ig;
  }

  public void update(Graphics g) {
    // since we're doing the whole bitmap, don't clear
    // the background
    paint(g);
  }

  public void paint(Graphics g) {
    System.out.println ("MemCanvas paint");
    if (g instanceof PrintGraphics) {
      System.out.println("PRINT! "+_w+" "+_h);
      return;
    }

    if (_i != null) {
      g.drawImage(_i, 0, 0, null);
    }
  }

  protected void processMouseEvent(MouseEvent event) {
    System.out.println("event!" + event);
    JavaInput.queue.putEvent(event);
    System.out.println("removed "+JavaInput.queue.getEvent());

    // do the standard gui processing
    super.processMouseEvent(event);
  }

}
