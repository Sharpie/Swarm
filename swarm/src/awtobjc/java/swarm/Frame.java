package swarm;

import FractionalLayout;
import OriginConstraint;

import java.awt.Image;
import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.event.MouseEvent;

import swarm.JavaInput;

// Frame: a container that holds various components
// that are added at particular locations.

// Use the FractionalLayoutManger that allows much more precise (and
// fragile) placement of components to line up with the expectations of
// the Tk/Tcl world.

class Frame extends java.awt.Frame
{
  private int _w;
  private int _h;
  private FractionalLayout _layout;
  private Image _i;
  private Color _color;

  Frame(int w, int h) {
    super();
    setSize(w, h);

    _w = w;
    _h = h;
    _color = Color.black;

    _layout = new FractionalLayout();
    setLayout(_layout);

    addNotify ();
    // back up this frame with a memory image 
    _i = createImage(_w, _h);

    System.out.println ("Frame _i: "+_i+" w: "+_w+" h: "+_h);

    setVisible(true);
  }

  public Image bgImage() {
    return _i;
  }

  public void setSize(int w, int h) {
    super.setSize(w, h);
    Image i = createImage(w, h);
    if (_i != null) {
      // copy old image into this
    }
    _i = i;
  }

  public void placeObj(Component c, int x, int y) {
    // make a constraint that will stick the new object
    // at the desired location
    _layout.setConstraint(c, new OriginConstraint(0.0, x, 0.0, y));

    // add it to this container
    add(c);
  }

  public void moveObj(Component c, int x, int y) {
    System.out.println("now moving item at "+x+","+y);

    _layout.removeLayoutComponent(c);
    _layout.setConstraint(c, new OriginConstraint(0.0, x, 0.0, y));

    pack();
  }

  public void setColor(String color) {
    if (color.equals("blue"))
      _color = Color.blue;
    else if (color.equals("black"))
      _color = Color.black;
    else if (color.equals("red"))
      _color = Color.blue;
    else if (color.equals("green"))
      _color = Color.green;
    else if (color.equals("orange"))
      _color = Color.orange;
    else if (color.equals("white"))
      _color = Color.white;
    else
      _color = Color.pink;
  }

  public void drawLine(int fx, int fy, int tx, int ty) {
    Graphics g = _i.getGraphics();

    g.setColor(_color);
    g.drawLine(fx, fy, tx, ty);
    g.setColor(Color.black);

    paintAll(g);
    repaint();
  }

  public void update(Graphics g) {
    if (_i != null) {
      paintAll(_i.getGraphics());
      System.out.println ("update");
      g.drawImage(_i, 0, 0, null);
    }
  }

  public void paint(Graphics g) {
    System.out.println ("Frame paint: "+_i);
    if (_i != null)
      g.drawImage(_i, 0, 0, this);
  }

  protected void processMouseEvent(MouseEvent event) {
    System.out.println("event!" + event);

    // add this event to a queue so we can get at it later from ObjC
    JavaInput.queue.putEvent(event);

    // do the standard gui processing
    super.processMouseEvent(event);
  }
}
