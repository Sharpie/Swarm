
import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;


class JavaJItem extends Panel
{
  int _type;
  int _color;
  int _width, _height;

  private static final int OVAL = 0;
  private static final int RECT = 1;

  JavaJItem(int type, int color, int w, int h) {
    _type = type;
    _color = 0;
    _width = w; _height = h;

    setVisible(true);

    // allow mouse events to filter through
    enableEvents(AWTEvent.MOUSE_EVENT_MASK);
    // enableEvents(AWTEvent.MOUSE_MOTION_EVENT_MASK);
  }

  public Dimension preferredSize() {
    return new Dimension(_width+1, _height+1);
  }

  public Dimension minimumSize() {
    return new Dimension(_width+1, _height+1);
  }

  public void setColor(int color) {
    if (color != _color) {
      _color = color;
      _color |= 0xff000000;
    }
  }

  public void erase() {
    Graphics g = getGraphics();

    paintObj(g);

    g.dispose();
    return;
  }

  public void paintObj(Graphics g) {
    g.setColor(Color.green);

    switch (_type) {
    case OVAL:
      g.fillOval(0, 0, _width, _height);
      break;

    case RECT:
      g.fillRect(0, 0, _width, _height);
      break;

    default:
      System.exit(1);
      break;
    }
  }

  public void update(Graphics g) {
    // update normally clears out the area first... a problem in our
    // case since we want to repait what's under us before we draw over
    // it with our rectangle
    paint(g);
  }

  public void paint(Graphics g) {

    Rectangle r = bounds();

    // pull the image underlying your parent, or we'll write out a block
    // of white
    SwarmFrame jf = ((SwarmFrame)getParent());

    CropImageFilter cropf = new CropImageFilter(r.x, r.y, r.width, r.height);
    Image jfi = jf.bgImage();
    ImageProducer ip = new FilteredImageSource(jfi.getSource(), cropf);
    Image img = getToolkit().createImage(ip);

    // wait until it's loaded or we'll get crap
    waitImage(img);
    g.drawImage(img, 0, 0, null);

    paintObj(g);
  }

  public void waitImage(Image img) {
    try {
      MediaTracker t = new MediaTracker(this);
      t.addImage(img, 0);
      t.waitForAll();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  protected void processMouseEvent(MouseEvent event) {
    System.out.println("event!" + event);

    // add this event to a queue so we can get at it later from ObjC
    SwarmJavaInput.queue.putEvent(event);

    // do the standard gui processing
    super.processMouseEvent(event);
  }

}
