package swarm;

import java.awt.*;
import java.awt.event.*;


class Raster extends java.awt.Frame
  implements ActionListener,
  ComponentListener
{
  int _color;
  int _width, _height;

  MemCanvas _canvas;

  Raster(int w, int h) {
    super();

    System.out.println("Raster init");

    setSize(w, h);
    _width = w; _height = h;
    _color = 0;

    // give us a canvas to stretch over the whole thing
    _canvas = new MemCanvas();
    add("Center", _canvas);

    this.addComponentListener(this);

    setBackground(Color.black);
    setVisible(true);

    if (false) {
      addWindowListener(new JRWAdapter());
    }
  }

  class JRWAdapter extends WindowAdapter {
    public void windowClosing(WindowEvent event) {
      System.out.println("window close");
      dispose();
    }
  }

  public void erase() {
    System.out.println("Raster erase");
    Graphics g = _canvas.imageGraphics();
    g.setColor(Color.black);
    g.clearRect(0, 0, _width, _height);
    return;
  }

  public void drawPoint(int x, int y, int color) {
    System.out.println("point "+x+","+y);
    fillRectangle(x, y, x+1, y+1, color);
  }

  public void fillRectangle(int x0, int y0, int x1, int y1, int color) {
    Graphics g = _canvas.imageGraphics();

    if (color != _color) {
      _color = color;
      _color |= 0xff000000;
      g.setColor(new Color(_color));
    }
    g.fillRect(x0, y0, x1-x0, y1-y0);
  }

  public void drawSelf() {
    System.out.println ("drawSelf");
    repaint();
  }

  public void paint(Graphics g) {
    System.out.println ("Raster paint _canvas="+_canvas);
    _canvas.repaint();
  }

  public void componentResized(ComponentEvent e) {
    System.out.println("Mem: resized!!!");
    _canvas.componentResized();
  }

  public void componentMoved(ComponentEvent e) {
    System.out.println("Mem: moved!!!");
  }

  public void componentShown(ComponentEvent e) {
    System.out.println("Mem: shown!!!");
  }

  public void componentHidden(ComponentEvent e) {
  }

  public void actionPerformed(ActionEvent event) {
    Object source = event.getSource();
    System.out.println("event!" + event);
  }

}
