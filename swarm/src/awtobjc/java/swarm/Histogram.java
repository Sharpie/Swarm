package swarm;

import java.awt.*;
import ptplot.*;

class Histogram extends java.awt.Frame {

  private Plot _plot;
  private int _barNum;
  private boolean _first[];

  public Histogram() {

    // bar graphs use the ptplot bargraph facilities
    _plot = new Plot();
    add("Center", _plot);
    _barNum = 1;

    _plot.setNumSets(1);
    _plot.setConnected(false);
    _plot.setMarksStyle("none");
    _plot.setBars(true);

    _first = new boolean[128];
    for (int i=0; i<128; i++) _first[i] = true;

    setSize(400, 200);
    setVisible(true);

    _plot.init();

  }

  public void addBar(String label, String color, double value) {

    // first time out, add our column headings
    if (_first[_barNum]) {
      _plot.addXTick(label, _barNum);
      _first[_barNum] = false;
    }

    _plot.addPoint(0, (double)_barNum, value, false);
    _barNum++;
  }

  public void addBar(String label, String color, int value) {
    // allow us to add integer values by converting them to double
    addBar(label, color, (double)value);
  }

  public void reset() {
    // forget earlier datasets; we'll be drawing in a new set of
    // values
    _plot.setNumSets(1);
    _barNum = 1;
  }

  public void paint(Graphics g) {
    _plot.drawPlot(g,true);
  }

  public void update () {
    repaint();
  }

}

