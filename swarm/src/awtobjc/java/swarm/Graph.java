package swarm;

import java.awt.*;
import java.awt.event.*;
import java.util.Hashtable;
import ptplot.*;

class Graph extends java.awt.Frame
{

  private int _numPoints;
  private int _numDataSets;
  private Hashtable _dataSets;
  private Plot _plot;
  private boolean _first[];
  

  private static final int DYNINC = 10;	// for dynamic data, refill this often
  private static final int NUMSETS = 63;// max number of datasets

  public Graph(int w, int h) {
    _numPoints = 0;
    _numDataSets = 0;
    _dataSets = new Hashtable();

    _first = new boolean[NUMSETS];
    for (int i=0; i<NUMSETS; i++) _first[i] = true;

    _plot = new Plot();
    add("Center", _plot);

    _plot.setNumSets(NUMSETS);
    _plot.setConnected(true);

    setSize(w, h);
    setVisible(true);

    _plot.init();
  }

  // add a new plot of a dataset under the given name
  public synchronized void createElement(String name) {
    System.out.println("Graph: adding line "+name);
    ++_numDataSets;
    _dataSets.put(name, new Integer(_numDataSets));
  }

  public void setRanges(double minx, double maxx, double miny, double maxy) {
    System.out.println("JGraph: setRanges");
    _plot.setXRange(minx, maxx);
    _plot.setYRange(miny, maxy);
  }

  public void setScaleMode(int xs, int ys) {
    System.out.println("JGraph: Scale mode "+xs+","+ys);
  }

  public void setAxisLabels(String xl, String yl) {
    _plot.setXLabel(xl);
    _plot.setYLabel(yl);
  }

  public void setTitle(String title) {
    super.setTitle(title);
  }

  // add another data point to this graph
  public void addElem(String elemName, double x, double y) {
    int dataSet = ((Integer)_dataSets.get(elemName)).intValue(); 
    boolean connect = true;
    
    if (_first[dataSet]) {
      // first point isn't connected to anyone
      connect = false;
      _first[dataSet] = false;
    }

    _plot.addPoint(dataSet, x, y, connect);

    // compromise: refilling on every point resizes too much; manually
    // refilling sucks for data that's dynamic
    if ((_numPoints++ % DYNINC) == 0)
      _plot.fillPlot();
  }

  // remove the n'th data point from this graph
  public void dropElem(String elemName) {
    System.out.println("JGraph: drop "+elemName);
    _dataSets.remove(elemName);
  }

  // add another data point to this graph
  public void setColor(String elemName, String color) {
  }

  public void paint(Graphics g) {
    _plot.drawPlot(g,true);
  }

  public void update() {
    repaint();
  }
}
