/* A signal plotter.

@Copyright (c) 1997 The Regents of the University of California.
All rights reserved.

Permission is hereby granted, without written agreement and without
license or royalty fees, to use, copy, modify, and distribute this
software and its documentation for any purpose, provided that the
above copyright notice and the following two paragraphs appear in all
copies of this software.

IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
THE UNIVERSITY OF CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGE.

THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE SOFTWARE
PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND THE UNIVERSITY OF
CALIFORNIA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,
ENHANCEMENTS, OR MODIFICATIONS.

                                                PT_COPYRIGHT_VERSION_2
                                                COPYRIGHTENDKEY
*/
package ptplot;

// FIXME: To do
//   - support for oscilloscope-like plots (where x axis wraps around).
//   - steps between points rather than connected lines.
//   - cubic spline interpolation
//   - support error bars (ydelta, or ylow yhigh)
//     (adjust yrange to fit error bars)
//   - log scale axes
//   - bar graph output does not handle having the y value be 0 or
//     less than 0.  With the dataset below, there is no bar for the 2nd val.
//     1 1
//     3 -0.2
//     
// NOTE: The XOR drawing mode is needed in order to be able to erase
// plotted points and restore the grid line, tick marks, and boundary
// rectangle.  Another alternative would be to put the tick marks
// outside the rectangle, disallow grid marks, and adjust drawing so
// it never overlaps the boundary rectangle.  Then erasing could be
// done by redrawing in white. This would be better.

// NOTE: There are quite a few subjective spacing parameters, all
// given, unfortunately, in pixels.  This means that as resolutions
// get better, this program may need to be adjusted.

import java.awt.*;
import java.io.*;
import java.util.*;

//////////////////////////////////////////////////////////////////////////
//// Plot
/** 
 * A flexible signal plotter.  The plot can be configured and data can
 * be provided either through a file with commands or through direct
 * invocation of the public methods of the class or by passing pxgraph
 * arguments through the "pxgraphargs" parameter.  If a file is used,
 * the file can be given as a URL through the <code>setDataurl</code>
 * method in the parent class.  The file contains any number commands,
 * one per line.  Unrecognized commands and commands with syntax
 * errors are ignored.  Comments are denoted by a line starting with a
 * pound sign "#".  The recognized commands include those supported by
 * the base class, plus a few more.  The commands are case
 * insensitive, but are usually capitalized.  The following command
 * defines the number of data sets to be plotted.
 * <pre>
 * NumSets: <i>positiveInteger</i>
 * </pre>
 * If data is provided for more data sets than this number, those
 * data are ignored.  Each dataset can be optionally identified with
 * color (see the base class) or with unique marks.  The style of
 * marks used to denote a data point is defined by one of the following
 * commands:
 * <pre>
 * Marks: none
 * Marks: points
 * Marks: dots
 * Marks: various
 * </pre>
 * Here, "points" are small dots, while "dots" are larger.  If "various"
 * is specified, then unique marks are used for the first ten data sets,
 * and then recycled.
 * Using no marks is useful when lines connect the points in a plot,
 * which is done by default.  To disable connecting lines, use:
 * <pre>
 * Lines: off
 * </pre>
 * To reenable them, use
 * <pre>
 * Lines: on
 * </pre>
 * You can also specify "impulses," which are lines drawn from a plotted point
 * down to the x axis.  These are off by default, but can be turned on with the
 * command:
 * <pre>
 * Impulses: on
 * </pre>
 * or back off with the command
 * <pre>
 * Impulses: off
 * </pre>
 * To create a bar graph, turn off lines and use any of the following commands:
 * <pre>
 * Bars: on
 * Bars: <i>width</i>
 * Bars: <i>width, offset</i>
 * </pre>
 * The <i>width</i> is a real number specifying the width of the bars
 * in the units of the x axis.  The <i>offset</i> is a real number
 * specifying how much the bar of the <i>i</i><sup>th</sup> data set
 * is offset from the previous one.  This allows bars to "peek out"
 * from behind the ones in front.  Note that the frontmost data set
 * will be the last one.  To turn off bars, use
 * <pre>
 * Bars: off
 * </pre>
 * To specify data to be plotted, start a data set with the following command:
 * <pre>
 * DataSet: <i>string</i>
 * </pre>
 * Here, <i>string</i> is a label that will appear in the legend.
 * It is not necessary to enclose the string in quotation marks.
 * The data itself is given by a sequence of commands with one of the
 * following forms:
 * <pre>
 * <i>x</i>,<i>y</i>
 * draw: <i>x</i>,<i>y</i>
 * move: <i>x</i>,<i>y</i>
 * </pre>
 * The "draw" command is optional, so the first two forms are equivalent.
 * The "move" command causes a break in connected points, if lines are
 * being drawn between points. The numbers <i>x</i> and <i>y</i> are
 * arbitrary numbers as supported by the Double parser in Java.
 * The numbers can be separated by commas, spaces or tabs. 
 *
 * This plotter has some limitations:
 * <ul>
 * <li> Marks, impulses, and bars are assumed to apply to the entire
 *      plot, i.e. to all data sets.  Although it is possible to change
 *      these styles for different data sets, the graph will not be
 *      correctly redrawn when it gets redrawn due to zooming in or out
 *      or due to a window exposure event.
 * <li> If you zoom in far enough, the plot becomes unreliable.
 *      In particular, if the total extent of the plot is more than
 *      2<sup>32</sup> times extent of the visible area, quantization
 *      errors can result in displaying points or lines.
 *      Note that 2<sup>32</sup> is over 4 billion.
 * </ul>
 *
 * @author Edward A. Lee, Christopher Hylands
 * @version @(#)Plot.java	1.66 10/30/97
 */
public class Plot extends PlotBox {

    //////////////////////////////////////////////////////////////////////////
    ////                         public methods                           ////
   
    /**
     * In the specified data set, add the specified x,y point to the
     * plot.  Data set indices begin with zero.  If the dataset
     * argument is out of range, ignore.  The number of data sets is
     * given by calling *setNumSets()*.  The fourth argument indicates
     * whether the point should be connected by a line to the previous
     * point.
     */
    public synchronized void addPoint(int dataset, double x, double y,
            boolean connected) {
        _addPoint(_graphics, dataset, x, y, connected);
    }
    
    /**
     * Draw the axes and then plot all points.  This is synchronized
     * to prevent multiple threads from drawing the plot at the same
     * time.  It calls <code>notify()</code> at the end so that a
     * thread can use <code>wait()</code> to prevent it plotting
     * points before the axes have been first drawn.  If the argument
     * is true, clear the display first.
     */
    public synchronized void drawPlot(Graphics graphics,
            boolean clearfirst) {
        super.drawPlot(graphics, clearfirst);
        // Plot the points
        for (int dataset = 0; dataset < _numsets; dataset++) {
            // FIXME: Make the following iteration more efficient.
            Vector data = _points[dataset];
            for (int pointnum = 0; pointnum < data.size(); pointnum++) {
                _drawPlotPoint(graphics, dataset, pointnum);
            }
        }
        notify();
    }
    
    /** 
     * Erase the point at the given index in the given dataset.  If
     * lines are being drawn, also erase the line to the next points
     * (note: not to the previous point).  The point is not checked to
     * see whether it is in range, so care must be taken by the caller
     * to ensure that it is.
     */
    public synchronized void erasePoint(int dataset, int index) {
        _erasePoint(_graphics, dataset, index);  
    }

    /**
     * Return the maximum number of datasets
     */
    public int getMaxDataSets() {
        return _MAX_DATASETS;
    }

    /**
     * Initialize the plotter, parse any data files.
     */
    public synchronized void init() {
        if (_debug > 8) System.out.println("Plot: init");

        setNumSets(_numsets);
        _currentdataset = -1;

        super.init();
        if (_dataurls != null ) {
            // If the pxgraphargs parameter was set, then we might have more
            // than one file to plot.
            Enumeration urls = _dataurls.elements();
            while (urls.hasMoreElements()) {
                String url = (String) urls.nextElement();
                if (_debug > 3) System.out.println("Plot: getting "+url);
                parseFile(url);
            }
        }
    }

    /**
     * Draw the axes and the accumulated points.
     */
    public void paint(Graphics graphics) {
        drawPlot(graphics,true);
    }

    /** Parse pxgraph style command line arguments.
     * This method exists only for backward compatibility with the X11 pxgraph
     * program.
     * @exception ptplot.CmdLineArgException if there is a problem parsing
     * the command line arguments passed in.
     */ 
    public int parseArgs(String args[]) throws CmdLineArgException {
        int i = 0, j, argsread;


        // If we see both -nl and -bar, assume we do an impulse plot.
        boolean sawbararg = false; // Saw -bar arg.
        boolean sawnlarg = false;  // Saw -nl arg.
        int savedmarks = 0;        // Save _marks in case we have -P -bar -nl.

        String arg;
        String unsupportedOptions[] = {
            "-bd", "-brb", "-bw", "-gw", "-lw", "-zg", "-zw"
        };
        String unsupportedFlags[] = {
            "-bb", "-lnx", "-lny"
        };
        // Default URL to be opened
        String dataurl = "";

        String title = "A plot";

        while (i < args.length && (args[i].startsWith("-") || 
                args[i].startsWith("=")) ) {
            arg = args[i++];
            if (_debug > 2) System.out.print("Plot: arg = " + arg + "\n");

            if (arg.startsWith("-")) {
                // Search for unsupported options that take arguments
                boolean badarg = false;
                for(j = 0; j < unsupportedOptions.length; j++) {
                    if (arg.equals(unsupportedOptions[j])) {
                        System.err.println("pxgraph: " + arg +
                                " is not yet supported");
                        i++;
                        badarg = true;
                    }
                }
                if (badarg) continue;
                // Search for unsupported boolean flags
                for(j = 0; j < unsupportedFlags.length; j++) {
                    if (arg.equals(unsupportedFlags[j])) {
                        System.err.println("pxgraph: " + arg +
                                " is not yet supported");
                        badarg = true;
                    }

                }
                if (badarg) continue;
                if (arg.equals("-bg")) {
                    setBackground(getColorByName(args[i++]));
                    continue;
                } else if (arg.equals("-brw")) {
                    // -brw <width> BarWidth Bars: 
                    // We default the baroffset to 0 here if the value does
                    // not include a comma.
                    if (arg.indexOf(",") == -1) {
                        if (!_parseLine("Bars: " + args[i++]+",0")) {
                            throw new 
                                CmdLineArgException("Failed to parse `"+
                                        arg+"'");
                        }
                    } else {
                        if (!_parseLine("Bars: " + args[i++])) {
                            throw new 
                                CmdLineArgException("Failed to parse `"+
                                        arg+"'");
                        }
                    }
                    continue;
                } else if (arg.equals("-lf")) {
                    // -lf <labelfont>
                    setLabelFont(args[i++]);
                    continue;
                } else if (arg.equals("-lx")) {
                    // -lx <xl,xh> XLowLimit, XHighLimit  XRange: 
                    if (!_parseLine("XRange: " + args[i++])) {
                        throw new 
                            CmdLineArgException("Failed to parse `"+arg+"'");
                    }
                    continue;
                } else if (arg.equals("-ly")) {
                    // -ly <yl,yh> YLowLimit, YHighLimit  YRange: 
                    if (!_parseLine("YRange: " + args[i++])) {
                        throw new 
                            CmdLineArgException("Failed to parse `"+arg+"'");
                    }
                    continue;
                } else if (arg.equals("-t")) {
                    // -t <title> TitleText "An X Graph"
                    title =  args[i++];
                    continue;
                } else if (arg.equals("-tf")) {
                    // -tf <titlefont>
                    setTitleFont(args[i++]);
                    continue;
                } else if (arg.equals("-x")) {
                    // -x <unitName> XUnitText XLabel:
                    setXLabel(args[i++]); 
                    continue;
                } else if (arg.equals("-y")) {
                    // -y <unitName> YUnitText YLabel:
                    setYLabel(args[i++]); 
                    continue;               
                } else if (arg.equals("-bar")) {
                    //-bar BarGraph Bars: on Marks: none Lines: off
                    // If we saw the -nl arg, then assume impulses 
                    sawbararg = true;
                    if (sawnlarg) {
                        setImpulses(true);
                    } else {
                        setBars(true); 
                        // Save _marks in case we did -P -bar -nl.
                        savedmarks = _marks;
                        setMarksStyle("none");
                    }
                    setConnected(false);
                    continue;
                } else if (arg.equals("-binary")) {
                    setBinary(true);
                    continue;
                } else if (arg.equals("-db")) {
                    _debug = 6;
                    continue;
                } else if (arg.equals("-debug")) {
                    // -debug is not in the original X11 pxgraph.
                    _debug = (int)Integer.valueOf(args[i++]).intValue();
                    continue;
                } else if (arg.equals("-fg")) {
                    setForeground(getColorByName(args[i++]));
                    continue;
                } else if (arg.equals("-help")) {
                    // -help is not in the original X11 pxgraph.
                    //_help();
                    continue;
                } else if (arg.equals("-impulses")) {
                    // -impulses is not in the original X11 pxgraph.
                    setImpulses(true);
                    setConnected(false);
                    continue;
                } else if (arg.equals("-m")) {
                    // -m Markers Marks: various
                    setMarksStyle("various");
                    continue;
                } else if (arg.equals("-M")) {
                    // -M StyleMarkers Marks: various
                    setMarksStyle("various");
                    continue;
                } else if (arg.equals("-nl")) {
                    // -nl NoLines Lines: off
                    // If we saw the -bar arg, then assume impulses 
                    sawnlarg = true;
                    if (sawbararg) {
                        // Restore the _marks in case we did -P -bar -nl
                        _marks = savedmarks;
                        setBars(false); 
                        setImpulses(true);
                    }
                    setConnected(false);
                    continue;
                } else if (arg.equals("-p")) {
                    // -p PixelMarkers Marks: points
                    setMarksStyle("points");
                    continue;
                } else if (arg.equals("-P")) {
                    // -P LargePixel Marks: dots\n 
                    setMarksStyle("dots");
                    continue;
                } else if (arg.equals("-rv")) {
                    setBackground(getColorByName("black"));
                    setForeground(getColorByName("white"));
                    continue;
                } else if (arg.equals("-test")) {
                    // -test is not in the original X11 pxgraph.
                    //_test = true;
                    continue;
                } else if (arg.equals("-tk")) {
                    setGrid(false);
                    continue;
                } else if (arg.equals("-v") || arg.equals("-version")) {
                    // -version is not in the original X11 pxgraph.
                    //_version();
                    continue;
                } else if (arg.equals("-m")) {

                } if (arg.length() > 1  && arg.charAt(0) == '-') {
                    // Process '-<digit> <datasetname>'
                    try {
                        Integer datasetnumberint = new
                            Integer(arg.substring(1));
                        int datasetnumber = datasetnumberint.intValue();
                        if (datasetnumber >= 0 &&
                                datasetnumber <= getMaxDataSets()) {
                            if (_debug > 8) 
                                System.out.println("Plot: parseArgs: "+
                                        "calling addLegend "+
                                        datasetnumber+" "+args[i]);
                            addLegend(datasetnumber, args[i++]);
                            continue;
                        }
                    } catch (NumberFormatException e) {}
                }
            } else {
                if (arg.startsWith("=")) {
                    // Process =WxH+X+Y
                    int endofheight;
                    _width = (int)Integer.valueOf(arg.substring(1,
                            arg.indexOf('x'))).intValue();
                    if (arg.indexOf('+') != -1) {
                        _height =
                            Integer.valueOf(arg.substring(
                                    arg.indexOf('x')+1,
                                    arg.indexOf('+'))).intValue();
                    } else {
                        if (arg.length() > arg.indexOf('x')) {
                            _height =
                                Integer.valueOf(arg.substring(
                                        arg.indexOf('x')+1,
                                        arg.length())).intValue();
                        }
                    }
                    // FIXME: it is unclear what X and Y in =WxH+X+Y mean
                    // in a non-toplevel window, so we don't process
                    // those here.  See Pxgraph.java for how to process
                    // X and Y for a toplevel window.
                    continue;
                }
            }
            // If we got to here, then we failed to parse the arg 
            throw new 
                CmdLineArgException("Failed to parse `" + arg + "'");
        }
        if (i < args.length) {
            dataurl=args[i];
        }
        argsread = i++;

        // Now we've parsed the parameters, so we call parent class methods.
        setDataurl(dataurl); // Set the dataurl in PlotBox
        setTitle(title);
        if (_debug > 9) 
            System.out.println("Plot: parseArgs: resize()"+_width+" "+_height);
        resize(_width,_height);

        if (_debug > 0) {
            System.err.println("Plot: dataurl = " + dataurl);
            System.err.println("Plot: title= " + title);
        }
        if (_debug > 3) System.out.println("Plot: argsread = "+ argsread +
                " args.length = "+args.length);
        // Copy the file names into the _dataurls Vector for use later.
        _dataurls = new Vector();
        for(i = argsread+1; i < args.length; i++) {
            if (_debug > 3) System.out.println("Plot: saving "+args[i]);
            _dataurls.addElement(args[i]);
        }
        return argsread;
    }

    /** 
     * Resize the plot.
     * @deprecated As of JDK1.1 in java.awt.component, but we need 
     * to compile under 1.0.2 for netscape3.x compatibility.
     */
    public void resize(int width, int height) {
        if (_debug > 8)
            System.out.println("Plot: resize"+width+" "+height);
        _width = width;
        _height = height;
        super.resize(width,height); // FIXME: resize() is deprecated.
    }

    /**
     * Turn bars on or off.
     */
    public void setBars (boolean on) {
        _bars = on;
    }

    /** 
     * Turn bars on and set the width and offset.  Both are specified
     * in units of the x axis.  The offset is the amount by which the
     * i<sup>th</sup> data set is shifted to the right, so that it
     * peeks out from behind the earlier data sets.
     */
    public void setBars (double width, double offset) {
        _barwidth = width;
        _baroffset = offset;
        _bars = true;
    }
    
    /** 
     * If the argument is true, then the default is to connect
     * subsequent points with a line.  If the argument is false, then
     * points are not connected.  When points are by default
     * connected, individual points can be not connected by giving the
     * appropriate argument to <code>addPoint()</code>.
     */
    public void setConnected (boolean on) {
        _connected = on;
    }
    
    /** 
     * If the argument is true, then a line will be drawn from any
     * plotted point down to the x axis.  Otherwise, this feature is
     * disabled.
     */
    public void setImpulses (boolean on) {
        _impulses = on;
    }
    
    /**
     * Set the marks style to "none", "points", "dots", or "various".
     * In the last case, unique marks are used for the first ten data
     * sets, then recycled.
     */
    public void setMarksStyle (String style) {
        if (style.equalsIgnoreCase("none")) {
            _marks = 0;
        } else if (style.equalsIgnoreCase("points")) {
            _marks = 1;
        } else if (style.equalsIgnoreCase("dots")) {
            _marks = 2;
        } else if (style.equalsIgnoreCase("various")) {
            _marks = 3;
        }
    }

    /** 
     * Specify the number of data sets to be plotted together.
     * Allocate a Vector to store each data set.  Note that calling
     * this causes any previously plotted points to be forgotten.
     * This method should be called before
     * <code>setPointsPersistence</code>.
     * @exception java.io.NumberFormatException if the number is less
     * than 1 or greater than an internal limit (usually 63).
     */
    public void setNumSets (int numsets) throws NumberFormatException {
        if (numsets < 1) {
            throw new NumberFormatException("Number of data sets ("+
                    numsets + ") must be greater than 0.");

        }
        if (numsets > _MAX_DATASETS) {
            throw new NumberFormatException("Number of data sets (" +
                    numsets + ") must be less than the internal limit of " +
                    _MAX_DATASETS + "To increase this value, edit " +
                    "_MAX_DATASETS and recompile");
        }

        this._numsets = numsets;
        _points = new Vector[numsets];
        _prevx = new long[numsets];
        _prevy = new long[numsets];
        for (int i=0; i<numsets; i++) {
            _points[i] = new Vector();
        }
    }
    
    /** 
     * Calling this method with a positive argument sets the
     * persistence of the plot to the given number of points.  Calling
     * with a zero argument turns off this feature, reverting to
     * infinite memory (unless sweeps persistence is set).  If both
     * sweeps and points persistence are set then sweeps take
     * precedence.  This method should be called after
     * <code>setNumSets()</code>.  
     * FIXME: No file format yet.
     */
    public void setPointsPersistence (int persistence) {
        _pointsPersistence = persistence;
        if (persistence > 0) {
            for (int i = 0; i < _numsets; i++) {
                _points[i].setSize(persistence);
            }
        }
    }
    
    /** 
     * A sweep is a sequence of points where the value of X is
     * increasing.  A point that is added with a smaller x than the
     * previous point increments the sweep count.  Calling this method
     * with a non-zero argument sets the persistence of the plot to
     * the given number of sweeps.  Calling with a zero argument turns
     * off this feature.  If both sweeps and points persistence are
     * set then sweeps take precedence.
     * FIXME: No file format yet.
     * FIXME: Not implemented yet.
     */
    public void setSweepsPersistence (int persistence) {
        _sweepsPersistence = persistence;
    }


    /** Start the plot.
     * This method is redefined in child classes, such as PlotLive.   
     */
    public void start () {
    }

    /** Stop the plot.
     * This method is redefined in child classes, such as PlotLive.   
     */
    public void stop () {
    }

    //////////////////////////////////////////////////////////////////////////
    ////                          protected methods                       ////
  
        
    /**
     * Draw bar from the specified point to the y axis.
     * If the specified point is below the y axis or outside the
     * x range, do nothing.  If the <i>clip</i> argument is true,
     * then do not draw above the y range.
     */
    protected void _drawBar (Graphics graphics, int dataset,
            long xpos, long ypos, boolean clip) {
        if (_debug > 21) {
            System.out.println("Plot: _drawBar("+dataset+" "+
                    xpos+" "+ypos+" "+clip+")");
        }
        if (clip) {
            if (ypos < _uly) {
                ypos = _uly;
            } if (ypos > _lry) {
                ypos = _lry;
            }
        }
        if (ypos <= _lry && xpos <= _lrx && xpos >= _ulx) {
            // left x position of bar.
            int barlx = (int)(xpos - _barwidth * _xscale/2 +
                    (_currentdataset - dataset - 1) *
                    _baroffset * _xscale);
            // right x position of bar
            int barrx = (int)(barlx + _barwidth * _xscale);
            if (barlx < _ulx) barlx = _ulx;
            if (barrx > _lrx) barrx = _lrx;
            // Make sure that a bar is always at least one pixel wide.
            if (barlx >= barrx) barrx = barlx+1;
            // The y position of the zero line.
            long zeroypos = _lry - (long) ((0-_yMin) * _yscale);
            if (_lry < zeroypos) zeroypos = _lry;
            if (_uly > zeroypos) zeroypos = _uly;

            if (_debug > 20) {
                System.out.println("Plot:_drawBar ("+barlx+" "+ypos+" "+
                        (barrx - barlx) + " "+(zeroypos-ypos)+") "+barrx+" "+
                        barlx+" ("+_ulx+" "+_lrx+" "+_uly+" "+_lry+
                        ") xpos="+xpos+" ypos="+ypos+" zeroypos="+zeroypos+ 
                        " "+_barwidth+" "+_xscale+" "+_currentdataset+
                        " "+_yMin);
            }

            if (_yMin >= 0 || ypos <= zeroypos) {
                graphics.fillRect(barlx, (int)ypos,
                        barrx - barlx, (int)(zeroypos - ypos));
            } else {
                graphics.fillRect(barlx, (int)zeroypos,
                        barrx - barlx, (int)(ypos - zeroypos));
            }
        }
    }

    /**
     * Draw an impulse from the specified point to the y axis.
     * If the specified point is below the y axis or outside the
     * x range, do nothing.  If the <i>clip</i> argument is true,
     * then do not draw above the y range.
     */
    protected void _drawImpulse (Graphics graphics,
            long xpos, long ypos, boolean clip) {
        if (_debug > 20) {
            System.out.println("Plot: _drawImpulse("+xpos+" "+ypos+" "+
                    clip+") ("+_ulx+" "+_uly+" "+_lrx+" "+_lry+")");
        }
        if (clip) {
            if (ypos < _uly) {
                ypos = _uly;
            } if (ypos > _lry) {
                ypos = _lry;
            }
        }
        if (ypos <= _lry && xpos <= _lrx && xpos >= _ulx) {
            // The y position of the zero line.
            double zeroypos = _lry - (long) ((0-_yMin) * _yscale);
            if (_lry < zeroypos) zeroypos = _lry;
            if (_uly > zeroypos) zeroypos = _uly;
            graphics.drawLine((int)xpos, (int)ypos, (int)xpos,
                    (int)zeroypos);
        }
    }

    /**
     * Draw a line from the specified starting point to the specified
     * ending point.  The current color is used.  If the <i>clip</i> argument
     * is true, then draw only that portion of the line that lies within the
     * plotting rectangle.
     */
    protected void _drawLine (Graphics graphics,
            int dataset, long startx, long starty, long endx, long endy,
            boolean clip) {

        if (clip) {
            // Rule out impossible cases.
            if (_debug > 20) {
                System.out.println("Plot: _drawLine: bounds: " + _ulx +", "+
                        _uly +", "+ _lrx +", "+ _lry);
                System.out.println("Plot: _drawLine:before: " + startx +", "+
                        starty +", "+ endx +", "+ endy);
            }
            if (!((endx <= _ulx && startx <= _ulx) ||
                    (endx >= _lrx && startx >= _lrx) ||
                    (endy <= _uly && starty <= _uly) ||
                    (endy >= _lry && starty >= _lry))) {
                // If the end point is out of x range, adjust
                // end point to boundary.
                // The integer arithmetic has to be done with longs so as
                // to not loose precision on extremely close zooms.
                if (startx != endx) {
                    if (endx < _ulx) {
                        endy = (int)(endy + ((long)(starty - endy) *
                                (_ulx - endx))/(startx - endx));
                        endx = _ulx;
                    } else if (endx > _lrx) {
                        endy = (int)(endy + ((long)(starty - endy) *
                                (_lrx - endx))/(startx - endx));
                        endx = _lrx;
                    }
                }
                    
                // If end point is out of y range, adjust to boundary.
                // Note that y increases downward
                if (starty != endy) {
                    if (endy < _uly) {
                        endx = (int)(endx + ((long)(startx - endx) *
                                (_uly - endy))/(starty - endy));
                        endy = _uly;
                    } else if (endy > _lry) {
                        endx = (int)(endx + ((long)(startx - endx) *
                                (_lry - endy))/(starty - endy));
                        endy = _lry;
                    }
                }
                    
                // Adjust current point to lie on the boundary.
                if (startx != endx) {
                    if (startx < _ulx) {
                        starty = (int)(starty + ((long)(endy - starty) *
                                (_ulx - startx))/(endx - startx));
                        startx = _ulx;
                    } else if (startx > _lrx) {
                        starty = (int)(starty + ((long)(endy - starty) *
                                (_lrx - startx))/(endx - startx));
                        startx = _lrx;
                    }
                }
                if (starty != endy) {
                    if (starty < _uly) {
                        startx = (int)(startx + ((long)(endx - startx) *
                                (_uly - starty))/(endy - starty));
                        starty = _uly;
                    } else if (starty > _lry) {
                        startx = (int)(startx + ((long)(endx - startx) *
                                (_lry - starty))/(endy - starty));
                        starty = _lry;
                    }
                }
            }
                 
            // Are the new points in range?
            if (endx >= _ulx && endx <= _lrx &&
                    endy >= _uly && endy <= _lry &&
                    startx >= _ulx && startx <= _lrx &&
                    starty >= _uly && starty <= _lry) {
                if (_debug > 20) {
                    System.out.println("after: " + startx +", "+ starty +
                            ", "+ endx +", "+ endy);
                }
                graphics.drawLine((int)startx, (int)starty,
                        (int)endx, (int)endy);
            }
        } else {
            // draw unconditionally.
            graphics.drawLine((int)startx, (int)starty,
                    (int)endx, (int)endy);
        }
    }

    /**
     * Put a mark corresponding to the specified dataset at the
     * specified x and y position. The mark is drawn in the current
     * color. What kind of mark is drawn depends on the _marks
     * variable and the dataset argument. If the fourth argument is
     * true, then check the range and plot only points that
     * are in range.
     */
    protected void _drawPoint(Graphics graphics,
            int dataset, long xpos, long ypos,
            boolean clip) {
        if (_debug > 20) {
            System.out.println("Plot:_drawPoint "+dataset+" "+xpos+
                    " "+ypos+" "+" "+clip);
        }

        // If the point is not out of range, draw it.
        if (!clip || (ypos <= _lry && ypos >= _uly &&
                xpos <= _lrx && xpos >= _ulx)) {
            int xposi = (int)xpos;
            int yposi = (int)ypos;
            switch (_marks) {
            case 0:
                // If no mark style is given, draw a filled rectangle.
                // This is used, for example, to draw the legend.
                graphics.fillRect(xposi-6, yposi-6, 6, 6);
                break;
            case 1:
                // points -- use 3-pixel ovals.
                graphics.fillOval(xposi-1, yposi-1, 3, 3);
                break;
            case 2:
                // dots
                graphics.fillOval(xposi-_radius, yposi-_radius,
                        _diameter, _diameter); 
                break;
            case 3:
                // marks
                int xpoints[], ypoints[];
                // Points are only distinguished up to _MAX_MARKS data sets.
                int mark = dataset % _MAX_MARKS;
                switch (mark) {
                case 0:
                    // filled circle
                    graphics.fillOval(xposi-_radius, yposi-_radius,
                            _diameter, _diameter); 
                    break;
                case 1:
                    // cross
                    graphics.drawLine(xposi-_radius, yposi-_radius,
                            xposi+_radius, yposi+_radius); 
                    graphics.drawLine(xposi+_radius, yposi-_radius,
                            xposi-_radius, yposi+_radius); 
                    break;
                case 2:
                    // square
                    graphics.drawRect(xposi-_radius, yposi-_radius,
                            _diameter, _diameter); 
                    break;
                case 3:
                    // filled triangle
                    xpoints = new int[4];
                    ypoints = new int[4];
                    xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                    xpoints[1] = xposi+_radius; ypoints[1] = yposi+_radius;
                    xpoints[2] = xposi-_radius; ypoints[2] = yposi+_radius;
                    xpoints[3] = xposi; ypoints[3] = yposi-_radius;
                    graphics.fillPolygon(xpoints, ypoints, 4);
                    break;
                case 4:
                    // diamond
                    xpoints = new int[5];
                    ypoints = new int[5];
                    xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                    xpoints[1] = xposi+_radius; ypoints[1] = yposi;
                    xpoints[2] = xposi; ypoints[2] = yposi+_radius;
                    xpoints[3] = xposi-_radius; ypoints[3] = yposi;
                    xpoints[4] = xposi; ypoints[4] = yposi-_radius;
                    graphics.drawPolygon(xpoints, ypoints, 5);
                    break;
                case 5:
                    // circle
                    graphics.drawOval(xposi-_radius, yposi-_radius,
                            _diameter, _diameter); 
                    break;
                case 6:
                    // plus sign
                    graphics.drawLine(xposi, yposi-_radius, xposi,
                            yposi+_radius); 
                    graphics.drawLine(xposi-_radius, yposi, xposi+_radius,
                            yposi); 
                    break;
                case 7:
                    // filled square
                    graphics.fillRect(xposi-_radius, yposi-_radius,
                            _diameter, _diameter); 
                    break;
                case 8:
                    // triangle
                    xpoints = new int[4];
                    ypoints = new int[4];
                    xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                    xpoints[1] = xposi+_radius; ypoints[1] = yposi+_radius;
                    xpoints[2] = xposi-_radius; ypoints[2] = yposi+_radius;
                    xpoints[3] = xposi; ypoints[3] = yposi-_radius;
                    graphics.drawPolygon(xpoints, ypoints, 4);
                    break;
                case 9:
                    // filled diamond
                    xpoints = new int[5];
                    ypoints = new int[5];
                    xpoints[0] = xposi; ypoints[0] = yposi-_radius;
                    xpoints[1] = xposi+_radius; ypoints[1] = yposi;
                    xpoints[2] = xposi; ypoints[2] = yposi+_radius;
                    xpoints[3] = xposi-_radius; ypoints[3] = yposi;
                    xpoints[4] = xposi; ypoints[4] = yposi-_radius;
                    graphics.fillPolygon(xpoints, ypoints, 5);
                    break;
                }
                break;
            default:
                // none
            }
        }
    }

    /** Hook for child classes to do any file preprocessing
     */ 
    protected void _newFile(){
        _filecount++;
        _firstinset = true;
        _sawfirstdataset = false;
    }   

    /**
     * Read in a pxgraph format binary file.
     * @exception PlotDataException if there is a serious data format problem.
     * @exception java.io.IOException if an I/O error occurs.
     */ 
    protected void _parseBinaryStream(DataInputStream in) throws PlotDataException,  IOException {
        // This method is similar to _parseLine() below, except it parses
        // an entire file at a time.
        int c;

        boolean connected = false;
        if (_connected) connected = true;

        if (_debug > 8) {
            System.out.println("Plot: _parseBinaryStream _connected = "+
                    _connected);
        }
        try {
            while (true) {
                // Here, we read pxgraph binary format data.
                // For speed reasons, the Ptolemy group extended 
                // pxgraph to read binary format data.
                // The format consists of a command character,
                // followed by optional arguments
                // d <4byte float> <4byte float> - Draw a X,Y point
                // e                             - End of a data set
                // n <chars> \n                  - New set name, ends in \n
                // m                             - Move to a point
                c = in.readByte();
                switch (c) {
                case 'd':
                    {
                        // Data point.
                        float x = in.readFloat();
                        float y = in.readFloat();
                        connected = _addLegendIfNecessary(connected);
                        addPoint(_currentdataset, x, y, connected);
                        if (_connected) connected = true;
                    }
                    break;
                case 'e':
                    // End of set name.
                    connected = false;
                    break;
                case 'n':
                    {
                        StringBuffer datasetname = new StringBuffer();
                        _firstinset = true;
                        _sawfirstdataset = true;
                        _currentdataset++;
                        if (_currentdataset >= _MAX_MARKS) _currentdataset = 0;
                        // New set name, ends in \n.
                        while (c != '\n')
                            datasetname.append(in.readChar());
                        addLegend(_currentdataset, datasetname.toString());
                        setConnected(true);
                    }
                    break;
                case 'm':
                    // a disconnected point
                    connected = false;
                    break;
                default:
                    throw new PlotDataException("Don't understand `" + 
                            (char)c + "' character " +
                            "(decimal value = " + c +
                            ") in binary file");
                }
            } 
        } catch (EOFException e) {}         
    }

    /**
     * Parse a line that gives plotting information. Return true if
     * the line is recognized.  Lines with syntax errors are ignored.
     */
    protected boolean _parseLine (String line) {
        boolean connected = false;
        if (_debug> 8) System.out.println("Plot: _parseLine " + line);
        if (_connected) connected = true;
        // parse only if the super class does not recognize the line.
        if (super._parseLine(line)) {
            return true;
        } else {
            // We convert the line to lower case so that the command
            // names are case insensitive
            String lcLine = new String(line.toLowerCase());
            if (lcLine.startsWith("marks:")) {
                String style = (line.substring(6)).trim();
                setMarksStyle(style);
                return true;
            } else if (lcLine.startsWith("numsets:")) {
                String num = (line.substring(8)).trim();
                try {
                    setNumSets(Integer.parseInt(num));
                }
                catch (NumberFormatException e) {
                    // ignore bogons
                }
                return true;
            } else if (lcLine.startsWith("dataset:")) {
                // new data set
                _firstinset = true;
                _sawfirstdataset = true;
                _currentdataset++;
                if (_currentdataset >= _MAX_MARKS) _currentdataset = 0;
                String legend = (line.substring(8)).trim();
                addLegend(_currentdataset, legend);
                return true;
            } else if (lcLine.startsWith("lines:")) {
                if (lcLine.indexOf("off",6) >= 0) {
                    setConnected(false);
                } else {
                    setConnected(true);
                }
                return true;
            } else if (lcLine.startsWith("impulses:")) {
                if (lcLine.indexOf("off",9) >= 0) {
                    setImpulses(false);
                } else {
                    setImpulses(true);
                }
                return true;
            } else if (lcLine.startsWith("bars:")) {
                if (lcLine.indexOf("off",5) >= 0) {
                    setBars(false);
                } else {
                    setBars(true);
                    if (! _yRangeGiven) {
                        _yBottom = 0;
                    }
                    int comma = line.indexOf(",", 5);
                    String barwidth;
                    String baroffset = null;
                    if (comma > 0) {
                        barwidth = (line.substring(5, comma)).trim();
                        baroffset = (line.substring(comma+1)).trim();
                    } else {
                        barwidth = (line.substring(5)).trim();
                    }
                    try {
                        Double bwidth = new Double(barwidth);
                        double boffset = _baroffset;
                        if (baroffset != null) {
                            boffset = (new Double(baroffset)).
                                doubleValue();
                        }
                        setBars(bwidth.doubleValue(), boffset);
                    } catch (NumberFormatException e) {
                        // ignore if format is bogus.
                    }
                }
                return true;
            } else if (line.startsWith("move:")) {
                // a disconnected point
                connected = false;
                // deal with 'move: 1 2' and 'move:2 2'
                line = line.substring(5, line.length()).trim();
            } else if (line.startsWith("move")) {
                // a disconnected point
                connected = false;
                // deal with 'move 1 2' and 'move2 2'
                line = line.substring(4, line.length()).trim();
            } else if (line.startsWith("draw:")) {
                // a connected point, if connect is enabled.
                line = line.substring(5, line.length()).trim();
            } else if (line.startsWith("draw")) {
                // a connected point, if connect is enabled.
                line = line.substring(4, line.length()).trim();
            }
            // See if an x,y or x<Space>y x<Taby> point is given
            line = line.trim();
            int fieldsplit = line.indexOf(",");
            if (fieldsplit == -1) {
                fieldsplit = line.indexOf(" ");
            }
            if (fieldsplit == -1) {
                fieldsplit = line.indexOf(" ");  // a tab
            }

            if (fieldsplit > 0) {
                String x = (line.substring(0, fieldsplit)).trim();
                String y = (line.substring(fieldsplit+1)).trim();
                try {
                    Double xpt = new Double(x);
                    Double ypt = new Double(y);
                    connected = _addLegendIfNecessary(connected);
                    addPoint(_currentdataset, xpt.doubleValue(),
                            ypt.doubleValue(), connected);
                    return true;
                } catch (NumberFormatException e) {
                    // ignore if format is bogus.
                }
            }
        }
        return false;
    }

    //////////////////////////////////////////////////////////////////////////
    ////                       protected variables                        ////
    
    // The current dataset
    protected int _currentdataset = -1;
    
    // A vector of datasets.
    protected Vector[] _points;

    // An indicator of the marks style.  See _parseLine method for
    // interpretation.
    protected int _marks;
    
    protected int _numsets = _MAX_DATASETS;

    //////////////////////////////////////////////////////////////////////////
    ////                       private methods                            ////

    /* Add a legend if necessary, return the value of the connected flag.
     */
    private boolean _addLegendIfNecessary(boolean connected) {
        if (! _sawfirstdataset  || _currentdataset < 0) {
            // We did not set a DataSet line, but
            // we did get called with -<digit> args
            _sawfirstdataset = true;
            ++_currentdataset;
        }
        if (_debug > 14) {
            System.out.println("Plot _addLegendIfNecessary( "+connected+" ) "+
                    + (_filecount) + " " + (_currentdataset) +
                    "<"+getLegend(0)+">");
        }
        if (getLegend(_currentdataset) == null) {
            // We did not see a "DataSet" string yet,
            // nor did we call addLegend().
            _firstinset = true;
            _sawfirstdataset = true;
            addLegend(_currentdataset,
                    new String("Set "+ _currentdataset));
        }
        if (_firstinset) {
            connected = false;
            _firstinset = false;
        }
        return connected;
    }
    
    /* In the specified data set, add the specified x,y point to the
     * plot.  Data set indices begin with zero.  If the dataset
     * argument is out of range, ignore.  The number of data sets is
     * given by calling *setNumSets()*.  The fourth argument indicates
     * whether the point should be connected by a line to the previous
     * point.
     */
    private synchronized void _addPoint(Graphics graphics,
            int dataset, double x, double y,
            boolean connected) {
        if (_debug > 100) {
            System.out.println("Plot: addPoint " + dataset + " "+
                    x+" "+y+" "+connected);
        }
        if (dataset >= _numsets || dataset < 0) return;
        
        // For auto-ranging, keep track of min and max.
        if (x < _xBottom) _xBottom = x;
        if (x > _xTop) _xTop = x;
        if (y < _yBottom) _yBottom = y;
        if (y > _yTop) _yTop = y;

        // FIXME: Ignoring sweeps for now.
        PlotPoint pt = new PlotPoint();
        pt.x = x;
        pt.y = y;
        pt.connected = connected;
        Vector pts = _points[dataset];
        pts.addElement(pt);
        if (_pointsPersistence > 0) {
            if (pts.size() > _pointsPersistence) erasePoint(dataset,0);
        }
        _drawPlotPoint(graphics, dataset, pts.size() - 1);
    }

    /* Draw the specified point and associated lines, if any.
     */
    private synchronized void _drawPlotPoint(Graphics graphics,
            int dataset, int index) {
        if (_debug > 20)
            System.out.println("Plot: _drawPlotPoint("+dataset+" "+index+")");
        // Set the color
        if (_pointsPersistence > 0) {
            // To allow erasing to work by just redrawing the points.
            graphics.setXORMode(_background);
        }
        if (_graphics == null) {
            System.out.println("Plot::_drawPlotPoint(): Internal error: " +
                    "_graphic was null, be sure to call show()\n"+
                    "before calling init()");
        }

        if (_usecolor) {
            int color = dataset % _colors.length;
            graphics.setColor(_colors[color]);
        } else {
            graphics.setColor(_foreground);
        }

        Vector pts = _points[dataset];
        PlotPoint pt = (PlotPoint)pts.elementAt(index);
        // Use long here because these numbers can be quite large
        // (when we are zoomed out a lot).
        long ypos = _lry - (long) ((pt.y - _yMin) * _yscale);
        long xpos = _ulx + (long) ((pt.x - _xMin) * _xscale);

        // Draw the line to the previous point.
        if (pt.connected) _drawLine(graphics, dataset, xpos, ypos,
                _prevx[dataset], _prevy[dataset], true);

        // Save the current point as the "previous" point for future
        // line drawing.
        _prevx[dataset] = xpos;
        _prevy[dataset] = ypos;

        // Draw the point & associated decorations, if appropriate.
        if (_marks != 0) _drawPoint(graphics, dataset, xpos, ypos, true);
        if (_impulses) _drawImpulse(graphics, xpos, ypos, true);
        if (_bars) _drawBar(graphics, dataset, xpos, ypos, true);

        // Restore the color, in case the box gets redrawn.
        graphics.setColor(_foreground);
        if (_pointsPersistence > 0) {
            // Restore paint mode in case axes get redrawn.
            graphics.setPaintMode();
        }
    }
    
    /** 
     * Erase the point at the given index in the given dataset.  If
     * lines are being drawn, also erase the line to the next points
     * (note: not to the previous point).
     */
    private synchronized void _erasePoint(Graphics graphics,
            int dataset, int index) {
        // Set the color
        if (_pointsPersistence > 0) {
            // To allow erasing to work by just redrawing the points.
            graphics.setXORMode(_background);
        }
        if (_usecolor) {
            int color = dataset % _colors.length;
            graphics.setColor(_colors[color]);
        } else {
            graphics.setColor(_foreground);
        }

        Vector pts = _points[dataset];
        PlotPoint pt = (PlotPoint)pts.elementAt(index);
        long ypos = _lry - (long) ((pt.y - _yMin) * _yscale);
        long xpos = _ulx + (long) ((pt.x - _xMin) * _xscale);

        // Erase line to the next point, if appropriate.
        if (index < pts.size() - 1) {
            PlotPoint nextp = (PlotPoint)pts.elementAt(index+1);
            int nextx = _ulx + (int) ((nextp.x - _xMin) * _xscale);
            int nexty = _lry - (int) ((nextp.y - _yMin) * _yscale);
            // NOTE: I have no idea why I have to give this point backwards.
            if (nextp.connected) _drawLine(graphics, dataset,
                    nextx, nexty,  xpos, ypos, true);
            nextp.connected = false;
        }

        // Draw the point & associated lines, if appropriate.
        if (_marks != 0) _drawPoint(graphics, dataset, xpos, ypos, true);
        if (_impulses) _drawImpulse(graphics, xpos, ypos, true);
        if (_bars) _drawBar(graphics, dataset, xpos, ypos, true);

        // Restore the color, in case the box gets redrawn.
        graphics.setColor(_foreground);
        if (_pointsPersistence > 0) {
            // Restore paint mode in case axes get redrawn.
            graphics.setPaintMode();
        }

        pts.removeElementAt(index);
    }

    /* Split pxgraphargs up into an array and call _parseArgs
     */       
    public int parsePxgraphargs(String pxgraphargs) throws CmdLineArgException  {
        // We convert the String to a Stream and then use a StreamTokenizer
        // to parse the arguments into a Vector and then copy
        // the vector into an array of Strings.  We use a Vector
        // so that we can handle an arbitrary number of arguments
        if (_debug > 3) {
            System.out.println("Plot: parsePxgraphargs "+pxgraphargs);
        }

        Vector argvector = new Vector();
        boolean prependdash = false; // true if we need to add a -
        
        StringBufferInputStream inp = new StringBufferInputStream(pxgraphargs);
        // StringBufferInput is deprecated, but StringReader is not in 1.0.2

        //StringReader inp = new StringReader(pxgraphargs);

        try {
            StreamTokenizer stoken = new StreamTokenizer(inp); // Deprecated.

            // We don't want to parse numbers specially, so we reset
            // the syntax and then add back what we want.
            stoken.resetSyntax();
            stoken.whitespaceChars(0, ' ');
            stoken.wordChars('(','~');
            stoken.quoteChar('"');
            stoken.quoteChar('\'');
            int c;

        out:
            while (true) {
                c = stoken.nextToken();
                //System.out.print(c + " "+stoken.ttype+" "+stoken.sval+" "); 
                switch (stoken.ttype) {        // same as value of 'c'
                case StreamTokenizer.TT_EOF:
                    break out;
                case StreamTokenizer.TT_WORD:
                    //System.out.println("Word: " + stoken.sval);
                    if (prependdash) {
                        prependdash = false;
                        argvector.addElement(new String("-"+stoken.sval));
                    } else {
                        argvector.addElement(new String(stoken.sval));
                    }

                    break;
                case '-':
                    prependdash = true;
                    break;
                case '"':
                case '\'':
                    //System.out.println("String: " + stoken.sval);
                    argvector.addElement(new String(stoken.sval));
                    break;
                default:
                    throw new IOException("Failed to parse: '"+ (char)c +
                            "' in `" + pxgraphargs + "'");
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }


        // Create a array 
        String args[] = new String[argvector.size()];
        for(int i = 0; i<argvector.size(); i++) {
            args[i] = (String)argvector.elementAt(i);
            if (_debug > 2) System.out.print("<"+args[i]+ "> ");
        }
        if (_debug > 2) System.out.println(" ");

        return parseArgs(args);
    }

    //////////////////////////////////////////////////////////////////////////
    ////                       private variables                          ////
    
    private int _pointsPersistence = 0;
    private int _sweepsPersistence = 0;
    private boolean _bars = false;
    private double _barwidth = 0.5;
    private double _baroffset = 0.05;
    private boolean _connected = true;
    private boolean _impulses = false;
    private boolean _firstinset = true; // Is this the first datapoint in a set
    private int _filecount = 0;         // Number of files read in.
    // Have we seen a DataSet line in the current data file?
    private boolean _sawfirstdataset = false;
    
    // Give both radius and diameter of a point for efficiency.
    private int _radius = 3;
    private int _diameter = 6;
    
    private Vector _dataurls = null;

    // Information about the previously plotted point.
    private long _prevx[], _prevy[];

    // Maximum number of _datasets.
    private static final int _MAX_DATASETS = 63;

    // Maximum number of different marks
    // NOTE: There are 11 colors in the base class.  Combined with 10
    // marks, that makes 110 unique signal identities.
    private static final int _MAX_MARKS = 10;
}
