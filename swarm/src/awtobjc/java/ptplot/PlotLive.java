/* A live signal plotter.

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

import java.awt.*;
import java.util.*;

//////////////////////////////////////////////////////////////////////////
//// PlotLive
/**
 * Plot signals dynamically, where points can be added at any time
 * and the the display will be updated.  This should be normally used
 * with some finite persistence so that old points are erased as new
 * points are added.  Unfortunately, the most efficient way to erase
 * old points is to draw graphics using the "exclusive or" mode, which
 * introduces quite a number of artifacts.  When lines are drawn
 * between points, where they overlap the points the line becomes
 * white. Moreover, if two lines or points overlap completely, they
 * disappear.
 * <p>
 * This class is abstract, so it must be used by creating a derived
 * class.  To use it, create a derived class with <code>init()</code>
 * and <code>addPoints()</code> methods. The <code>init()</code>
 * method can call methods in the <code>Plot</code> or
 * <code>PlotBox</code> classes (both of which are base classes) to
 * set the static properties of the graph, such as the title, axis
 * ranges, and axis labels.  The <code>addPoints()</code> method
 * should call <code>addPoint()</code> of the <code>Plot</code> base
 * class to dynamically add points to the plot.  This method is called
 * within a thread separate from the applet thread.
 * <p>
 * The <code>init()</code> method <i>must</i> call
 * <code>super.init()</code> somewhere in its body; along with general
 * initialization, this reads a file given by a URL if the dataurl
 * applet parameter is specified.  Thus, the initial configuration can
 * be specified in a separate file rather than in Java code.
 *
 * @author Edward A. Lee, Christopher Hylands
 * @version @(#)PlotLive.java	1.28 10/25/97
 */
public abstract class PlotLive extends Plot implements Runnable {

    //////////////////////////////////////////////////////////////////////////
    ////                         public methods                           ////
   
    /**
     * Handle button presses to enable or disable plotting.
     * @deprecated As of JDK1.1 in java.awt.component 
     * but we need to compile under 1.0.2 for netscape3.x compatibility.
     */
    public boolean action (Event evt, Object arg) {
        if (evt.target == _startButton) {
            _plotting = true;
            return true;
        } else if (evt.target == _stopButton) {
            _plotting = false;
            return true;
        } else {
            return super.action (evt, arg); // action() is deprecated in 1.1
            // but we need to compile under 
            // jdk1.0.2 for netscape3.x
        }
    }

    /**
     * Redefine in derived classes to add points to the plot.
     * Adding many points at once will make the plot somewhat faster
     * because the thread yields between calls to this method.
     * However, the plot will also be somewhat less responsive to user
     * inputs such as zooming, filling, or stopping.
     */
  public /* synchronized  */ abstract void addPoints();

    /**
     * Create a start and stop buttons, by which the user can invoke
     * <code>enable()</code> and <code>disable</code>.  Alternatively,
     * a derived class might invoke these directly and dispense with
     * the buttons.  This should be called within the
     * <code>init()</code> method in derived classes.
     */
    public void makeButtons () {
        if (_debug >8 ) System.out.println("PlotLive: makeButtons");
        // So that the buttons appear at the upper right...
        // Note that this infringes on the title space... maybe not good.
        _startButton = new Button("start");
        add(_startButton);
        _stopButton = new Button("stop");
        add(_stopButton);
    }
    
    /**
     * This is the body of a thread that monitors which of the start
     * or stop buttons have been pushed most recently, or which of the
     * <code>enable()</code> or <code>disable()</code> methods has
     * been called most recently, to determine whether to patiently
     * wait or to call the <code>addPoints()</code> method.  Between
     * calls to <code>addPoints()</code>, it calls
     * <code>Thread.yield()</code> so that the thread does not hog all
     * the resources.  This somewhat slows down execution, so derived
     * classes may wish to plot quite a few points in their
     * <code>addPoints()</code> method, if possible.  However,
     * plotting more points at once may also decrease the
     * responsiveness of the user interface.
     */
    public void run() {
        if (_debug >8 ) System.out.println("PlotLive: run");
        while (Thread.currentThread() == _plotLiveThread) {
            if (_plotting) {
                addPoints();
                Thread.yield();
            } else {
                try {
                    // NOTE: Using wait here with notifyAll in the action 
                    // method leads to inexplicable deadlocks.
                    // So we just sleep.
                    Thread.sleep(100);
                } catch (InterruptedException e) {}
            }
        }
    }

    /**
     * If plotting is set to true, then we plot.
     */  
    public void setPlotting( boolean plotting) {
        _plotting = plotting;
    }

    /** 
     * Start the widget. It creates a thread to plot live data, if
     * this has not been already done.  However, we don't actually
     * start plotting until either the start button is called, or
     * setPlotting(true) is called.
     */
    public void start() {
        if (_debug >8 ) System.out.println("PlotLive: start");
        if (_plotLiveThread == null) {
            _plotLiveThread = new Thread(this, "PlotLive Thread");
            _plotLiveThread.start();
        }
    }

    /**
     * Stop the widget.
     */
    public void stop() {
        if (_debug >8 ) System.out.println("PlotLive: stop");
        if (_plotLiveThread != null) {
            // Don't stop the thread, just set it to null. 
            //_plotLiveThread.stop();
            _plotLiveThread = null;
        }
    }


    //////////////////////////////////////////////////////////////////////////
    ////                       private variables                          ////
    
    private Thread _plotLiveThread;
   
    // True if we are actually plotting
    private boolean _plotting = false;

    private Button _startButton, _stopButton;
    
}
