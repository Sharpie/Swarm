// JavaStartBox.java 

// JavaStartBox pops up a non-modal Dialog box with a message and a
// single button.  (There are defaults for the message and the button
// label appropriate for a "start" box) If provided, the message is a
// single string that may contain '\n' characters.  If it does, the
// message is broken into separate lines at each '\n'.  The Dialog box
// waits for the button to be pushed, at which point the window
// disappears and the window is disposed of.  JavaStartBox is NOT
// modal, i.e., it does allow other window I/O to occur while it is
// waiting.

import java.lang.Thread;
import java.awt.*;
import java.awt.event.*;

public class JavaStartBox extends Frame implements WindowListener
{
    Thread thisThread;
    Button button;

    public JavaStartBox(String title)
    {
	this(title, "Press the START button when ready.", "START");
    }

    public JavaStartBox(String title, String message, 
			  String buttonLabel)
    {
	// Create a parent Frame with no arguments.  (The Frame will
	// not display.) Then create a non-modal Dialog box based on
	// this Frame.
	super();
	Dialog d = new Dialog(this, title, false);

	// Register the WindowListener for the Dialog, specify that
	// the Dialog is not resizable, and set its color.
	d.addWindowListener(this);
	d.setResizable(false);
	d.setBackground(Color.lightGray);

	// Create and use a GridLayout manager with a variable number
	// of rows, one column, and 5 pixel spacing.
	d.setLayout(new GridLayout(0, 1, 5, 5));

	// Search for '\n' (end-of-line) characters in the message
	// string and break the string up into separate labels at each
	// one.  Add each label to Dialog as it is created.
	int posn = 0;
	int eol = -1;
	while ((eol = message.indexOf('\n', posn)) != -1)
	    {
	    d.add(new Label(message.substring(posn, eol), 
			       Label.CENTER));
	    posn = eol + 1;
	    }
	d.add(new Label(message.substring(posn), Label.CENTER));

	// Create a button with the buttonLabel passed to us.  Set the
	// ActionListener on the button.  Then add the button to the
	// Dialog.
	button = new Button(buttonLabel);
	button.addActionListener(new ButtonL());
	d.add(button);

	// Pack up the components and show the Dialog.
	d.pack();
	d.show();

	// Find the Thread in which we are running.  (Typically it
	// will be the "main" thread.)  Then call a method that will
	// wait until the button is pushed before JavaStartBox
	// returns.
	thisThread = Thread.currentThread();
	waitHere();
    }

    // We use this method to force the current Threat to wait until it
    // is released by the button press below.  The interrupted()
    // method clears any leftover interruptions.
    synchronized private void waitHere()
    {
	try
	    {
	    thisThread.interrupted();
	    this.wait();
	    } catch(InterruptedException e) {}
    }

    // These are the various methods that could conceivably be called
    // by the WindowListener.  The only event that we need to worry
    // about is when the user tries to close the window.  In this case
    // we handle things in the same way as we do when the button is
    // pressed (see below).
    public void windowClosing(WindowEvent e) 
	{
	    this.hide();
	    this.dispose();
	    thisThread.interrupt();
	}
    public void windowOpened(WindowEvent e) {}
    public void windowClosed(WindowEvent e) {}
    public void windowIconified(WindowEvent e) {}
    public void windowDeiconified(WindowEvent e) {}
    public void windowActivated(WindowEvent e) {}
    public void windowDeactivated(WindowEvent e) {}

    // This is the class we gave the ActionListener for the button.
    // Its single method is called when the button is pressed.  First
    // pop down and dispose of the window.  Since the ActionListener
    // runs this mehod in a separate Thread from the main Thread, we
    // can have this Thread send an interrupt to the main Thread we
    // left waiting above to wake it up from its wait.
    private class ButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    JavaStartBox.this.hide();
	    JavaStartBox.this.dispose();
	    thisThread.interrupt();
	}
    }
}
