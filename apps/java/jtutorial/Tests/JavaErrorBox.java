// JavaErrorBox.java 

// JavaErrorBox pops up a modal Dialog box with a message and a single
// button.  All processing stops until the button is pushed.  The
// message is a single string that may contain \n's.  If it does, the
// message is broken into separate lines at each \n.  The user
// provides a title for the Dialog box, the message, and optionally
// the label for the button.  (The label defaults to "OK".)

import java.awt.*;
import java.awt.event.*;

public class JavaErrorBox extends Dialog implements WindowListener
{
    public JavaErrorBox(Frame parent, String title, String message)
    {
	this(parent, title, message, "OK");
    }

    public JavaErrorBox(Frame parent, String title, String message, 
			  String buttonLabel)
    {
	// Create a modal Dialog with the specified title and
	// parent. The third argument specified whether the Dialog is
	// modal or not.  (With a modal Dialog, everything stops until
	// the Dialog is closed.)
	super(parent, title, true);

	// Register the WindowListener, specify that the Dialog is not
	// resizable, and set its color.
	this.addWindowListener(this);
	setResizable(false);
	setBackground(Color.lightGray);

	// Create and use a GridLayout manager with a variable number
	// of rows, one column, and 5 pixel spacing.
	this.setLayout(new GridLayout(0, 1, 5, 5));

	// Search for '\n' (end-of-line) characters in the message
	// string and break the string up into separate labels at each
	// one.  Add each label to Dialog as it is created.
	int posn = 0;
	int eol = -1;
	while ((eol = message.indexOf('\n', posn)) != -1)
	    {
	    this.add(new Label(message.substring(posn, eol), 
			       Label.CENTER));
	    posn = eol + 1;
	    }
	this.add(new Label(message.substring(posn), Label.CENTER));

	// Create a button with the buttonLabel passed to us.  Set the
	// ActionListener on the button.  Then add the button to the
	// Dialog.
	Button button = new Button(buttonLabel);
	button.addActionListener(new ButtonL());
	this.add(button);

	// Pack up the components and show the Dialog.
	this.pack();
	this.show();
    }

    // These are the various methods that could conceivably be called
    // by the WindowListener.  The only event that we need to worry
    // about is when the user closes the window.  The result is the
    // same as the user pressing the button.
    public void windowClosing(WindowEvent e) 
	{
	this.hide();
	this.dispose();
	}
    public void windowOpened(WindowEvent e) {}
    public void windowClosed(WindowEvent e) {}
    public void windowIconified(WindowEvent e) {}
    public void windowDeiconified(WindowEvent e) {}
    public void windowActivated(WindowEvent e) {}
    public void windowDeactivated(WindowEvent e) {}

    // Pop down the window when the button is clicked. This is the
    // class we gave the ActionListener for the button.
    private class ButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    JavaErrorBox.this.hide();
	    JavaErrorBox.this.dispose();
	}
    }
}
