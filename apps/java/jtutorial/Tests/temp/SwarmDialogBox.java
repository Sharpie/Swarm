import java.awt.*;
import java.awt.event.*;

public class SwarmDialogBox extends Dialog implements WindowListener
{
    public SwarmDialogBox(Frame parent, String title, String message)
    {
	this(parent, title, message, "OK");
    }

    public SwarmDialogBox(Frame parent, String title, String message, 
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

	//Create and use a BorderLayout manager with 15 pixel spacing.
	this.setLayout(new BorderLayout(15, 15));

	// Create a messagePanel to hold the message to the user.  The
	// messagePanel will have a GridLayout with one line for each
	// Label (line of the message).  We specify 0 rows and 1
	// column which will lead the layout manager to create a row
	// for each Label added.
	Panel messagePanel = new Panel();
	messagePanel.setLayout(new GridLayout(0, 1, 0, 0));

	// Search for '\n' (end-of-line) characters in the message
	// string and break the string up into separate labels at each
	// one.  Add each label to the messagePanel as it is created.
	int posn = 0;
	int eol = -1;
	while ((eol = message.indexOf('\n', posn)) != -1)
	    {
	    messagePanel.add(new Label(message.substring(posn, eol), 
				       Label.CENTER));
	    posn = eol + 1;
	    }
	messagePanel.add(new Label(message.substring(posn), Label.CENTER));

	// Add the messagePanel to the center of the Dialog.
	this.add("Center", messagePanel);

	// Create a button with the buttonLabel passed to us.  Set the
	// ActionListener on the button.  Then create a Panel to hold
	// the button, specify its layout, add the button to the
	// panel, and add the panel to the bottom of the Dialog.
	Button button = new Button(buttonLabel);
	button.addActionListener(new ButtonL());
	Panel p = new Panel();
	p.setLayout(new FlowLayout(FlowLayout.CENTER, 15, 5));
	p.add(button);
	this.add("South", p);

	// Pack up the components and show the Dialog.
	this.pack();
	this.show();
    }

    // These are the various methods that could conceivably be called
    // by the WindowListener.  The only event that we need to worry
    // about is when the user closes the window.
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
    // class we gave the ActionListener for the OK button.
    public class ButtonL implements ActionListener
    {
	public void actionPerformed(ActionEvent e)
	{
	    SwarmDialogBox.this.hide();
	    SwarmDialogBox.this.dispose();
	}
    }


    public static void main(String[] args)
    {
	Frame f = new Frame("DialogBox Test");
	f.setSize(100,100);
	f.show();

	SwarmDialogBox d = new SwarmDialogBox(f, "DialogBox", "Here is a very long\nmessage\nxxxxxxxxxxxxxx.");

	System.out.println("We're done.");
	f.dispose();
	System.exit(0);
    }

}
